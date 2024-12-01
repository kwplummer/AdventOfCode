(ql:quickload '(:str :cl-ppcre :binding-arrows :hu.dwim.defclass-star :alexandria :parseq :metabang-bind :fset :priority-queue :cl-tqdm :function-cache))
(defpackage :advent (:use :cl :cl-ppcre :binding-arrows :parseq :metabang-bind :hu.dwim.defclass-star))
(in-package :advent)

(defclass* module () (name (connections (list))))
(defmethod send ((module module) sender value modules send-queue) send-queue)

;; Print method
(defmethod print-object ((module module) stream)
  (with-slots (name connections) module
    (format stream "#<~a ~a ~a>" (class-name (class-of module)) name connections)))

(defclass* flip-flop (module) ((enabled nil))) ;;% Flips on low

(defmethod send ((module flip-flop) sender value modules send-queue)
  (with-slots (name connections enabled) module
    (when (equal value :low)
      (setf enabled (not enabled))
      ;;(format t "Flip-flop ~a is now ~a~%" name enabled)
      (loop for connection in connections
              do (setf send-queue (nconc send-queue (list (list name connection (if enabled :high :low)))))))
    send-queue))

(defclass* conjunction (module) ((most-recent (list)))) ;; & Remembers last per connection in alist

(defmethod send ((module conjunction) sender value modules send-queue)
    (with-slots (name connections most-recent) module
      (setf (cdr (assoc sender most-recent :test #'equal)) value)
      (if (every (lambda (x) (equal x :high)) (mapcar #'cdr most-recent))
          (progn
            ;; (format t "Conjunction ~a is now high~%" name)
            (loop for connection in connections
                  do (setf send-queue (nconc send-queue (list (list name connection :low))))
                  finally (return send-queue)))
          (progn
            ;; (format t "Conjunction ~a is still low~%" name)
            (loop for connection in connections
                  do (setf send-queue (nconc send-queue (list (list name connection :high))))
                  finally (return send-queue))))))

(defclass* broadcast (module) ()) ;; Broadcasts low same to all connections

(defmethod send ((module broadcast) sender value modules send-queue)
  (with-slots (name connections) module
    ;;(format t "Broadcast ~a is now ~a~%" name value)
    (loop for connection in connections
          do (setf send-queue (nconc send-queue (list (list name connection value))))
          finally (return send-queue))))

;; Pulses are queued, if a and b are called, and a notifies c, c will wait for b to finish

(defun parse (file)
  (loop with out = (serapeum:dict)
        for line in (str:lines file)
        for (source dest-csv) = (str:split " -> " line)
        for unprefixed = (str:substring 1 nil source)
        for dests = (str:split ", " dest-csv)
        if (string= "broadcaster" source) do (setf (gethash source out) (make-instance 'broadcast :name source :connections dests))
          if (str:starts-with-p "%" source) do (setf (gethash unprefixed out) (make-instance 'flip-flop :name unprefixed :connections dests))
            if (str:starts-with-p "&" source) do (setf (gethash unprefixed out) (make-instance 'conjunction :name unprefixed :connections dests))
              finally (loop for source being the hash-values of out
                            for source-name = (name-of source)
                            for connections = (connections-of source)
                            do (loop for conection in connections if (not (gethash conection out)) do (setf (gethash conection out) (make-instance 'module :name conection)))
                            do (loop for dest being the hash-values of out
                                     for dest-name = (name-of dest)
                                     if (and (member dest-name connections :test #'equal) (conjunctionp dest))
                                       do (setf (most-recent-of dest) (acons source-name :low (most-recent-of dest)))))
        finally (return out)))

(defun part-1 (file presses)
  (let* ((modules (parse file))
         (send-queue (list))
         (high 0) (low 0))
    (dotimes (i presses)
      (incf low)
      (setf send-queue (send (gethash "broadcaster" modules) nil :low modules send-queue))
      (loop while send-queue
            for (sender dest value) = (pop send-queue)
            do (if (equal :high value) (incf high) (incf low))
            ;; do (format t "Sending ~a from ~a to ~a~%" value sender dest)
            do (setf send-queue (send (gethash dest modules) sender value modules send-queue))))
    (format t "High: ~a, Low: ~a~%" high low)
    (* high low)))

(print (time (part-1 (frog:get-advent-of-code-input 2023 20 :input-suffix "test") 1000)))
(print (time (part-1 (frog:get-advent-of-code-input 2023 20 :input-suffix "test2") 1000)))
(print (time (part-1 (frog:get-advent-of-code-input 2023 20) 1000)))

(defun parse-2 (file)
  (loop with out = (serapeum:dict "rx" (make-instance 'module :name "rx"))
        for line in (str:lines file)
        for (source dest-csv) = (str:split " -> " line)
        for unprefixed = (str:substring 1 nil source)
        for dests = (str:split ", " dest-csv)
        if (string= "broadcaster" source) do (setf (gethash source out) (make-instance 'broadcast :name source :connections dests))
          if (str:starts-with-p "%" source) do (setf (gethash unprefixed out) (make-instance 'flip-flop :name unprefixed :connections dests))
            if (str:starts-with-p "&" source) do (setf (gethash unprefixed out) (make-instance 'conjunction :name unprefixed :connections dests))
              finally (loop for source being the hash-values of out
                            for source-name = (name-of source)
                            for connections = (connections-of source)
                            do (loop for conection in connections if (not (gethash conection out)) do (setf (gethash conection out) (make-instance 'module :name conection)))
                            do (loop for dest being the hash-values of out
                                     for dest-name = (name-of dest)
                                     if (and (member dest-name connections :test #'equal) (conjunctionp dest))
                                       do (setf (most-recent-of dest) (acons source-name :low (most-recent-of dest)))))
        finally (return out)))



(defun part-2 (file presses)
  (let* ((modules (parse file))
         (send-queue (list))
         (high 0) (low 0))
    (dotimes (i presses)
      (incf low)
      (setf send-queue (send (gethash "broadcaster" modules) nil :low modules send-queue))
      (loop while send-queue
            for (sender dest value) = (pop send-queue)
            do (if (equal :high value) (incf high) (incf low))
               (if (and (equal :low value) (equal dest "rx")) (return-from part-2 i))
            ;; do (format t "Sending ~a from ~a to ~a~%" value sender dest)
            do (setf send-queue (send (gethash dest modules) sender value modules send-queue))))))

(print (time (part-2 (frog:get-advent-of-code-input 2023 20) most-positive-fixnum)))
