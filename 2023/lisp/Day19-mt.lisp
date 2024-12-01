(ql:quickload '(:str :cl-ppcre :binding-arrows :hu.dwim.defclass-star :alexandria :parseq :metabang-bind :fset :priority-queue :cl-tqdm :function-cache :lparallel))
(defpackage :advent (:use :cl :cl-ppcre :binding-arrows :parseq :metabang-bind :hu.dwim.defclass-star))
(in-package :advent)
(declaim (optimize (speed 3) (safety 0) (debug 0) (compilation-speed 0) (space 0)))
(setf lparallel:*kernel* (lparallel:make-kernel 16))
(defclass* part () (x m a s))
(defclass* rule () (name rules)) ; Rule is a list of pairs, first is the predicate lambda, second is the destination rule

(defun part-value (part)
  (with-slots (x m a s) part
    (+ x m a s)))

(defun run-workflow (rule part rules-by-name)
  (loop for (predicate destination) in (rules-of rule)
        if (funcall predicate part)
          do (cond ((string= destination "R") (return nil))
                   ((string= destination "A") (return t))
                   (t (return (run-workflow (gethash destination rules-by-name) part rules-by-name))))
        finally (return nil)))


(defun exec-rules (rules part)
  (let ((rules-by-name (serapeum:dict)))
    (loop for rule in rules do (setf (gethash (name-of rule) rules-by-name) rule))
    (run-workflow (gethash "in" rules-by-name) part rules-by-name)))


;; Slot-name is a character.
(defun get-slot-by-name (slot-name)
  (case slot-name
    (#\x 'x)
    (#\m 'm)
    (#\a 'a)
    (#\s 's)))

(defun get-comparison (comparison)
  (case comparison
    (#\= '=)
    (#\> '>)
    (#\< '<)))

(defun parse-rule (line)
  (let* ((parts (str:split "{" (str:substring 0 -1 line)))
         (name (first parts))
         (rule-text (str:split "," (second parts)))
         (rules (mapcar (lambda (r) (str:split ":" r)) rule-text)))
    (loop with parsed-rules = (list)
          for rule in rules
          if (= 1 (length rule))
            do (if (not (string= "R" (first rule))) (push (list (lambda (x) t) (first rule)) parsed-rules))
          else do (register-groups-bind (slot comparison value) ("(\\w+)([=<>])(\\w+)" (first rule))
                    (push (list (lambda (x) (funcall (get-comparison (char comparison 0))
                                                     (slot-value x (get-slot-by-name (char slot 0)))
                                                     (parse-integer value)))
                                (second rule)) parsed-rules))
            finally (return (make-instance 'rule :name name :rules (nreverse parsed-rules))))))

(defun parse-part (line)
    (register-groups-bind (x m a s) ("\\{x=(\\d+),m=(\\d+),a=(\\d+),s=(\\d+)\\}" line)
      (make-instance 'part
                     :x (parse-integer x)
                     :m (parse-integer m)
                     :a (parse-integer a)
                     :s (parse-integer s))))

(defun part-1 (file)
  (let* ((instructions (str:split frog:+double-newline+ file))
         (rules (mapcar #'parse-rule (str:lines (first instructions))))
         (parts (mapcar #'parse-part (str:lines (second instructions)))))
    (loop for part in parts
          for i from 0
          do (format t "Starting part ~a~%" i)
          if (exec-rules rules part)
            summing (part-value part))))

(print (time (part-1 (frog:get-advent-of-code-input 2023 19))))

(defun part-2 (file)
  (let* ((instructions (str:split frog:+double-newline+ file))
         (rules (mapcar #'parse-rule (str:lines (first instructions)))))
    (cl-tqdm:with-tqdm progress (* 4000 4000 4000 4000) ""
      (reduce #'+
              (lparallel:pmapcar
               (lambda (x)
                 (loop for m from 1 upto 4000
                       do (cl-tqdm:update-locked progress :incf 16000000)
                       summing (loop for a from 1 upto 4000
                                     summing (loop for s from 1 upto 4000
                                                   if (exec-rules rules (make-instance 'part :x x :m m :a a :s s))
                                                     sum 1))))
               (loop for x from 1 upto 4000 collect x))))))

(print (time (part-2 (frog:get-advent-of-code-input 2023 19 :input-suffix "test"))))

;; (loop for x from 1 upto 4000
;;          summing (loop for m from 1 upto 4000
;;                        do (cl-tqdm:update progress :incf 16000000)
;;                   summing (loop for a from 1 upto 4000
;;                            summing (loop for s from 1 upto 4000
;;                                          if (exec-rules rules (make-instance 'part :x x :m m :a a :s s))
;;                                            sum 1))))
