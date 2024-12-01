(ql:quickload '(:str :cl-ppcre :binding-arrows :hu.dwim.defclass-star :alexandria :parseq :metabang-bind :fset :priority-queue))
(defpackage :advent (:use :cl :cl-ppcre :binding-arrows :parseq :metabang-bind :hu.dwim.defclass-star))
(in-package :advent)
(declaim (optimize (speed 3) (safety 0) (debug 0) (compilation-speed 0)))

;; Note: Neighbors is a pair (node, direction)
(defclass* node () (char x y (in-flood nil) (valid t) (listed-neighbors (list)) (neighbors (list))))
(defmethod print-object ((node node) stream)
  (print-unreadable-object (node stream :type t :identity t)
    (format stream "~A ~A ~A ~A" (char-of node) (y-of node) (x-of node)
            (loop for neighbor in (neighbors-of node)
                  collect (list (second neighbor) (y-of (first neighbor)) (x-of (first neighbor)))))))

(defun accepts-left (char) (member char '(#\- #\J #\7 #\S)))
(defun accepts-right (char) (member char '(#\- #\F #\L #\S)))
(defun accepts-up (char) (member char '(#\| #\J #\L #\S)))
(defun accepts-down (char) (member char '(#\| #\F #\7 #\S)))

(defun determine-direction (src dest)
  (with-slots (x y) dest
    (let ((d-x (x-of src))
          (d-y (y-of src)))
      (cond ((and (= x d-x) (< y d-y)) :up)
            ((and (= x d-x) (> y d-y)) :down)
            ((and (< x d-x) (= y d-y)) :left)
            ((and (> x d-x) (= y d-y)) :right)))))

(defun make-network (file)
  (let ((network (serapeum:dict))
        (start nil)
        (lines (str:lines file)))
    (labels ((char-at (y x)
               (if (and (>= y 0) (< y (length lines))
                        (>= x 0) (< x (length (nth y lines))))
                   (char (nth y lines) x)
                   #\space)))

      (loop for y from 0 below (length lines)
            for line = (nth y lines)
            do (loop for x from 0 below (length line)
                     for node = (make-instance 'node :x x :y y :char (char line x))
                     for char = (char line x)
                     do (case char
                          ((#\|) (setf (gethash (list y x) network) node)
                           (if (accepts-down (char-at (- y 1) x))
                               (push (list (- y 1) x) (slot-value node 'listed-neighbors)))
                           (if (accepts-up (char-at (+ y 1) x))
                               (push (list (+ y 1) x) (slot-value node 'listed-neighbors))))
                          (push (list (+ y 1) x) (slot-value node 'listed-neighbors))
                          ((#\-) (setf (gethash (list y x) network) node)
                           (if (accepts-right (char-at y (- x 1)))
                               (push (list y (- x 1)) (slot-value node 'listed-neighbors)))
                           (if (accepts-left (char-at y (+ x 1)))
                               (push (list y (+ x 1)) (slot-value node 'listed-neighbors))))
                          ((#\L) (setf (gethash (list y x) network) node)
                           (if (accepts-down (char-at (- y 1) x))
                               (push (list (- y 1) x) (slot-value node 'listed-neighbors)))
                           (if (accepts-left (char-at y (+ x 1)))
                               (push (list y (+ x 1)) (slot-value node 'listed-neighbors))))
                          ((#\J) (setf (gethash (list y x) network) node)
                           (if (accepts-down (char-at (- y 1) x))
                               (push (list (- y 1) x) (slot-value node 'listed-neighbors)))
                           (if (accepts-right (char-at y (- x 1)))
                               (push (list y (- x 1)) (slot-value node 'listed-neighbors))))
                          ((#\7) (setf (gethash (list y x) network) node)
                           (if (accepts-up (char-at (+ y 1) x))
                               (push (list (+ y 1) x) (slot-value node 'listed-neighbors)))
                           (if (accepts-right (char-at y (- x 1)))
                               (push (list y (- x 1)) (slot-value node 'listed-neighbors))))
                          ((#\F) (setf (gethash (list y x) network) node)
                           (if (accepts-up (char-at (+ y 1) x))
                               (push (list (+ y 1) x) (slot-value node 'listed-neighbors)))
                           (if (accepts-left (char-at y (+ x 1)))
                               (push (list y (+ x 1)) (slot-value node 'listed-neighbors))))
                          ((#\.) (setf (gethash (list y x) network) node))
                          ((#\S) (setf (gethash (list y x) network) node)
                           (setf start node)))))
      ;; Expand all neighbors
      (loop for node being the hash-values of network
            do (loop for neighbor-coords in (slot-value node 'listed-neighbors)
                     for neighbor = (gethash neighbor-coords network)
                     when neighbor do (progn (push (list neighbor (determine-direction node neighbor))  (slot-value node 'neighbors))
                                             (push (list node (determine-direction neighbor node)) (slot-value neighbor 'neighbors))))
            finally (loop for node being the hash-values of network
                          do (setf (slot-value node 'neighbors) (remove-duplicates (slot-value node 'neighbors) :test #'equal))))
      (values start network))))

(defun get-loop (start)
  (let* ((min-distance (serapeum:dict))
         (best-distance most-positive-fixnum)
         (queue (priority-queue:make-pqueue #'<)))
    (setf (gethash start min-distance) 0)
    (priority-queue:pqueue-push (list 0 start) 0 queue)
    (loop until (priority-queue:pqueue-empty-p queue)
          for next = (priority-queue:pqueue-pop queue)
          for dist = (first next)
          for node = (second next)
          for neighbors = (mapcar #'first (neighbors-of node))
          do (loop with new-distance = (1+ dist)
                   for neighbor in neighbors
                   for old-distance = (gethash neighbor min-distance best-distance)
                   if (< new-distance old-distance)
                     do (setf (gethash neighbor min-distance) new-distance)
                        (priority-queue:pqueue-push (list new-distance neighbor) new-distance queue))
          finally (return min-distance))))

(defun part-1 (file)
  (let* ((start (make-network file))
         (pipe-loop (get-loop start)))
    (reduce #'max (alexandria:hash-table-values pipe-loop))))

;; (print (make-network (frog:get-advent-of-code-input 2023 10 :input-suffix "test2")))
(print (time (part-1 (frog:get-advent-of-code-input 2023 10))))

(defun sneak-vertical (node directions)
  (if node
      (remove-if
       (lambda (d) (member d (mapcar #'second
                                     (remove-if
                                      (lambda (neighbor) (= (x-of (first neighbor)) (x-of node)))
                                      (neighbors-of node)))))
       directions)
      directions))

(defun sneak-horizontal (node directions)
  (if node
      (remove-if
       (lambda (d) (member d (mapcar #'second
                                     (remove-if
                                      (lambda (neighbor) (= (y-of (first neighbor)) (y-of node)))
                                      (neighbors-of node)))))
       directions)
      directions))

(defun invalidate-node (node loops network visited)
  (when (and node (not (gethash (list (y-of node) (x-of node)) loops)) (not (gethash node visited)))
    (with-slots (y x valid) node
      (when valid (format t "Invalidating ~a~%" node))
      (setf valid nil)
      (setf (gethash node visited) t)
      (invalidate-node (gethash (list y (- x 1)) network) loops network visited)
      (invalidate-node (gethash (list y (+ x 1)) network) loops network visited)
      (invalidate-node (gethash (list (- y 1) x) network) loops network visited)
      (invalidate-node (gethash (list (+ y 1) x) network) loops network visited))))

;; (defun part-2 (file)
;;   (multiple-value-bind (start network) (make-network file)
;;     (let* ((loop-nodes (alexandria:hash-table-keys (get-loop start)))
;;            (loops (serapeum:dict))
;;            (max-x (reduce #'max (mapcar #'x-of (alexandria:hash-table-values network))))
;;            (max-y (reduce #'max (mapcar #'y-of (alexandria:hash-table-values network)))))
;;       (loop for node in loop-nodes do (setf (valid-of node) nil (gethash (list (y-of node) (x-of node)) loops) node))
;;       (loop for x from 0 upto max-x
;;             do (loop with directions = (list :left :right)
;;                      while directions
;;                      for y from 0 upto max-y
;;                      for node = (gethash (list y x) network)
;;                      do (setf directions (sneak-vertical (gethash (list y x) loops) directions))
;;                      if (and node directions) do
;;                                                  (invalidate-node node loops network (serapeum:dict))))
;;       (loop for x from 0 upto max-x
;;             do (loop with directions = (list :left :right)
;;                      while directions
;;                      for y from max-y downto 0
;;                      for node = (gethash (list y x) network)
;;                      do (setf directions (sneak-vertical (gethash (list y x) loops) directions))
;;                      if (and node directions) do (invalidate-node node loops network (serapeum:dict))))
;;       (loop for y from 0 upto max-y
;;             do (loop with directions = (list :up :down)
;;                      while directions
;;                      for x from 0 upto max-x
;;                      for node = (gethash (list y x) network)
;;                      do (setf directions (sneak-horizontal (gethash (list y x) loops) directions))
;;                      if (and node directions) do (invalidate-node node loops network (serapeum:dict))))
;;       (loop for y from 0 upto max-y
;;             do (loop with directions = (list :up :down)
;;                      while directions
;;                      for x from max-x downto 0
;;                      for node = (gethash (list y x) network)
;;                      do (setf directions (sneak-horizontal (gethash (list y x) loops) directions))
;;                      if (and node directions) do (invalidate-node node loops network (serapeum:dict))))
;;       (format t "Valid nodes: ~A~%" (remove-if-not #'valid-of (alexandria:hash-table-values network)))
;;       (count-if #'valid-of (alexandria:hash-table-values network)))))

 (defun part-2 (file)
   (multiple-value-bind (start network) (make-network file)
     (let* ((loop-nodes (alexandria:hash-table-keys (get-loop start)))
            (loops (serapeum:dict))
            (max-x (reduce #'max (mapcar #'x-of (alexandria:hash-table-values network))))
            (max-y (reduce #'max (mapcar #'y-of (alexandria:hash-table-values network)))))
       (loop for node in loop-nodes do (setf (valid-of node) nil (gethash (list (y-of node) (x-of node)) loops) node))
       (loop for location being the hash-keys of network using (hash-value node)
             for x = (second location) and y = (first location)
             when (not (member node loop-nodes))
               do (loop for y-beam from y downto 0
                        with directions = (list :left :right)
                        do (setf directions (sneak-vertical (gethash (list y-beam x) loops) directions))
                        while directions
                        when (= y-beam 0) do (invalidate-node node loops network (serapeum:dict)))
                  (loop for y-beam from y upto max-y
                        with directions = (list :left :right)
                        do (setf directions (sneak-vertical (gethash (list y-beam x) loops) directions))
                        while directions
                        when (= y-beam max-y) do (invalidate-node node loops network (serapeum:dict)))
                  (loop for x-beam from x downto 0
                        with directions = (list :up :down)
                        do (setf directions (sneak-horizontal (gethash (list y x-beam) loops) directions))
                        while directions
                        when (= x-beam 0) do (invalidate-node node loops network (serapeum:dict)))
                  (loop for x-beam from x upto max-x
                        with directions = (list :up :down)
                        do (setf directions (sneak-horizontal (gethash (list y x-beam) loops) directions))
                        while directions
                        when (= x-beam max-x) do (invalidate-node node loops network (serapeum:dict)))
             finally (format t "Valid nodes: ~A~%" (remove-if-not #'valid-of (alexandria:hash-table-values network)))
                     (return (count-if #'valid-of (alexandria:hash-table-values network)))))))


(print (part-2 (frog:get-advent-of-code-input 2023 10 :input-suffix "test3"))) ;; 4
(print (part-2 (frog:get-advent-of-code-input 2023 10 :input-suffix "test3.5"))) ;; 4
(print (part-2 (frog:get-advent-of-code-input 2023 10 :input-suffix "test4"))) ;; 8
(print (part-2 (frog:get-advent-of-code-input 2023 10 :input-suffix "test5"))) ;; 10

;; 529 too high
;; 528 too high
(print (part-2 (frog:get-advent-of-code-input 2023 10)))
