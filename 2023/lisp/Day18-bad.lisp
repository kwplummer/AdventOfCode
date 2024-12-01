(ql:quickload '(:str :cl-ppcre :binding-arrows :hu.dwim.defclass-star :alexandria :parseq :metabang-bind :fset :priority-queue :cl-tqdm :function-cache))
(defpackage :advent (:use :cl :cl-ppcre :binding-arrows :parseq :metabang-bind :hu.dwim.defclass-star))
(in-package :advent)

(defclass* node () (y x char (valid t)))
(defun invalidate-node (node network visited)
  (when (and node (not (equal #\# (char-of node))) (not (gethash node visited)))
    (with-slots (y x valid) node
      (setf valid nil)
      (setf (gethash node visited) t)
      (invalidate-node (gethash (list y (1- x)) network) network visited)
      (invalidate-node (gethash (list y (1+ x)) network) network visited)
      (invalidate-node (gethash (list (1- y) x) network) network visited)
      (invalidate-node (gethash (list (1+ y) x) network) network visited))))

(defmacro flood-direction ((outer end-outer) (inner start-inner dir-inner end-inner))
  `(loop for ,outer from 0 upto ,end-outer
         do (loop for ,inner from ,start-inner ,dir-inner ,end-inner
                  for node = (gethash (list y x) dug)
                  if (and node (equal #\# (char-of node))) do (return)
                  else do (invalidate-node node dug visited))))

(defun print-grid-inline (dug)
  (let ((max-y (reduce #'max (alexandria:hash-table-keys dug) :key #'first))
        (min-y (reduce #'min (alexandria:hash-table-keys dug) :key #'first))
        (max-x (reduce #'max (alexandria:hash-table-keys dug) :key #'second))
        (min-x (reduce #'min (alexandria:hash-table-keys dug) :key #'second)))
    (loop for y from min-y upto max-y
          do (loop for x from min-x upto max-x
                   for node = (gethash (list y x) dug (make-instance 'node :y y :x x :char #\. :valid nil))
                   do (write-char (if (valid-of node) #\# #\.)))
            do (terpri))))

(defun get-area (dug)
  (let ((max-y (reduce #'max (alexandria:hash-table-keys dug) :key #'first))
        (min-y (reduce #'min (alexandria:hash-table-keys dug) :key #'first))
        (max-x (reduce #'max (alexandria:hash-table-keys dug) :key #'second))
        (min-x (reduce #'min (alexandria:hash-table-keys dug) :key #'second))
        (visited (serapeum:dict)))
    (loop for y from min-y upto max-y
          do (loop for x from min-x upto max-x
                   if (not (gethash (list y x) dug))
                     do (setf (gethash (list y x) dug) (make-instance 'node :y y :x x :char #\.))))
    (flood-direction (x max-x) (y 0 upto max-y))
    (flood-direction (x max-x) (y max-y downto 0))
    (flood-direction (y max-y) (x 0 upto max-x))
    (flood-direction (y max-y) (x max-x downto 0))
    (print-grid-inline dug)
    (length (remove-if-not #'valid-of (alexandria:hash-table-values dug)))))

(defun print-grid-outline (dug)
  (let ((max-y (reduce #'max (alexandria:hash-table-keys dug) :key #'first))
        (min-y (reduce #'min (alexandria:hash-table-keys dug) :key #'first))
        (max-x (reduce #'max (alexandria:hash-table-keys dug) :key #'second))
        (min-x (reduce #'min (alexandria:hash-table-keys dug) :key #'second)))
    (loop for y from min-y upto max-y
          do (loop for x from min-x upto max-x
                   for node = (gethash (list y x) dug (make-instance 'node :y y :x x :char #\.))
                   do (write-char (char-of node)))
          do (terpri))
    (format t "=================~%")))

(defun dig (file)
  (loop with commands = (str:lines file)
        with dug = (serapeum:dict)
        with y = 0 and x = 0
        for command in commands
        for (dir dist) = (str:split " " command)
        do (loop for i from 0 below (parse-integer dist)
                if (equal dir "U") do (decf y)
                if (equal dir "D") do (incf y)
                if (equal dir "L") do (decf x)
                if (equal dir "R") do (incf x)
                do (setf (gethash (list y x) dug) (make-instance 'node :y y :x x :char #\#)))
        finally (print-grid-outline dug)
        finally (return (get-area dug))))


;; Not 51120
;; Not 26235
(print (time (dig (frog:get-advent-of-code-input 2023 18 :input-suffix "test"))))
(print (time (dig (frog:get-advent-of-code-input 2023 18))))
