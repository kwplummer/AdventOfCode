(ql:quickload '(:str :cl-ppcre :binding-arrows :hu.dwim.defclass-star :alexandria :parseq :metabang-bind :fset :priority-queue :cl-tqdm :function-cache))
(defpackage :advent (:use :cl :cl-ppcre :binding-arrows :parseq :metabang-bind :hu.dwim.defclass-star))
(in-package :advent)
;;(declaim (optimize (speed 3) (safety 0) (debug 0) (compilation-speed 0) (space 0)))

(defparameter *ticks* 0)
(defparameter *update-interval* 1000)
(defun mark-outside (y x min-y min-x max-y max-x dug visited to-mark)
  (let ((char (gethash (list y x) dug #\.)))
    (when (and (not (gethash (list y x) visited))
               (not (equal char #\#))
               (>= y min-y) (<= y max-y)
               (>= x min-x) (<= x max-x))
      (when (zerop (mod (incf *ticks*) *update-interval*)) (format t "Running. visited: ~a dug: ~a~%" (hash-table-count visited) (hash-table-count dug)))
      (setf (gethash (list y x) visited) t)
      (push (list (1- y) x) to-mark)
      (push (list (1+ y) x) to-mark)
      (push (list y (1- x)) to-mark)
      (push (list y (1+ x)) to-mark)))
  to-mark)

(defun print-grid-inline (min-y min-x max-y max-x visited)
  (format t "=================~%")
    (loop for y from min-y upto max-y
            do (loop for x from min-x upto max-x
                     do (write-char (if (gethash (list y x ) visited) #\. #\#))
                     finally (terpri)))
    (format t "=================~%"))

(defun get-area (dug)
  (let ((max-y (reduce #'max (alexandria:hash-table-keys dug) :key #'first))
        (min-y (reduce #'min (alexandria:hash-table-keys dug) :key #'first))
        (max-x (reduce #'max (alexandria:hash-table-keys dug) :key #'second))
        (min-x (reduce #'min (alexandria:hash-table-keys dug) :key #'second))
        (visited (serapeum:dict))
        (to-mark (list)))
        (loop for y from min-y upto max-y
              do (setf to-mark (mark-outside y min-x min-y min-x max-y max-x dug visited to-mark))
              do (setf to-mark (mark-outside y max-x min-y min-x max-y max-x dug visited to-mark)))
        (loop for x from min-x upto max-x
              do (setf to-mark (mark-outside min-y x min-y min-x max-y max-x dug visited to-mark))
              do (setf to-mark (mark-outside max-y x min-y min-x max-y max-x dug visited to-mark)))
    (loop with counter = 0
          with tqdm = (cl-tqdm:tqdm (length to-mark))
          while to-mark
          for (y x) = (pop to-mark)
          if (zerop (mod (incf counter) *update-interval*)) do (cl-tqdm:update tqdm :incf *update-interval* :total-count (+ counter (length to-mark)))
                                                               (setf to-mark (remove-duplicates to-mark :test #'equal))
          do (setf to-mark (mark-outside y x min-y min-x max-y max-x dug visited to-mark)))
    ;;(print-grid-inline min-y min-x max-y max-x visited)
    (- (* (1+ (- max-y min-y)) (1+ (- max-x min-x))) (length (alexandria:hash-table-keys visited)))))

(defun print-grid-outline (dug)
  (format t "=================~%")
  (let ((max-y (reduce #'max (alexandria:hash-table-keys dug) :key #'first))
        (min-y (reduce #'min (alexandria:hash-table-keys dug) :key #'first))
        (max-x (reduce #'max (alexandria:hash-table-keys dug) :key #'second))
        (min-x (reduce #'min (alexandria:hash-table-keys dug) :key #'second)))
    (loop for y from min-y upto max-y
          do (loop for x from min-x upto max-x
                   for node = (gethash (list y x) dug #\.)
                   do (write-char node))
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
                do (setf (gethash (list y x) dug) #\#))
        finally (print-grid-outline dug)
        finally (return (get-area dug))))

(print (time (dig (frog:get-advent-of-code-input 2023 18 :input-suffix "test"))))
;;(print (time (dig (frog:get-advent-of-code-input 2023 18))))

(defun decode-hex (line)
  (list (case (char line (- (length line) 2)) (#\0 #\R) (#\1 #\D) (#\2 #\L) (#\3 #\U))
        (parse-integer (str:substring 2 -2 (third (str:split " " line))) :radix 16)))


(defun dig-2 (file)
  (loop with commands = (str:lines file)
        with dug = (serapeum:dict)
        with y = 0 and x = 0
        for command in commands
        for (dir dist) = (decode-hex command)
        do (loop for i from 0 below dist
                if (char= dir #\U) do (decf y)
                if (char= dir #\D) do (incf y)
                if (char= dir #\L) do (decf x)
                if (char= dir #\R) do (incf x)
                do (setf (gethash (list y x) dug) #\#))
        finally (format t "Built grid~%")
        finally (return (get-area dug))))

;;(print (time (dig-2 (frog:get-advent-of-code-input 2023 18 :input-suffix "test"))))
