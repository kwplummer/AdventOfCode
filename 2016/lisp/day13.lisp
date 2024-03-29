(ql:quickload '(:str :cl-ppcre :binding-arrows :hu.dwim.defclass-star :alexandria :parseq :metabang-bind :fset :priority-queue))
(defpackage :advent (:use :cl :cl-ppcre :binding-arrows :parseq :metabang-bind :hu.dwim.defclass-star))
(in-package :advent)

(defparameter *favorite-number* 1352)
(defparameter *end* (list 31 39))

(defun is-wall (position)
  (let ((x (first position)) (y (second position)))
    (-<> (+ (* x x) (* 3 x) (* 2 x y) y (* y y) *favorite-number*)
      (write-to-string :base 2)
      (str:count-substring "1" <>)
      (mod 2)
      (= 1))))

(defun valid-moves (x y)
  (let (out)
    (when (> x 0) (push (list (1- x) y) out))
    (when (> y 0) (push (list x (1- y)) out))
    (push (list (1+ x) y) out)
    (push (list x (1+ y)) out)
    (remove-if #'is-wall out)))

(defun pathfind () ;; This is... just day 11?
  (let* ((min-distance (make-hash-table :test #'equal))
         (best-distance most-positive-fixnum)
         (queue (priority-queue:make-pqueue #'<)))
    (setf (gethash (list 1 1) min-distance) 0)
    (priority-queue:pqueue-push (list 0 (list 1 1)) 0 queue)
    (loop with counter = 0
          until (priority-queue:pqueue-empty-p queue)
          for next = (priority-queue:pqueue-pop queue)
          for dist = (first next)
          for location = (second next)
          for loc-x = (first location)
          for loc-y = (second location)
          do (loop with new-distance = (1+ dist)
                   until (< best-distance new-distance)
                   for neighbor in (valid-moves loc-x loc-y)
                   for old-distance = (gethash neighbor min-distance best-distance)
                   if (< new-distance old-distance)
                     do (setf (gethash neighbor min-distance) new-distance)
                        (priority-queue:pqueue-push (list new-distance neighbor) new-distance queue)
                        (when (equal *end* neighbor) (setf best-distance (min best-distance new-distance))
                              (format t "Found End. New Best: ~:d~%" best-distance)))
          finally (return best-distance))))

(print (time (pathfind)))

(setf *end* (list 9001 9001))
(defun unique-in-range () ;; Tweak of above, fake 50.1 as the best distance. Get size of min-distance graph.
  (let* ((min-distance (make-hash-table :test #'equal))
         (best-distance 50.1)
         (queue (priority-queue:make-pqueue #'<)))
    (setf (gethash (list 1 1) min-distance) 0)
    (priority-queue:pqueue-push (list 0 (list 1 1)) 0 queue)
    (loop with counter = 0
          until (priority-queue:pqueue-empty-p queue)
          for next = (priority-queue:pqueue-pop queue)
          for dist = (first next)
          for location = (second next)
          for loc-x = (first location)
          for loc-y = (second location)
          do (loop with new-distance = (1+ dist)
                   until (< best-distance new-distance)
                   for neighbor in (valid-moves loc-x loc-y)
                   for old-distance = (gethash neighbor min-distance best-distance)
                   if (< new-distance old-distance)
                     do (setf (gethash neighbor min-distance) new-distance)
                        (priority-queue:pqueue-push (list new-distance neighbor) new-distance queue)
                        (when (equal *end* neighbor) (setf best-distance (min best-distance new-distance))
                              (format t "Found End. New Best: ~:d~%" best-distance)))
          finally (return (hash-table-count min-distance)))))

(print (time (unique-in-range)))
