(ql:quickload '(:str :cl-ppcre :binding-arrows :hu.dwim.defclass-star :alexandria :parseq :metabang-bind :fset :priority-queue))
(defpackage :advent (:use :cl :cl-ppcre :binding-arrows :parseq :metabang-bind :hu.dwim.defclass-star))
(in-package :advent)

(defclass* cube () (x y))
(defclass* rock () (x y))
(defun roll-vertical (rock cave direction max-y)
  (with-slots (x y) rock
    (setf y (-<>> cave
              (alexandria:hash-table-values)
              (remove-if-not (lambda (rock) (and (= (x-of rock) x)
                                                 (if (equal direction :up)
                                                     (< (y-of rock) y)
                                                     (> (y-of rock) y)))))
              (reduce #'max <> :key #'y-of :initial-value (if (equal direction :up) -1 max-y))
              (+ (if (equal direction :up) 1 -1))))))

(defun move-up (cave max-x max-y)
  (print-cave cave max-x max-y)
  (loop for y from 0 below max-y
        do (loop for x from 0 below max-x
                 for rock = (gethash (list y x) cave)
                 if (rockp rock) do (remhash (list y x) cave)
                                    (setf (gethash (list (roll-vertical rock cave :up max-y) x) cave) rock))))


(defun parse-cave (file)
  (loop with lines = (str:lines file)
        with out = (serapeum:dict)
        for y from 0 below (length lines)
        for line = (nth y lines)
        maximize (loop for x from 0 below (length line)
                       for c = (char line x)
                       if (char= c #\O)
                         do (setf (gethash (list y x) out) (make-instance 'rock :x x :y y))
                       else if (char= c #\#)
                              do (setf (gethash (list y x) out) (make-instance 'cube :x x :y y))
                       finally (return x)) into max-x
        finally (return (list out max-x y))))

;; Print the cave each cube is a #, each O is a rock, empty spaces are .
(defun print-cave (cave max-x max-y)
  (loop for y from 0 below max-y
        do (loop for x from 0 below max-x
                 for rock = (gethash (list y x) cave)
                 if (rockp rock) do (format t "O")
                 else if (cubep rock) do (format t "#")
                 else do (format t "."))
        (format t "~%")))

(defun part-1 (file)
  (let* ((parsed (parse-cave file))
         (cave (first parsed))
         (max-x (second parsed))
         (max-y (third parsed)))
    (move-up cave max-x max-y)
    ;;(print-cave cave max-x max-y)
    (reduce #'+ (remove-if-not #'rockp (alexandria:hash-table-values cave))
            :key (lambda (r) (1+ (- max-y (y-of r) 1 ))))))

;;(defparameter *test-cave* (first (parse-cave (frog:get-advent-of-code-input 2023 14 :input-suffix "test"))))
(format t "~a~%" (time (part-1 (frog:get-advent-of-code-input 2023 14 :input-suffix "test"))))
(format t "~a~%" (time (part-1 (frog:get-advent-of-code-input 2023 14))))
