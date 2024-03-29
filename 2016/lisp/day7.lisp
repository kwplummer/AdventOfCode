(ql:quickload '(:str :cl-ppcre :binding-arrows :snakes :alexandria :parseq :metabang-bind))
(defpackage :advent (:use :cl :cl-ppcre :binding-arrows :parseq :metabang-bind))
(in-package :advent)

(parseq:defrule ip-rule ()
    (* (or (+ alpha)
           (and "[" (+ alpha) "]")))
  (:function (lambda (&rest args)
               (list (mapcar (lambda (list) (coerce list 'string))
                             (remove-if (lambda (segment) (equal "[" (first segment))) args))
                     (mapcar (lambda (list) (coerce (second list) 'string))
                             (remove-if (lambda (segment) (not (equal "[" (first segment)))) args))))))

(defun part-1 (line)
  (labels ((has-abba (string)
             (loop for i below (- (length string) 3)
                   for j = (+ i 3)
                   if (and (equal (char string i) (char string j))
                           (equal (char string (1+ i)) (char string (1- j)))
                           (not (equal (char string i) (char string (1+ i)))))
                     do (return (str:substring i (1+ j) string)))))
    (bind (((outer inner) (parseq:parseq 'ip-rule line)))
      (unless (find-if #'has-abba inner) (find-if #'has-abba outer)))))

(defun part-2 (line)
  (bind (((outer inner) (parseq:parseq 'ip-rule line)))
    (find-if (lambda (substring) (find-if (lambda (chunk) (str:containsp substring chunk)) inner))
             (loop with search = nil
                   for string in outer
                   do (loop for i below (- (length string) 2)
                            for j = (+ i 2)
                            for first = (char string i)
                            for second = (char string (1+ i))
                            for third = (char string j)
                            if (and (equal first third) (not (equal first second)))
                              do (push (coerce (list second first second) 'string) search))
                   finally (return search)))))

(print (time (count-if #'part-1 (str:lines (str:from-file "../input/day7.txt")))))
(print (time (count-if #'part-2 (str:lines (str:from-file "../input/day7.txt")))))
