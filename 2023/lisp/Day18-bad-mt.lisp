(ql:quickload '(:str :cl-ppcre :binding-arrows :hu.dwim.defclass-star :alexandria :parseq :metabang-bind :fset :priority-queue :cl-tqdm :function-cache :lparallel))
(defpackage :advent (:use :cl :cl-ppcre :binding-arrows :parseq :metabang-bind :hu.dwim.defclass-star))
(in-package :advent)
(declaim (optimize (speed 3) (safety 0) (debug 0) (compilation-speed 0) (space 0)))
(setf lparallel:*kernel* (lparallel:make-kernel 16))

(defun print-grid-inline (min-y min-x max-y max-x temp)
  (format nil "=================~%")
    (loop for y from min-y upto max-y
            do (loop for x from min-x upto max-x
                     do (write-char (if (gethash (list y x) temp) #\# #\.))
                     finally (terpri)))
    (format nil "=================~%"))

(defun get-area (dug sorted-ranges)
  (let ((max-y (reduce #'max (alexandria:hash-table-keys dug) :key #'first))
        (min-y (reduce #'min (alexandria:hash-table-keys dug) :key #'first))
        (max-x (reduce #'max (alexandria:hash-table-keys dug) :key #'second))
        (min-x (reduce #'min (alexandria:hash-table-keys dug) :key #'second)))
    (cl-tqdm:with-tqdm progress (- max-x min-x) "Inspecting holes"
    (loop with holes = 0
          for x from (1+ min-x) below max-x
          do (cl-tqdm:update progress)
          do (loop with inside = t
                   with ranges = (gethash x sorted-ranges)
                   with temp-holes = 0
                   for y from 0 below (1- (length ranges))
                   for current-range = (nth y ranges)
                   for next-range = (nth (1+ y) ranges)
                   for delta = (abs (- next-range current-range))
                   if (= 1 delta)
                     do (incf holes delta) ;inside wall
                        (incf temp-holes delta)
                        (format nil "1 ~a: From ~a to ~a, inside=~a delta=~a added=1~%" x current-range next-range inside delta)
                   else if inside
                          do (incf holes delta)
                             (incf temp-holes delta)
                             (format nil "2 ~a: From ~a to ~a, inside=~a delta=~a added=~a~%" x current-range next-range inside delta delta)
                             (setf inside (char= (gethash (list (1+ next-range) x) dug #\.) #\#))
                   else if (not inside)
                         do (incf temp-holes)
                            (incf holes)
                            (format nil "3 ~a: From ~a to ~a, inside=~a delta=~a added=1~%" x current-range next-range inside delta)
                            (setf inside (char= (gethash (list (1+ next-range) x) dug #\.) #\.))
                   finally (incf holes)
                           (incf temp-holes)
                           (format nil "Finished column ~a, holes ~a~%" x temp-holes))
          ;; finally (print-grid-inline min-y min-x max-y max-x temp)
          finally (return (+ (length (gethash min-x sorted-ranges))
                             (length (gethash max-x sorted-ranges))
                           holes))))))

(defun get-sorted-ranges (dug)
  (let ((y-ranges (serapeum:dict))
        (sorted (sort (sort (alexandria:hash-table-keys dug) #'< :key #'first) #'< :key #'second)))
    (cl-tqdm:with-tqdm progress (length sorted) "Sorting ranges"
      (mapc (lambda (range)
              (cl-tqdm:update progress)
              (let ((ranges (gethash (second range) y-ranges (list))))
                (setf (gethash (second range) y-ranges) (nconc ranges (list (first range))))))
              sorted))
    y-ranges))

(defun print-grid-outline (dug)
  (format nil "=================~%")
  (let ((max-y (reduce #'max (alexandria:hash-table-keys dug) :key #'first))
        (min-y (reduce #'min (alexandria:hash-table-keys dug) :key #'first))
        (max-x (reduce #'max (alexandria:hash-table-keys dug) :key #'second))
        (min-x (reduce #'min (alexandria:hash-table-keys dug) :key #'second)))
    (loop for y from min-y upto max-y
          do (loop for x from min-x upto max-x
                   for node = (gethash (list y x) dug #\.)
                   do (write-char node))
          do (terpri))
    (format nil "=================~%")))

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
        finally (format nil "=================~%~a~%=================~%" (get-sorted-ranges dug))
        finally (return (get-area dug (get-sorted-ranges dug)))))

(print (time (dig (frog:get-advent-of-code-input 2023 18 :input-suffix "test"))))
;;(print (time (dig (frog:get-advent-of-code-input 2023 18))))

(defun decode-hex (line)
  (list (case (char line (- (length line) 2)) (#\0 #\R) (#\1 #\D) (#\2 #\L) (#\3 #\U))
        (parse-integer (str:substring 2 -2 (third (str:split " " line))) :radix 16)))

(defun get-area-mt (dug sorted-ranges)
  (let ((max-y (reduce #'max (alexandria:hash-table-keys dug) :key #'first))
        (min-y (reduce #'min (alexandria:hash-table-keys dug) :key #'first))
        (max-x (reduce #'max (alexandria:hash-table-keys dug) :key #'second))
        (min-x (reduce #'min (alexandria:hash-table-keys dug) :key #'second)))
    (cl-tqdm:with-tqdm progress (- max-x min-x) "Inspecting holes"
      (reduce #'+ (lparallel:pmapcar (lambda (x)
                                       (cl-tqdm:update-locked progress)
                                       (loop with inside = t
                                             with ranges = (gethash x sorted-ranges)
                                             with holes = 0
                                             for y from 0 below (1- (length ranges))
                                             for current-range = (nth y ranges)
                                             for next-range = (nth (1+ y) ranges)
                                             for delta = (abs (- next-range current-range))
                                             if (= 1 delta)
                                               do (incf holes delta) ;inside wall
                                             else if inside
                                                    do (incf holes delta)
                                                       (setf inside (char= (gethash (list (1+ next-range) x) dug #\.) #\#))
                                             else if (not inside)
                                                    do (incf holes)
                                                       (setf inside (char= (gethash (list (1+ next-range) x) dug #\.) #\.))
                                             finally (incf holes))
                                       )(loop for x from min-x upto max-x collect x))))))

(defun sort-columns (columns)
  (cl-tqdm:with-tqdm progress (length (alexandria:hash-table-keys columns)) "Sorting columns"
  (loop with sorted = (serapeum:dict)
        for x being the hash-keys of columns
        do (setf (gethash x sorted) (sort (gethash x columns) #'<))
        do (cl-tqdm:update progress)
        finally (format t "Sorted columns~%")
        finally (return sorted))))

(defun dig-2 (file)
  (loop with commands = (str:lines file)
        with dug = (serapeum:dict)
        with columns = (serapeum:dict) ; x to list of ys
        with y = 0 and x = 0
        for command in commands
        for (dir dist) = (decode-hex command)
        do (loop for i from 0 below dist
                if (char= dir #\U) do (decf y)
                if (char= dir #\D) do (incf y)
                if (char= dir #\L) do (decf x)
                if (char= dir #\R) do (incf x)
                do (setf (gethash (list y x) dug) #\#)
                   (setf (gethash x columns) (cons y (gethash x columns))))
        finally (format t "Built grid~%")
        finally (return (get-area-mt dug (sort-columns columns)))))

;;(print (time (dig-2 (frog:get-advent-of-code-input 2023 18 :input-suffix "test"))))
