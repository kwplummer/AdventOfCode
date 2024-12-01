;; This solution is no fun, it swaps out J with each character. Part 1 is not included, sa it's irrelevant.
(ql:quickload '(:str :cl-ppcre :binding-arrows :hu.dwim.defclass-star :alexandria :parseq :metabang-bind :fset :priority-queue))
(defpackage :advent (:use :cl :cl-ppcre :binding-arrows :parseq :metabang-bind :hu.dwim.defclass-star))
(in-package :advent)
(declaim (optimize (speed 3) (safety 0) (debug 0)))
(defparameter *card-ordering* '(#\A #\K #\Q #\T #\9 #\8 #\7 #\6 #\5 #\4 #\3 #\2 #\J))
(defparameter *card-ordering-no-j* '(#\A #\K #\Q #\T #\9 #\8 #\7 #\6 #\5 #\4 #\3 #\2))
(defclass* hand () (cards bid (classification nil)))

(defun count-chars (list)
  (loop with counts = (make-hash-table)
        for char in list
        do (incf (gethash char counts 0))
        finally (return counts)))

(defun five-of-a-kind (hand) (every (lambda (card) (equal card (first hand))) hand))
(defun four-of-a-kind (hand)
  (= 4 (first (sort (alexandria:hash-table-values (count-chars hand)) #'>))))
(defun three-of-a-kind (hand)
  (= 3 (first (sort (alexandria:hash-table-values (count-chars hand)) #'>))))
(defun full-house (hand)
  (let ((counts (alexandria:hash-table-values (count-chars hand))))
    (and (= 3 (first (sort counts #'>)))
         (= 2 (second (sort counts #'>))))))
(defun two-pair (hand)
  (let ((counts (alexandria:hash-table-values (count-chars hand))))
    (and (= 2 (first (sort counts #'>)))
         (= 2 (second (sort counts #'>))))))
(defun one-pair (hand)
  (let ((counts (alexandria:hash-table-values (count-chars hand))))
    (= 2 (first (sort counts #'>)))))
(defun high-card (hand)
  (every (lambda (count) (= 1 count))
         (alexandria:hash-table-values (count-chars hand))))


(defun classify-hand (hand)
  (if (member #\J hand)
      (loop with original = (copy-list hand)
            with pos = (position #\J hand)
            for char in *card-ordering-no-j*
            do (setf (nth pos hand) char)
            maximize (classify-hand hand)
            finally (setf (nth pos hand) #\J))
  (cond ((five-of-a-kind hand) 7)
        ((four-of-a-kind hand) 6)
        ((full-house hand) 5)
        ((three-of-a-kind hand) 4)
        ((two-pair hand) 3)
        ((one-pair hand) 2)
        ((high-card hand) 1))))

(defun right-first (left right)
  (loop for i from 0 below (min (length left) (length right))
        for left-card = (position (nth i left) *card-ordering*)
        for right-card = (position (nth i right) *card-ordering*)
        when (< left-card right-card) return nil
          when (> left-card right-card) return t))

(defun compare-hands (left right)
  (cond ((< (classification-of left) (classification-of right)) t)
         ((> (classification-of left) (classification-of right)) nil)
         (t (right-first (cards-of left) (cards-of right)))))

(defun parse-hand (line)
  (let* ((parts (str:split #\Space line))
         (hand (make-instance 'hand :cards (coerce (first parts) 'list)
                              :bid (parse-integer (second parts)))))
    (setf (classification-of hand) (classify-hand (cards-of hand)))
    hand))

(defun part-2 (file)
  (let ((sorted (-<>> file
                  (str:lines)
                  (mapcar #'parse-hand)
                  (sort <> #'compare-hands)
                  (mapcar #'bid-of))))
    (loop for i from 0 below (length sorted)
          for bid = (nth i sorted)
          sum (* (1+ i) bid))))

(print (time (part-2 (frog:get-advent-of-code-input 2023 7 :input-suffix "test"))))
(print (time (part-2 (frog:get-advent-of-code-input 2023 7))))