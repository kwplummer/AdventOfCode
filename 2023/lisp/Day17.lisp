(ql:quickload '(:str :cl-ppcre :binding-arrows :hu.dwim.defclass-star :metabang-bind :priority-queue))
(defpackage :advent (:use :cl :cl-ppcre :binding-arrows :metabang-bind :hu.dwim.defclass-star))
(in-package :advent)

(defparameter *part-2* nil)
(defclass* dirpos () (y x dy dx))
(defun nnth (y x l) (->> l (nth y) (nth x)))
(defun moves (dp steps max-y max-x)
  (with-slots (y x dy dx) dp
    (let ((out (list)))
      (if (< steps (if *part-2* 10 3)) (push (list (1+ steps) (make-instance 'dirpos :y (+ y dy) :x (+ x dx) :dy dy :dx dx)) out))
      (when (or (not *part-2*) (>= steps 4))
        (when (= dy 0)
          (push (list 1 (make-instance 'dirpos :y (1+ y) :x x :dy 1 :dx 0)) out)
          (push (list 1 (make-instance 'dirpos :y (1- y) :x x :dy -1 :dx 0)) out))
        (when (= dx 0)
          (push (list 1 (make-instance 'dirpos :y y :x (1+ x) :dy 0 :dx 1)) out)
          (push (list 1 (make-instance 'dirpos :y y :x (1- x) :dy 0 :dx -1)) out)))
      (remove-if (lambda (pos) (or (< (y-of pos) 0) (< (x-of pos) 0)
                                   (>= (y-of pos) max-y) (>= (x-of pos) max-x)))
                 out :key #'second))))

(defun parse-input (input)
  (->> input (str:lines)
    (mapcar (lambda (l) (cl-ppcre:all-matches-as-strings "(\\d)" l)))
    (mapcar (lambda (l) (mapcar #'parse-integer l)))))

(defun a-star (input)
  (let* ((parsed (parse-input input))
         (max-y (length parsed)) (max-x (length (first parsed)))
         (min-distance (make-hash-table :test #'equal))
         (best-distance most-positive-fixnum)
         (start-pos (make-instance 'dirpos :y 0 :x 0 :dy 0 :dx 1))
         (queue (priority-queue:make-pqueue #'<)))
    (setf (gethash (list 0 0 0 1 0) min-distance) 0)
    (priority-queue:pqueue-push (list 0 start-pos 0) 0 queue)
    (loop with counter = 0
          until (priority-queue:pqueue-empty-p queue)
          for next = (priority-queue:pqueue-pop queue)
          for dist = (first next) and pos = (second next) and steps = (third next)
          do (loop for (new-steps move) in (moves pos steps max-y max-x)
                   for y = (y-of move) and x = (x-of move) and dy = (dy-of move) and dx = (dx-of move)
                   for key = (list y x dy dx new-steps)
                   for new-distance = (+ dist (nnth y x parsed))
                   until (< best-distance new-distance)
                   for old-distance = (gethash key min-distance best-distance)
                   if (< new-distance old-distance)
                     do (setf (gethash key min-distance) new-distance)
                        (priority-queue:pqueue-push (list new-distance move new-steps) new-distance queue)
                        (when (and (= (1- max-y) y) (= (1- max-x) x) (or (not *part-2*) (>= new-steps 4)))
                          (setf best-distance (min best-distance new-distance))))
          finally (return best-distance))))
(print (time (a-star (frog:get-advent-of-code-input 2023 17))))
(let ((*part-2* t)) (print (time (a-star (frog:get-advent-of-code-input 2023 17)))))
