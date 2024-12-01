(ql:quickload '(:str :cl-ppcre :binding-arrows :hu.dwim.defclass-star :alexandria :parseq :metabang-bind :fset :priority-queue :cl-tqdm :function-cache))
(defpackage :advent (:use :cl :cl-ppcre :binding-arrows :parseq :metabang-bind :hu.dwim.defclass-star))
(in-package :advent)
(declaim (optimize (speed 3) (safety 0) (debug 0) (compilation-speed 0) (space 0)))
(defclass* brick () (xs ys zs xe ye ze id))
(defmethod print-object ((brick brick) stream)
  (with-slots (xs ys zs xe ye ze id) brick
    (format stream "~a:~a,~a,~a~~~a,~a,~a" id xs ys zs xe ye ze)))

(defparameter *id-counter* -1)
(defun parse-brick (line)
  (register-groups-bind (xs ys zs xe ye ze) ("(\\d+),(\\d+),(\\d+)~(\\d+),(\\d+),(\\d+)" line)
    (make-instance 'brick :xs (parse-integer xs) :ys (parse-integer ys) :zs (parse-integer zs)
                          :xe (parse-integer xe) :ye (parse-integer ye) :ze (parse-integer ze)
                          :id (incf *id-counter*))))

(defun drop-brick (brick ground &optional (preview nil))
  (with-slots (xs ys zs xe ye ze id) brick
    (if (loop for x from xs to xe
              always (loop for y from ys to ye
                           always (every #'null (loop for z from zs to ze
                                                      do (format nil "~a(~a,~a)=~a ground=~a~%" id x y z (aref ground x y))
                                                      collect (= 1 (- z (aref ground x y)))))))
        (progn (format nil "Dropping ~a~%" brick)
               (unless preview (decf zs) (decf ze))
               nil)
        (progn (format nil "Hit Ground: ~a~%" brick)
               t))))

(defun can-remove (brick bricks)
  (let ((occupied (loop with out = (serapeum:dict)
                        for other in bricks
                        do (unless (eq brick other)
                             (with-slots (xs ys zs xe ye ze id) other
                               (loop for x from xs to xe
                                     do (loop for y from ys to ye
                                              do (loop for z from zs to ze
                                                       do (setf (gethash (list x y z) out) id))))))
                        finally (return out))))
    (with-slots (xs ys zs xe ye ze id) brick
      (loop for x from xs to xe
            do (loop for y from ys to ye
                     do (loop for z from zs to ze
                              do (when (gethash (list x y (1+ z)) occupied)
                                   (format nil "Cannot remove ~a because it is occupied by ~a~%" id (gethash (list x y (1+ z)) occupied))
                                   (return-from can-remove nil))))
            finally (return t)))))

(defun adjust-ground (brick ground)
  (with-slots (xs ys zs xe ye ze) brick
    (loop for x from xs to xe
          do (loop for y from ys to ye
                   if (< (aref ground x y) ze) do (format nil "Adjusting ground ~a,~a to ~a~%" x y ze)
                   do (setf (aref ground x y) (max (aref ground x y) ze))))
    ground))

(defun build-ground (brick bricks)
  (with-slots (xs ys zs xe ye ze) brick
    (loop with ground = (make-array (list (1+ (apply #'max (mapcar #'xe-of bricks)))
                                          (1+ (apply #'max (mapcar #'ye-of bricks)))) :initial-element 0)
          for brick in bricks
          if (or (< (zs-of brick) zs) (< (ze-of brick) zs))
            do (setf ground (adjust-ground brick ground))
          finally (return ground))))

(defun part-1 (file)
  (loop with bricks = (sort (mapcar #'parse-brick (str:lines file)) #'< :key #'zs-of)
        with floating = bricks
        with ground = (make-array (list (1+ (apply #'max (mapcar #'xe-of bricks)))
                                        (1+ (apply #'max (mapcar #'ye-of bricks)))) :initial-element 0)
        with progress = (cl-tqdm:tqdm (length bricks) "")
        with last = nil
        while floating
        for brick = (first floating)
        if (drop-brick brick ground)
          do (setf floating (rest floating))
             (cl-tqdm:update progress)
             (setf ground (adjust-ground brick ground))
        finally (format t "Testing removal~%")
                (return (loop with progress = (cl-tqdm:tqdm (length bricks) "")
                              for test in bricks
                              for others = (remove test bricks)
                              do (cl-tqdm:update progress)
                              count (loop for other in others
                                          for ground = (build-ground other others)
                                          always (drop-brick other ground t))))))

;;(print (time (part-1 (frog:get-advent-of-code-input 2023 22 :input-suffix "test"))))
;;(print (time (part-1 (frog:get-advent-of-code-input 2023 22))))

(defun chain-reaction (others)
  (loop with out = (list)
        with recurse = others
        for other in others
        for ground = (build-ground other others)
        if (not (drop-brick other ground t))
          do (setf recurse (remove other recurse))
             (push (id-of other) out)
        finally (return (if out (append out (chain-reaction recurse)) out))))

(defun part-2 (file)
  (loop with bricks = (sort (mapcar #'parse-brick (str:lines file)) #'< :key #'zs-of)
        with floating = bricks
        with ground = (make-array (list (1+ (apply #'max (mapcar #'xe-of bricks)))
                                        (1+ (apply #'max (mapcar #'ye-of bricks)))) :initial-element 0)
        with progress = (cl-tqdm:tqdm (length bricks) "")
        with last = nil
        while floating
        for brick = (first floating)
        if (drop-brick brick ground)
          do (setf floating (rest floating))
             (cl-tqdm:update progress)
             (setf ground (adjust-ground brick ground))
        finally (format t "Testing removal~%")
                (return (loop with progress = (cl-tqdm:tqdm (length bricks) "")
                              for test in bricks
                              do (cl-tqdm:update progress)
                              sum (length (remove-duplicates (chain-reaction (remove test bricks))))))))
;;(print (time (part-2 (frog:get-advent-of-code-input 2023 22 :input-suffix "test"))))
(print (time (part-2 (frog:get-advent-of-code-input 2023 22))))
