(ql:quickload '(:str :cl-ppcre :binding-arrows :hu.dwim.defclass-star :alexandria :parseq :metabang-bind :fset :priority-queue :cl-tqdm :function-cache))
(defpackage :advent (:use :cl :cl-ppcre :binding-arrows :parseq :metabang-bind :hu.dwim.defclass-star))
(in-package :advent)
(declaim (optimize (speed 3) (safety 0) (debug 0) (compilation-speed 0) (space 0)))
(defclass* part () (x m a s))
(defclass* rule () (name slots-used rules)) ; Rule is a list of triples, First is the slot checked, second is the predicate lambda, third is the destination rule

(defun part-value (part)
  (with-slots (x m a s) part
    (+ x m a s)))

(defparameter *workflow-cache* (make-instance 'function-cache:lru-cache :capacity 1000000))
(function-cache:cached-results-count *workflow-cache*)
(declaim (inline cache-key))
(defun cache-key (name part slots-used)
  (let ((cache-key (list)))
    (push (if (member #\s slots-used) (s-of part) nil) cache-key)
    (push (if (member #\m slots-used) (m-of part) nil) cache-key)
    (push (if (member #\a slots-used) (a-of part) nil) cache-key)
    (push (if (member #\x slots-used) (x-of part) nil) cache-key)
    (push name cache-key)
    cache-key))

(let ((list (list 1 2)))
  (push 3 list)
  (push nil list)
  (print list))

(declaim (inline cache-result))
(defun cache-result (cache-key value)
  (setf (function-cache:get-cached-value *workflow-cache* cache-key) (list value)))

(defparameter *cache-hits* 0)
(defun run-workflow (rule part rules-by-name)
  (let* ((cache-key (cache-key (name-of rule) part (slots-used-of rule)))
         (cached (function-cache:get-cached-value *workflow-cache* cache-key)))
    (when cached
      (incf *cache-hits*)
;;      (break *workflow-cache*)
      (if (or (null (first cached)) (equal t (first cached)))
          (return-from run-workflow (first cached)))
      (return-from run-workflow (run-workflow (gethash (first cached) rules-by-name) part rules-by-name)))
    (loop for (slot predicate destination) in (rules-of rule)
          if (funcall predicate part)
            do (cond ((string= destination "R")
                      (cache-result cache-key nil)
                      (return nil))
                     ((string= destination "A")
                      (cache-result cache-key t)
                      (return t))
                     (t
                      (cache-result cache-key destination)
                      (return (run-workflow (gethash destination rules-by-name) part rules-by-name))))
          finally (cache-result cache-key nil)
          finally (return nil))))

(defun exec-rules (rules part)
  (let ((rules-by-name (serapeum:dict)))
    (loop for rule in rules do (setf (gethash (name-of rule) rules-by-name) rule))
    (run-workflow (gethash "in" rules-by-name) part rules-by-name)))

(defun get-slot-by-name (slot-name)
  (case slot-name
    (#\x 'x)
    (#\m 'm)
    (#\a 'a)
    (#\s 's)))

(defun get-comparison (comparison)
  (case comparison
    (#\= '=)
    (#\> '>)
    (#\< '<)))

(defun parse-rule (line)
  (let* ((parts (str:split "{" (str:substring 0 -1 line)))
         (name (first parts))
         (rule-text (str:split "," (second parts)))
         (rules (mapcar (lambda (r) (str:split ":" r)) rule-text)))
    (loop with parsed-rules = (list)
          for rule in rules
          if (= 1 (length rule))
            do (if (not (string= "R" (first rule))) (push (list nil (lambda (x) t) (first rule)) parsed-rules))
          else do (register-groups-bind (slot comparison value) ("(\\w+)([=<>])(\\w+)" (first rule))
                    (push (list (char slot 0) (lambda (x) (funcall (get-comparison (char comparison 0))
                                                     (slot-value x (get-slot-by-name (char slot 0)))
                                                     (parse-integer value)))
                                (second rule)) parsed-rules))
            finally (return (make-instance 'rule :name name :slots-used (mapcar #'first parsed-rules) :rules (nreverse parsed-rules))))))

(defun parse-part (line)
    (register-groups-bind (x m a s) ("\\{x=(\\d+),m=(\\d+),a=(\\d+),s=(\\d+)\\}" line)
      (make-instance 'part
                     :x (parse-integer x)
                     :m (parse-integer m)
                     :a (parse-integer a)
                     :s (parse-integer s))))

(defun part-1 (file)
  (setf *workflow-cache* (make-instance 'function-cache:lru-cache :capacity 1000000))
  (let* ((instructions (str:split frog:+double-newline+ file))
         (rules (mapcar #'parse-rule (str:lines (first instructions))))
         (parts (mapcar #'parse-part (str:lines (second instructions)))))
    (loop for part in parts
          for i from 0
          do (format t "Starting part ~a~%" i)
          if (exec-rules rules part)
            summing (part-value part))))

;;(print (time (part-1 (frog:get-advent-of-code-input 2023 19 :input-suffix "test"))))
;;(print (time (part-1 (frog:get-advent-of-code-input 2023 19))))

(defun part-2 (file)
  (setf *workflow-cache* (make-instance 'function-cache:lru-cache :capacity 1000000))
  (let* ((instructions (str:split frog:+double-newline+ file))
         (rules (mapcar #'parse-rule (str:lines (first instructions)))))
    (cl-tqdm:with-tqdm progress (* 4000 4000 4000 4000) ""
      (loop for x from 1 upto 4000
            do (format t "X=~a~%" x)
            summing (loop for m from 1 upto 4000
                          do (format t "M=~a~%" m)
                          summing (loop for a from 1 upto 4000
                                        do (cl-tqdm:update progress :incf 4000)
                                           (princ (format nil " A=~a" a))
                                        summing (loop for s from 1 upto 4000
                                                      if (exec-rules rules (make-instance 'part :x x :m m :a a :s s))
                                                        sum 1)))))))

;;(print (time (part-2 (frog:get-advent-of-code-input 2023 19 :input-suffix "test"))))
