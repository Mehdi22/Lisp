; Automated Theorem Prover
; atp.lisp

(require 'unify)
(require 'data)

(defun atp (&optional (kb *kb-west*) (nq *nq-west*))
  (let ((set (generate-set kb nq)))
    (inference set 0 1)
    ))

(defun generate-set (kb nq)
  (cons nq kb))

(defun inference (set n1 n2)
  (let ((set-size (list-length set)))
    (if (< n1 (1- set-size))
	(if (< n2 set-size)
	    (let* ((clause1 (nth n1 set))
		   (clause2 (nth n2 set))
		   (triple (resolution clause1 clause2)))
	      (cond ((eq triple 'Fail)
		     (inference set n1 (1+ n2)))
		    (t
		     (format t "T = ~a~%" triple)
		     (let ((resolvent (third triple)))
		       (if (null resolvent)
			   nil
			   (atp set resolvent))))))
	      (inference set (1+ n1) (1+ (1+ n1))))
	    'Fail)
	))

(defun infer (set n1 n2)
  (let ((clause1 (nth n1 set))
	(clause2 (nth n2 set)))
    (resolution clause1 clause2)
    ))

; *********  resolution  *********

(defun resolution (clause1 clause2)
  "resolve clause1 and clause2"
  (resolve clause1 0 clause2 0))

(defun resolve (clause1 n1 clause2 n2)
  "resolve clause1 and clause2 from clause1's n1th unit and clause2's n2th unit"
  (let ((max1 (list-length clause1))
	(max2 (list-length clause2)))
    (if (< n1 max1)
	(if (< n2 max2)
	    (let ((theta (resolve-unit clause1 n1 clause2 n2)))
	      (if (eq theta 'Fail)
		  (resolve clause1 n1 clause2 (1+ n2))
		  (make-resolution-triple clause1 n1 clause2 n2 theta)))
	    (resolve clause1 (1+ n1) clause2 0))
	'Fail)
    ))

(defun make-resolution-triple (clause1 index1 clause2 index2 theta)
  "generate resolution triple (clause1 clause2 resolvent)"
  (let ((r1 (remove (nth index1 clause1) clause1))
	(r2 (remove (nth index2 clause2) clause2)))
    (setf r1 (sub theta r1))
    (setf r2 (sub theta r2))
    (list clause1 clause2 (append r1 r2))
    ))

(defun resolve-unit (clause1 n1 clause2 n2)
  "resolve clause1's n1th unit and clause2's n2th unit"
  (let* ((unit1 (nth n1 clause1))
	 (unit2 (nth n2 clause2))
	 (negate_num1 (negate-num unit1 0))
	 (negate_num2 (negate-num unit2 0)))
    (if (eql (mod (+ negate_num1 negate_num2) 2) 1)
	(unify (remove-negate unit1 negate_num1)
	       (remove-negate unit2 negate_num2))
	'Fail)
    ))

(defun negate-num (unit num)
  "calculate unit's negation number"
  (if (compound-p unit)
      (if (eq (op unit) '!)
	  (negate-num (args unit) (1+ num))
	  num)
      num))

(defun remove-negate (unit num)
  "remove negation in order to unify"
  (if (> num 0)
      (remove-negate (args unit) (1- num))
      unit))