; Project: Automated Theorem Prover
; atp.lisp
; Description: automated theorem prover main function
; Author: Mengqi Zong
; Email: mz2326@columbia.edu

(require 'unify)

(defun atp (kb nq)
  (let ((kb-cpy (copy-list kb))
	(nq-cpy (copy-list nq)))
    (setf kb-cpy (sort-set kb-cpy))
    (prove kb-cpy nq-cpy nil)
    ))

(defun generate-set (kb nq)
  (cons nq kb))

(defun sort-set (set)
  "sort the set by each clause's number of units, clause with less units
   is in the front of the list represents the set"
  (sort set #'less-length))

(defun less-length (clause1 clause2)
  (let ((l1 (list-length clause1))
	(l2 (list-length clause2)))
    (if (< l1 l2) T nil)))

(defun prove (kb nq rl)
  "rl stands for resolution-list, the return value"
  (let ((set (generate-set kb nq)))
    (inference set 0 1 rl)))

(defun inference (set n1 n2 rl)
  (let ((set-size (list-length set)))
    (if (< n1 (1- set-size))
	(if (< n2 set-size)
	    (let* ((clause1 (nth n1 set))
		   (clause2 (nth n2 set))
		   (triple (resolution clause1 clause2)))
	      (cond
		; resolution failed, try next pair
		((eq triple 'Fail)
		 (inference set n1 (1+ n2) rl))
		; resolution succeeded
		(t (let ((resolvent (third triple)))
		     (if (null resolvent)
			 ; resolvent is nil, program ends
			 (cons triple rl)
			 ; not nil, not finished. We have a new clause.
			 ; we can now prove on the new set
			 (if (null (in-set set resolvent))
			     (if (resolution-consistent rl triple)
				 ; previous resolutions are related with this
				 (prove set resolvent (cons triple rl))
				 ; previous resolutions are not related
				 ; this means previous resolution has nothing
				 ; to with the "nil" in the future
				 (prove set resolvent (cons triple nil)))
			     ; resolvent already exist, try next pair
			     (inference set n1 (1+ n2) rl))
			 )))))
	      ; n2 >= set-size
	      (inference set (1+ n1) (1+ (1+ n1)) rl))
	    ; n1 >= set-size: have tried all possible pairs
	    'Fail)
	))

(defun in-set (set clause)
  (find clause set :test #'equalp))

(defun resolution-consistent (rl triple)
  (let* ((prev-clause (third (car rl)))
	 (clause1 (first triple))
	 (clause2 (second triple)))
    (or
     (equalp prev-clause clause1)
     (equalp prev-clause clause2))
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
	 (negation_num1 (negation-num unit1 0))
	 (negation_num2 (negation-num unit2 0)))
    (if (eql (mod (+ negation_num1 negation_num2) 2) 1)
	(unify (remove-negation unit1 negation_num1)
	       (remove-negation unit2 negation_num2))
	'Fail)
    ))

(defun negation-num (unit num)
  "calculate unit's negation number"
  (if (compound-p unit)
      (if (eq (op unit) '!)
	  (negation-num (args unit) (1+ num))
	  num)
      num))

(defun remove-negation (unit num)
  "remove negation in order to unify"
  (if (> num 0)
      (remove-negation (args unit) (1- num))
      unit))
