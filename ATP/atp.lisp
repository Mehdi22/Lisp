; Automated Theorem Proving

(require 'unify)

(defvar *KB*
  (list
   ; !American(x) V !Weapon(y) V !Sells(x, y, z) V !Hostile(z) V Criminal(x)
   (list
    (negate (make-compound :op 'American :args '$x))
    (negate (make-compound :op 'Weapon :args '$y))
    (negate (make-compound :op 'Sells :args '($x $y $z)))
    (negate (make-compound :op 'Hostile :args '$z))
    (make-compound :op 'Criminal :args '$x))
   ; Owns(Nono, M1)
   (list
    (make-compound :op 'Owns :args '(Nono M1)))
   ; Missile(M1)
   (list
    (make-compound :op 'Missile :args 'M1))
   ; !Missile(y) V !Owns(Nono, y) V Sells(West, y, Nono)
   (list
    (negate (make-compound :op 'Missile :args '$y))
    (negate (make-compound :op 'Owns :args '(Nono $y)))
    (make-compound :op 'Sells :args '(West $y Nono)))
   ; !Missile(y) V Weapon(y)
   (list
    (negate (make-compound :op 'Missile :args '$y))
    (make-compound :op 'Weapon :args '$y))
   ; !Enemy(z, America) V Hostile(z)
   (list
    (negate (make-compound :op 'Enemy :args '($z America)))
    (make-compound :op 'Hostile :args '$z))
   ; American(West)
   (list
    (make-compound :op 'American :args 'West))
   ; Enemy(Nono, America)
   (list
    (make-compound :op 'Enemy :args '(Nono America)))
   ))

(defvar *NQ*
  (list
   (negate (make-compound :op 'Criminal :args 'West))))

(defun atp (&optional (kb *KB*) (nq *NQ*))
  (format t "s1 = ~a~%" (car kb))
  (format t "s2 = ~a~%" (cadddr kb))
  (format t "nq = ~a~%" nq)
  (resolution (car kb) (cadddr kb))
  )

(defun resolution (clause1 clause2)
  ; for i = 1 to len(s1)
  ;   for j = 1 to len(s2)
  ;     c1 = s1[i]
  ;     c2 = s2[j]
  ;     if total_negate is an odd number
  ;        if op(c1) = op(c2)
  ;           if (unification(c1, c2) != nil)
  ;              return *success*
  ; *fail*
  (resolve clause1 0 clause2 0))

(defun resolve (clause1 n1 clause2 n2)
  (let ((max1 (list-length clause1))
	(max2 (list-length clause2)))
    (if (< n1 max1)
	(if (< n2 max2)
	    (let ((theta (resolve-unit clause1 n1 clause2 n2)))
	      (if (eq theta 'Fail)
		  (resolve clause1 n1 clause2 (1+ n2))
		  (make-resolution clause1 n1 clause2 n2 theta)))
	    (resolve clause1 (1+ n1) clause2 0))
	'Fail)
    ))

(defun make-resolution (clause1 index1 clause2 index2 theta)
  (let ((r1 (remove (nth index1 clause1) clause1))
	(r2 (remove (nth index2 clause2) clause2)))
    (setf r1 (sub theta r1))
    (setf r2 (sub theta r2))
    (list clause1 clause2 (append r1 r2))
    ))

(defun resolve-unit (clause1 n1 clause2 n2)
  (let ((unit1 (nth n1 clause1))
	(unit2 (nth n2 clause2))
	(negate_num1 0)
	(negate_num2 0))
    (setf negate_num1 (get-negate-num unit1 negate_num1))
    (setf negate_num2 (get-negate-num unit2 negate_num2))
    (if (eql (mod (+ negate_num1 negate_num2) 2) 1)
	(unification (remove-negate unit1 negate_num1)
		     (remove-negate unit2 negate_num2))
	'Fail)
    ))

(defun get-negate-num (unit num)
  (if (compound-p unit)
      (if (eq (op unit) '!)
	  (get-negate-num (args unit) (1+ num))
	  num)
      num))

(defun remove-negate (unit num)
  (if (> num 0)
      (remove-negate (args unit) (1- num))
      unit))

(defun unification (unit1 unit2)
  (let ((theta (unify unit1 unit2)))
    (if (eq theta 'Fail)
	'Fail
	theta)
    ))

; algorithm
; space = kb V nq
; while (space != null) {
;   len = len(space)
;   for i = 1 to len - 1
;       for j = i+1 to len
;         resolve(s[i], s[j]);
;         if resolve success
;           add two new one, mark old one as old

