; Automated Theorem Proving
;

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
   ; !Missile(x) V !Owns(Nono, x) V Sells(West, x, Nono)
   (list
    (negate (make-compound :op 'Missile :args '$x))
    (negate (make-compound :op 'Owns :args '(Nono $x)))
    (make-compound :op 'Sells :args '(West $x Nono)))
   ; !Missile(x) V Weapon(x)
   (list
    (negate (make-compound :op 'Missile :args '$x))
    (make-compound :op 'Weapon :args '$x))
   ; !Enemy(x, America) V Hostile(x)
   (list
    (negate (make-compound :op 'Enemy :args '($x America)))
    (make-compound :op 'Hostile :args '$x))
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
  (format t "KB:~%~a~%" kb)
  (format t "NQ:~%~a~%" nq)
  )

; algorithm
; space = kb V nq
; while (space != null) {
;   len = len(space)
;   for i = 1 to len - 1
;       for j = i+1 to len
;         resolve(s[i], s[j]);
;         if resolve success
;           add two new one, mark old one as old
;         
;         
;   
;
;
