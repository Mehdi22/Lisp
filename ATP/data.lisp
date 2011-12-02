; Automated Theorem Prover
; data.lisp
; variables represent example knowledge bases and negate queries

(require 'unify)

; 1. Colonel West

(defvar *kb-west*
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

(defvar *nq-west*
  (list
   (negate (make-compound :op 'Criminal :args 'West))))

(provide 'data)