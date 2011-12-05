; Project: Automated Theorem Prover
; data.lisp
; Description: example knowledge bases and negate queries
; Author: Mengqi Zong
; Email: mz2326@columbia.edu

(require 'unify)

; 1. Colonel West Test

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

; 2. resolvent relavence check test
; ** Without resolution consistence checking, it will return not relevent
; ** resolution triples

(defvar *kb-symbol*
  (list
   ; !A V !B V !C V D
   (list (negate 'A) (negate 'B) (negate 'C) 'D)
   ; !E V !F V !G V D
   (list (negate 'E) (negate 'F) (negate 'G) 'D)
   '(A)
   '(B)
   '(E)
   '(F)
   '(G)
   ))

(defvar *nq-symbol*
  (list
   (negate 'D)))

(defvar *kb-evil*
  (list
   ; Vx King(x)^Greedy(x)=>Evil(x)
   (list
    (make-compound
     :op '[$x
     :args (make-compound
	    :op '=>
	    :args (list
		   (make-compound
		    :op '^
		    :args (list
			    (make-compound
			     :op 'King
			     :args '$x)
			    (make-compound
			     :op 'Greedy
			     :args '$x)))
		   (make-compound
		    :op 'Evil
		    :args '$x)))))
   ; King(Richard)
   (list (make-compound :op 'King :args 'Richard))
   ; King(John)
   (list (make-compound :op 'King :args 'John))
   ; Greedy(John)
   (list (make-compound :op 'Greedy :args 'John))
   ))

(defvar *nq-evil*
  (list
   (negate (make-compound :op 'Evil :args 'John))))


; used to test function snf: (snf *love*)
(defvar *love*
  ; Vx ((Vy Animal(y)=>Loves(x,y))=>Ez Loves(z,x))
  ; ->
  ; (Animal(F(x)) V Loves(G(x), x)) ^ (!Loves(x, F(x)) V Loves(G(x), x))
  (list
   (make-compound
    :op '[$x
    :args (make-compound
	   :op '=>
	   :args (list
		  (make-compound
	           :op '[$y
	           :args (make-compound
		          :op '=>
		          :args (list
			         (make-compound
				  :op 'Animal
				  :args '$y)
				 (make-compound
				  :op 'Loves
				  :args '($x $y)))))
		  (make-compound
		   :op ']$z
		   :args (make-compound
			  :op 'Loves
			  :args '($z $x)))
		  )))))

(provide 'data)
