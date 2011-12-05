; Project: Automated Theorem Prover
; snf.lisp
; Description: convert FOL to SNF
; Author: Mengqi Zong
; Email: mz2326@columbia.edu

(require 'unify)

(defvar *name-counter* 0)

(defun snf-set (set)
  "used to snf the kb"
  (cond
    ((null set) nil)
    ((listp set)
     (append
      (snf (car set))
      (snf-set (cdr set))))
    ))

(defun snf-clause (clause)
  "used to snf the nq"
  (car (snf clause)))

(defun snf (clause)
  "in order to make the kb and nq compatibile with the original code, this
   function can not directly generate a snf clause. Instead it generates
   a list: (snf_clause)"
  (let ((clause-ei (eliminate-implication clause)))
    (let ((clause-en (eliminate-negation clause-ei)))
      (let ((clause-eq (eliminate-quantifier clause-en)))
	(let ((clause-ds (distribute clause-eq)))
	  (let ((clause-cb (combine-predicate clause-ds)))
	    (let ((clause-ss (separate-sentence clause-cb)))
	      clause-ss
	      )))))))

(defun eliminate-biconditional (clause)
  "eliminate biconditonal"
  (cond
    ((null clause) nil)
    ((compound-p clause)
     (if (eq (op clause) '<=>)
	 (convert<=> clause)
	 (let ((s (copy-compound clause)))
	   (setf (args s) (eliminate-biconditional (args s)))
	   s)))
    ((listp clause)
     (cons
      (eliminate-biconditional (car clause))
      (eliminate-biconditional (cdr clause))))
    (t clause)))

(defun convert<=> (comp)
  "A<=>B -> A=>B ^ B=>A"
  (let ((A (eliminate-biconditional (nth 0 (args comp))))
	(B (eliminate-biconditional (nth 1 (args comp)))))
  (make-compound
   :op '^
   :args (list
	  (make-compound
	   :op '=>
	   :args (list A B))
	  (make-compound
	   :op '=>
	   :args (list B A)))
   )))

(defun eliminate-implication (clause)
  "eliminate implication"
  (cond
    ((null clause) nil)
    ((compound-p clause)
     (if (eq (op clause) '=>)
	 (convert=> clause)
	 (let ((s (copy-compound clause)))
	   (setf (args s) (eliminate-implication (args s)))
	   s)))
    ((listp clause)
     (cons
      (eliminate-implication (car clause))
      (eliminate-implication (cdr clause))))
    (t clause)))

(defun convert=> (comp)
  "eliminate implication for a implication compound A=>B -> !A V B"
  (let ((arg1 (eliminate-implication (nth 0 (args comp))))
	(arg2 (eliminate-implication (nth 1 (args comp)))))
  (make-compound
   :op 'V
   :args (list
	  (negate arg1)
	   arg2))))

(defun eliminate-negation (clause)
  "eliminate negation"
  (cond
    ((null clause) nil)
    ((compound-p clause)
     (if (eq (op clause) '!)
	 (convert! clause)
	 (let ((s (copy-compound clause)))
	   (setf (args s) (eliminate-negation (args s)))
	   s)))
    ((listp clause)
     (cons
      (eliminate-negation (car clause))
      (eliminate-negation (cdr clause))))
    (t clause)))

(defun convert! (comp)
  "eliminate negation for a negation compound"
  (let ((x (args comp)))
    (cond
      ((compound-p x)
       (convert!-compound comp))
      ; x is constant, like !John
      (t (copy-compound comp)))))

(defun convert!-compound (comp)
  "eliminate negation for !compound"
  (let ((c (copy-compound (args comp))))
    (cond
      ; !(A V B) -> !A ^ !B
      ((eq (op c) '^)
       (setf (op c) 'V)
       (setf (args c) (mapcar #'negate (args c)))
       (setf (args c) (eliminate-negation (args c)))
       c)
      ; !(A ^ B) -> !A V !B
      ((eq (op c) 'V)
       (setf (op c) '^)
       (setf (args c) (mapcar #'negate (args c)))
       (setf (args c) (eliminate-negation (args c)))
       c)
      ; !!A -> A
      ((eq (op c) '!)
       (args c))
      ((universal-p c)
       (let ((str (copy-seq (symbol-name (op c)))))
	 (setf (subseq str 0 1) "]")
	 (setf (op c) (read-from-string str)))
       (setf (args c) (negate (args c)))
       (setf (args c) (eliminate-negation (args c)))
       c)
      ((existential-p c)
       (let ((str (copy-seq (symbol-name (op c)))))
	 (setf (subseq str 0 1) "[")
	 (setf (op c) (read-from-string str)))
       (setf (subseq (op c) 0 1) "[")
       (setf (args c) (negate (args c)))
       (setf (args c) (eliminate-negation (args c)))
       c)
      ; !Mother(x, y)
      (t comp)
      )))

(defun eliminate-quantifier (clause)
  "eliminate quantifier"
  (cond
    ((null clause) nil)
    ((compound-p clause)
     (cond
       ((universal-p clause)
	(convert-universal clause))
       ((existential-p clause)
	(convert-existential clause))
       (t
	(let ((s (copy-compound clause)))
	  (setf (args s) (eliminate-quantifier (args s)))
	  s))
       ))
    ((listp clause)
     (cons
      (eliminate-quantifier (car clause))
      (eliminate-quantifier (cdr clause))))
    (t clause)))

(defun convert-universal (clause)
  "eliminate universal quantifier [$x"
  (eliminate-quantifier (copy-compound (args clause))))

(defun convert-existential (clause)
  "eliminate existential quantifier ]$x"
  (let* ((var (get-variable clause))
	 (sv (skolem-constant))
	 (theta (list (cons var sv))))
    (eliminate-quantifier (sub theta (args clause)))
    ))

(defun get-variable (clause)
  "get the variable from quantifier. For example, ]$x -> $x"
  (let* ((str (symbol-name (op clause)))
	 (strlen (length str)))
    (read-from-string
     (copy-seq (subseq str 1 strlen)))
    ))

(defun skolem-constant ()
  "generate a skolem variable, the skolem function."
  (setf *name-counter* (1+ *name-counter*))
  (read-from-string
   (concatenate 'string "C" (write-to-string *name-counter*)))
  )

(defun universal-p (x)
  "for all x = '[$x"
  (if (eq (char (symbol-name (op x)) 0) #\[)
      (if (eq (char (symbol-name (op x)) 1) #\$)
	  t
	  nil)
      nil))

(defun existential-p (x)
  "exists some x = ']$x"
  (if (eq (char (symbol-name (op x)) 0) #\])
      (if (eq (char (symbol-name (op x)) 1) #\$)
	  t
	  nil)
      nil))

(defun combine-predicate (clause)
  "combine the args together when predicate are the same"
  (cond
    ((null clause) nil)
    ((compound-p clause)
     (cond
       ((eq (op clause) '^)
	(combine-and clause))
       ((eq (op clause) 'V)
	(combine-or clause))
       (t
	(let ((s (copy-compound clause)))
	  (setf (args s) (combine-predicate (args s)))
	  s))
       ))
    ((listp clause)
     (cons
      (combine-predicate (car clause))
      (combine-predicate (cdr clause))))
    (t clause)))

(defun combine-and (comp)
  "A ^ (B ^ C) -> (A ^ B ^ C)"
  (let ((c (copy-compound comp)))
    (setf (args c) (and-again (args c)))
    c))

(defun and-again (unit)
  (cond
    ((null unit) nil)
    ((compound-p unit)
     (if (eq (op unit) '^)
	 (and-again (args unit))
	 (list (combine-predicate unit))))
    ((listp unit)
     (append
      (and-again (car unit))
      (and-again (cdr unit))))
    (t (list (combine-predicate unit)))))

(defun combine-or (comp)
  "A V (B V C) -> (A V B V C)"
  (let ((c (copy-compound comp)))
    (setf (args c) (or-again (args c)))
    c))

(defun or-again (unit)
  (cond
    ((null unit) nil)
    ((compound-p unit)
     (if (eq (op unit) 'V)
	 (or-again (args unit))
	 (list unit)))
    ((listp unit)
     (append
      (or-again (car unit))
      (or-again (cdr unit))))
    (t (list unit))))

(defun distribute (clause)
  (cond
    ((null clause) nil)
    ((compound-p clause)
     (if (eq (op clause) 'V)
	 (distribute-and clause)
	 (let ((s (copy-compound clause)))
	   (setf (args s) (distribute (args s)))
	   s)
	 ))
    ((listp clause)
     (cons
      (distribute (car clause))
      (distribute (cdr clause))))
    (t clause)))

(defun distribute-and (comp)
  "(A ^ B) V C -> (A V C) ^ (B V C)
   A V (B ^ C) -> (A V B) ^ (A V C)"
  (let* ((c (copy-compound comp))
	 (arg1 (nth 0 (args c)))
	 (arg2 (nth 1 (args c))))
    (cond
      ((and
	(compound-p arg1)
        (eq (op arg1) '^))
       (let ((A (nth 0 (args arg1)))
	     (B (nth 1 (args arg1))))
	 (setf (op c) '^)
	 (setf (args c)
	       (list
		(make-compound
		 :op 'V
		 :args (list A arg2))
		(make-compound
		 :op 'V
		 :args (list B arg2))))
	 (setf (args c) (distribute (args c)))
	 c))
      ((and
	(compound-p arg2)
	(eq (op arg2) '^))
       (let ((B (nth 0 (args arg1)))
	     (C (nth 1 (args arg1))))
	 (setf (op c) '^)
	 (setf (args c)
	       (list
		(make-compound
		 :op 'V
		 :args (list arg1 B))
		(make-compound
		 :op 'V
		 :args (list arg1 C))))
	 (setf (args c) (distribute (args c)))
	 c))
      (t
       (setf (args c) (distribute (args c)))
       c)
      )))

(defun separate-sentence (clause)
  "spearate the ^ to make multiple clauses"
  (cond
    ((and
      (compound-p clause)
      (eq (op clause) '^))
     (generate-sentence (args clause)))
    (t (generate-sentence clause))
    ))

(defun generate-sentence (clause)
  "Because the original code use list to represent or, for compatibility reason
   there will have some weird (list ...) code. Please see the examples in
   README."
  (cond
    ((null clause) nil)
    ((listp clause)
     (append
      (generate-sentence (car clause))
      (generate-sentence (cdr clause))))
    ((and
      (compound-p clause)
      (eq (op clause) 'V))
     (list (args clause)))
    (t (list (list clause)))
    ))