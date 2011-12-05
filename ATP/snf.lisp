; Project: Automated Theorem Prover
; snf.lisp
; Description: convert FOL to SNF
; Author: Mengqi Zong
; Email: mz2326@columbia.edu

(require 'unify)

(defvar *name-counter* 0)

(defun snf-set (set)
  (cond
    ((null set) nil)
    ((listp set)
     (cons
      (snf (car set))
      (snf-set (cdr set))))
    ))

(defun snf (clause)
  (let ((clause-ei (eliminate-implication clause)))
    (let ((clause-en (eliminate-negation clause-ei)))
      (let ((clause-eq (eliminate-quantifier clause-en)))
	clause-eq
	)
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
 	 (sv (skolem-variable))
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

(defun skolem-variable ()
  "generate a skolem variable, the skolem function."
  (setf *name-counter* (1+ *name-counter*))
  (read-from-string
   (concatenate 'string "$" (write-to-string *name-counter*)))
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

(defun distribute (clause)
  "(A ^ B) V C -> (A V C) ^ (B V C)
   A V (B ^ C) -> (A V B) ^ (A V C)"
  clause
  )