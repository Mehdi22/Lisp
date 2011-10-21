;
; Author: Mengqi Zong
;
; Email:  mz2326@columbia.edu
;

; Load queue.lisp first
(require :queue)

(defvar *adj*		; neighboors
  '((0 1 3)		; tile 0
    (1 0 2 4)		; tile 1
    (2 1 5)		; tile 2
    (3 0 4 6)		; tile 3
    (4 1 3 5 7)		; tile 4
    (5 2 4 8)		; tile 5
    (6 3 7)		; tile 6
    (7 4 6 8)		; tile 7
    (8 5 7)))		; tile 8

(defvar *adj-num*
  '(2 3 2 3 4 3 2 3 2))

(defvar *nodes-expanded* 0)

(defstruct node
  (state nil)
  (gap-loc nil)	; the location of 0, the space
  (cost 0)	; g(n), the path cost
  (h-val 0)	; h(n), the heuristic value of current state
  (parent nil))	; parent node

(defun solve-puzzle (&optional (heurf #'misplaced-tiles)
		     (initial-state '(0 1 2 3 4 5 6 7 8)) (step 30))
  "If you do not want to randomnize the initial state, just call this
   function like this: (solve-puzzle heurf state 0)."
  (let* ((start (init-node heurf initial-state))
	 (fringe (make-q :enqueue #'enqueue-priority :key #'fn-a*)))
    (random-init-node heurf start step)
    (format t "Initial state: ~a~%" (node-state start))
    (q-insert fringe (list start))
    (setf *nodes-expanded* 0)
    (format t "processing...~%")
    (A-star fringe nil heurf)
    ))

(defun A-star (fringe closed heurf)
  "Given a priority queue, pop a node to check if it is goal node.
   If not, insert the expanded nodes to the queue and call A-star
   again. Nothing special, the essence is in function expand."
  (when fringe
    (let ((node (q-front fringe)))
      (q-remove fringe)
      (cond ((is-goal-state (node-state node)) (generate-solution node))
	    ; in colsed?
	    ((member node closed :test
		     (lambda (x y)
		       (is-same-state (node-state x) (node-state y))))
	     (incf *nodes-expanded*)
	     (A-star (q-insert fringe (expand heurf node)) closed heurf))
	    (t (incf *nodes-expanded*)
	       (A-star (q-insert fringe (expand heurf node))
		       (cons node closed) heurf))
	  ))))

(defun init-node (heurf puzzle-state)
  "You have to init gap-loc and h-val for every new node, so write
   a new function."
  (let* ((zloc (find-gap puzzle-state))
	 (node (make-node :state (copy-list puzzle-state) :gap-loc zloc)))
    (setf (node-h-val node) (funcall heurf (node-state node)))
    node))

(defun expand (heurf puzzle)
  "generate a bunch of successors"
  (let* ((zloc (node-gap-loc puzzle))
	 (entry-num (nth zloc *adj-num*))
	 successors
	 successors-last)
    (loop for i from 1 to entry-num
	 do (let ((child (generate-child puzzle i heurf)))
	      (if (null successors)
		  (setf successors (list child))
		  (setf (cdr successors-last) (list child)))
	      (setf successors-last (last successors))))
    successors))

(defun generate-child (puzzle i heurf)
  (let* ((move (elt (elt *adj* (node-gap-loc puzzle)) i))
	 (child (make-node)))
    (setf (node-state child) (copy-list (node-state puzzle)))
    (move-puzzle child (node-gap-loc puzzle) move)
    (setf (node-gap-loc child) move)
    (setf (node-cost child) (1+ (node-cost puzzle)))
    (setf (node-h-val child) (funcall heurf (node-state child)))
    (setf (node-parent child) puzzle)
    child))

(defun generate-solution (puzzle)
  (reverse (action-sequence puzzle)))

(defun action-sequence (puzzle &optional (step 0))
  (if  (node-parent puzzle)
       (cons (node-state puzzle)
	     (action-sequence (node-parent puzzle) (1+ step)))
       (progn (format t "node: ~a~%" *nodes-expanded*)
	      (format t "step: ~a~%" step))
    ))

; Heuristic Function

(defun misplaced-tiles (state)
  (let ((mis-num 0))
    (loop for i from 0 to 8
	 do (if (/= (elt state i) (elt '(0 1 2 3 4 5 6 7 8) i))
		(setf mis-num (1+ mis-num))))
    mis-num))

; Helper Function

(defun random-init-node (heurf puzzle step)
  "randomly initiate state, randomly move the gap 'step' steps"
  (cond
    ((= step 0) (progn (setf (node-h-val puzzle)
			     (funcall heurf (node-state puzzle)))
		       puzzle))
    (t (let* ((adj-entry (nth (node-gap-loc puzzle) *adj*))
	      (entry-num (nth (node-gap-loc puzzle) *adj-num*))
	      (move (nth (+ 1 (random entry-num)) adj-entry)))
	 (move-puzzle puzzle (node-gap-loc puzzle) move)
	 (setf (node-gap-loc puzzle) move)
	 (decf step)
	 (random-init-node heurf puzzle step)))))

(defun is-goal-state (puzzle)
  (equal puzzle '(0 1 2 3 4 5 6 7 8)))

(defun is-same-state (s1 s2)
  (equal s1 s2))

(defun find-gap (puzzle-state)
  "find the location of zero"
  (loop for i from 0 to 8
     do (if (= (elt puzzle-state i) 0)
	    (return-from find-gap i))))

(defun fn-a* (puzzle)
  (+ (node-cost puzzle) (node-h-val puzzle)))

(defun move-puzzle (puzzle i j)
  "Swap the ith element and jth element of puzzle"
  (let ((i_val (elt (node-state puzzle) i))
	(j_val (elt (node-state puzzle) j)))
    (setf (elt (node-state puzzle) i) j_val)
    (setf (elt (node-state puzzle) j) i_val)))