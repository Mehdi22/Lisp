; The Eight Puzzle Problem
;
; Author: Mengqi Zong
; Email:  mz2326@columbia.edu
;

; Load queue.lisp first
(require :queue)

; *** Global variable declaration ***

(defvar *adj*		; neighboors for each tile, first element is tile.
  '((0 1 3)		; tile 0
    (1 0 2 4)		; tile 1
    (2 1 5)		; tile 2
    (3 0 4 6)		; tile 3
    (4 1 3 5 7)		; tile 4
    (5 2 4 8)		; tile 5
    (6 3 7)		; tile 6
    (7 4 6 8)		; tile 7
    (8 5 7)))		; tile 8

(defvar *adj-num*	; number of neighboors for each tile
  '(2 3 2 3 4 3 2 3 2))

(defvar *distance*	; distance for each tile to reach the goal tile
  '((0 1 2 1 2 3 2 3 4)	; tile 0
    (1 0 1 2 1 2 3 2 3) ; tile 1
    (2 1 0 3 2 1 4 3 2) ; tile 2
    (1 2 3 0 1 2 1 2 3) ; tile 3
    (2 1 2 1 0 1 2 1 2) ; tile 4
    (3 2 1 2 1 0 3 2 1) ; tile 5
    (2 3 4 1 2 3 0 1 2) ; tile 6
    (3 2 3 2 1 2 1 0 1) ; tile 7
    (4 3 2 3 2 1 2 1 0) ; tile 8
    ))

(defvar *conflict*	; 0 means no conflict              Graph 4 5 6
  			; 1-3 means conflict line is line 1-3 | 1
  			; 4-6 means conflict line is row 1-3  | 2
  			;                                     | 3
  '((0 1 1 4 0 0 4 0 0)	; tile 0
    (1 0 1 0 5 0 0 5 0) ; tile 1
    (1 1 0 0 0 6 0 0 6) ; tile 2
    (4 0 0 0 2 2 4 0 0) ; tile 3
    (0 5 0 2 0 2 0 5 0) ; tile 4
    (0 0 6 2 2 0 0 0 6) ; tile 5
    (4 0 0 4 0 0 0 3 3) ; tile 6
    (0 5 0 0 5 0 3 0 3) ; tile 7
    (0 0 6 0 0 6 3 3 0) ; tile 8
    ))

(defvar *nodes-expanded* 0)

; *** main functions ***

; state representation
(defstruct node
  (state nil)	; the state
  (gap-loc nil)	; the location of 0, the space
  (cost 0)	; g(n), the path cost
  (h-val 0)	; h(n), the heuristic value of current state
  (parent nil))	; parent node

; If you do not want to randomnize the initial state, just call this
; function like this: (solve-puzzle heurf state 0).
(defun solve-puzzle (&optional (heurf #'misplaced-tiles)
		     (initial-state '(0 1 2 3 4 5 6 7 8)) (step 30))
  (let* ((start (init-node heurf initial-state))
	 (fringe (make-q :enqueue #'enqueue-priority :key #'fn-a*)))
    (random-init-node heurf start step)
    (format t "Initial state: ~a~%" (node-state start))
    (q-insert fringe (list start))
    (setf *nodes-expanded* 0)
    (format t "processing...~%")
    (A-star fringe nil heurf)
    ))

; Just A-star, like the graph-search.
(defun A-star (fringe closed heurf)
  (when fringe
    (let ((node (q-front fringe)))
      (q-remove fringe)
      (cond ((is-goal-state (node-state node)) (generate-solution node))
	    ; in colsed?
	    ((member node closed :test
		     (lambda (x y)
		       (is-same-state (node-state x) (node-state y))))
	     (incf *nodes-expanded*)
	     (A-star fringe closed heurf))
	    (t (incf *nodes-expanded*)
	       (A-star (q-insert fringe (expand heurf node))
		       (cons node closed) heurf))
	  ))
    ))

; Function expand calls function generate-child to generate each successor.
; And at last return a list of successors.
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

; *** Heuristic Function ***

(defun misplaced-tiles (state)
  (let ((mis-num 0))
    (loop for i from 0 to 8
	 do (let ((num (elt state i)))
	      (when (/= num 0)
		  (when (/= num i)
		      (setf mis-num (1+ mis-num))
		      ))))
    mis-num))

(defun manhattan-distance (state)
  (let ((total-dst 0))
    (loop for i from 0 to 8
	 do (let* ((num (elt state i))
		   (dst (elt (elt *distance* i) num)))
	      ; Blank is not a real tile
	      (if (/= num 0)
		  (setf total-dst (+ total-dst dst)))
	      ))
    total-dst))

(defun linear-conflict (state)
  (let ((total-dst 0))
    (loop for i from 0 to 8
	 do (let* ((num (elt state i))
		   (dst (elt (elt *distance* i) num)))
	      ; Blank is not a real tile
	      (if (/= num 0)
		  (progn
		    (when (is-linear-conflict state i)
		      ; In 8 puzzle, linear-conflict can only be
		      ; manhattandistance + 2. Because there is
		      ; only one situtaion: like (2 0 1 ...)
		      (setf dst (+ dst 2)))
		    (setf total-dst (+ total-dst dst))
		    ))
	      ))
    total-dst))

(defun is-linear-conflict (state index)
  "Check if state(index) and state(state(index)) is linear conflict."
  (let* ((num (elt state index))
	 (num_goal (elt state num))
	 ; if num(pos:index) and num_goal(pos:num) are in the same line ?
	 (index_conflict (elt (elt *conflict* num) index))
	 ; if num's goal pos(num) and num_goal's goal pos(num_goal) are in
	 ; the same line ?
	 (goal_conflict (elt (elt *conflict* num) num_goal))
	 )
    (if (/= index_conflict 0)
	; all in the same line
	(if (= goal_conflict index_conflict)
	    ; (1 2 0 ...) is not linear conflict
	    (if (and
		 ; index is to the right of num
		 (> index num)
		 ; goal position of index is to the left
		 ; of the goal position of num
		 (< num num_goal))
		; if two tiles are adjacent, then it's not linear conflict
		; like (1 0 ...)
		(let* ((adj-entry (elt *adj* num))
		       (adj-num (elt *adj-num* num)))
		  (loop for i from 1 to adj-num
		     do (when (= (elt adj-entry i) num_goal)
			  (return-from is-linear-conflict nil)))
		  T)
		nil)
	    nil)
	nil)
    ))

; *** Helper Function ***

; Have to init gap-loc and h-val for every new node, so
; write a new function.
(defun init-node (heurf puzzle-state)
  (let* ((zloc (find-gap puzzle-state))
	 (node (make-node :state (copy-list puzzle-state) :gap-loc zloc)))
    (setf (node-h-val node) (funcall heurf (node-state node)))
    node))

; At last, it  will correctly calculate the heuristic value
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

; goal test
(defun is-goal-state (puzzle)
  (equal puzzle '(0 1 2 3 4 5 6 7 8)))

; same state test
(defun is-same-state (s1 s2)
  (equal s1 s2))

; find the location of zero, or gap, or space.
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