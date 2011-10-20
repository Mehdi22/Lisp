; The 8-Puzzle Problem
;
; Author: Mengqi Zong
;
; Email: mz2326@columbia.edu
;
; Variable definition

(defvar *state* '(0 1 2 3 4 5 6 7 8))

(defvar *adj*
  '((0 1 3)
    (1 0 2 4)
    (2 1 5)
    (3 0 4 6)
    (4 1 3 5 7)
    (5 2 4 8)
    (6 3 7)
    (7 4 6 8)
    (8 5 7)))

(defvar *adj-num*
  '(2 3 2 3 4 3 2 3 2))

(defstruct node
  (state nil)
  (gap-loc nil)	; the location of 0, the space
  (cost 0)	; g(n), the path cost
  (h-val 0)	; h(n), the heuristic value of current state
  (parent nil))	; parent node

(defun solve-puzzle (&optional (heurf #'misplaced-tiles)
		     (initial-state *state*) (step 100))
  "Given a state, use this state to create a node, then create a new
   priority queue. Then call function A-star to start searching."
  (let* ((start (init-node heurf initial-state))
	 (fringe (make-q)))
    (random-init-node heurf start step)
    (format t "Initial state: ~a~%" (node-state start))
    (q-insert fringe (list start))
    (format t "processing...~%")
    (A-star fringe heurf)
    ))

(defun A-star (fringe heurf)
  "Given a priority queue, pop a node to check if it is goal node.
   If not, insert the expanded nodes to the queue and call A-star
   again. Nothing special, the essence is in function expand."
  (when fringe
    (let ((node (q-front fringe)))
      (q-remove fringe)
      (if (is-goal-state (node-state node))
	  ; do something! Goal state found!
	  (generate-solution node)
	  (A-star (q-insert fringe (expand heurf node)) heurf))
      )))

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
  (if  puzzle
       (cons (node-state puzzle)
	     (action-sequence (node-parent puzzle) (1+ step)))
       (format t "step: ~a~%" step)
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

(defun reset-state ()
  (setf *state* '(0 1 2 3 4 5 6 7 8)))

(defun is-goal-state (puzzle)
  (equal puzzle '(0 1 2 3 4 5 6 7 8)))

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


; The Priority Queue Implementation

; The Queue datatype

(defstruct q
  (enqueue #'enqueue-priority)
  (key #'fn-a*)
  (last nil)
  (elements nil))

; Operations on Queues

(defun q-emptyp (q)
  "Returns T if queue is empty."
  (= (length (q-elements q)) 0))

(defun q-front (q)
  "Returns the element at the front of the queue."
  (elt (q-elements q) 0))

(defun q-remove (q)
  "Removes the element from the front of the queue and returns it."
  (if (listp (q-elements q))
	     (pop (q-elements q))
	     (heap-pop (q-elements q) (q-key q))))

(defun q-insert (q items)
  "Inserts the items into the queue, according to the queue's enqueue function"
  (funcall (q-enqueue q) q items)
  q)

; The Three Enqueuing Functions

(defun enqueue-LIFO (q items)
  "Adds a list of items to the front of the queue."
  (setf (q-elements q) (nconc items (q-elements q)))
  items)

(defun enqueue-FIFO (q items)
  "Adds a list of items to the end of the queue."
  (if (q-emptyp q)
      (setf (q-elements q) items)
      (setf (cdr (q-last q)) items))
      (setf (q-last q) (last items))
      items)

(defun enqueue-priority (q items)
  "Inserts the items by priority determined by the queue's key function."
  ;; If first insert, create the heap
  (when (null (q-elements q))
    (setf (q-elements q) (make-heap)))
  ;; Now insert the items
  (mapc (lambda (item)
	  (heap-insert (q-elements q) item (q-key q)))
	items)
  )

; The Heap Impelentation of Priority Queues

; The idea is to store a heap in an array so that the heap property is
; maintained for all elements: heap[Parent(i)] <= heap[i]. Note that we
; start at index 0, not 1, and that we put the lowest vaule at the top of
; the heap, not the highest value.

(defun heap-val (heap i key) (funcall key (elt heap i)))
(defun heap-parent (i) (floor (1- i) 2))
(defun heap-left (i) (+ 1 i i))
(defun heap-right (i) (+ 2 i i))
(defun heap-leafp (heap i) (> i (1- (floor (length heap) 2))))

(defun heapify (heap i key)
  "Assume that the children of i are heaps, but that heap[i] may be
  larger than its children. If it is, move heap[i] down where it belongs."
  (unless (heap-leafp heap i)
    (let ((l (heap-left i))
	  (r (heap-right i)))
      (let ((smaller-child (if (and (< r (length heap))
				    (< (heap-val heap r key)
				       (heap-val heap l key)))
			       r l)))
	(when (> (heap-val heap i key) (heap-val heap smaller-child key))
	  (rotatef (elt heap i) (elt heap smaller-child))
	  (heapify heap smaller-child key))))
    ))

(defun heap-pop (heap key)
  "Pops the best (lowest valued) item off the heap"
  (let ((min (elt heap 0)))
    (setf (elt heap 0) (elt heap (1- (length heap))))
    (decf (fill-pointer heap))
    (heapify heap 0 key)
    min))

(defun heap-insert (heap item key)
  "Puts an item into a heap."
  (vector-push-extend nil heap)
  (setf
   (elt heap (heap-find-pos heap (1- (length heap)) (funcall key item) key))
   item)
  )

(defun heap-find-pos (heap i val key)
  "Bubbles up from i to find position for val, moving items down in the process"
  (cond
    ((or (zerop i) (< (heap-val heap (heap-parent i) key) val))
     i)
    (t
     (setf (elt heap i) (elt heap (heap-parent i)))
     (heap-find-pos heap (heap-parent i) val key))
    ))

(defun make-heap (&optional (size 100))
  (make-array size :fill-pointer 0 :adjustable t))
