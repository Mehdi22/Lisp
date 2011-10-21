; The Queue datatype

(defstruct q
  (enqueue #'enqueue-FIFO)
  (key #'identity)
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
  "Inserts the items into the queue, according to the queue's enqueue
  function."
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
  ; If first insert, create the heap
  (when (null (q-elements q))
    (setf (q-elements q) (make-heap)))
  ; Now insert the items
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
  "Bubbles up from i to find position for val, moving items down in the
  process"
  (cond
    ((or (zerop i) (< (heap-val heap (heap-parent i) key) val))
     i)
    (t
     (setf (elt heap i) (elt heap (heap-parent i)))
     (heap-find-pos heap (heap-parent i) val key))
    ))

(defun make-heap (&optional (size 100))
  (make-array size :fill-pointer 0 :adjustable t))

(provide :queue)
