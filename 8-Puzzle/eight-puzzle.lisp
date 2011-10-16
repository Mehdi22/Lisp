;
; The 8-Puzzle Problem
;
; Author: Mengqi Zong
;
; Email: mz2326@columbia.edu
;
; symbol-name intern elt

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

(defvar *adj-num* '(2 3 2 3 4 3 2 3 2))

(defun reset-state ()
    (setf *state* '(0 1 2 3 4 5 6 7 8)))

(defun is-goal-state (puzzle)
  (equal puzzle '(0 1 2 3 4 5 6 7 8)))

(defun random-init-state (step)
  (cond
    ((equal step 0) *state*)
    (t (let* ((gap-loc (find-gap *state*))
	      (adj-entry (nth gap-loc *adj*))
	      (entry-num (nth gap-loc *adj-num*))
	      (move (nth (+ 1 (random entry-num)) adj-entry)))
	 (move-puzzle *state* gap-loc move)
	 (decf step)
	 (random-init-state step)))))

(defun find-gap (puzzle)
  (loop for i from 0 to 8
     do (if (equal (nth i puzzle) 0)
	    (return-from find-gap i)))
  (format t "ERROR: Gap not found in the puzzle state!~%")
  (return-from find-gap NIL))

(defun move-puzzle (puzzle i j)
  (let ((i_val (nth i puzzle)) (j_val (nth j puzzle)))
    (setf (nth i puzzle) j_val)
    (setf (nth j puzzle) i_val)))
