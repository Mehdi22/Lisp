; Computer Architecture Homework 3
; Problem 5 - fluidanimate

; A: L1 cache
(defvar *L1*
  '(0		;  1) 8k, 32 byte, 1 way
    5		;  2) 8k, 32 byte, 2 way
    15		;  3) 8k, 32 byte, 4 way
    10		;  4) 16k, 32 byte, 1 way
    15		;  5) 16k, 32 byte, 2 way
    25		;  6) 16k, 32 byte, 4 way
    13		;  7) 16k, 64 byte, 1 way
    18		;  8) 16k, 64 byte, 2 way
    29		;  9) 16k, 64 byte, 4 way
    30		; 10) 32k, 32 byte, 2 way
    50		; 11) 32k, 32 byte, 4 way
    34		; 12) 32k, 64 byte, 2 way
    55))	; 13) 32k, 64 byte, 4 way

; B: DL1 cache
(defvar *DL1*
  '(0		;  1) 8k, 32 byte, 1 way
    5		;  2) 8k, 32 byte, 2 way
    12		;  3) 8k, 32 byte, 4 way
    10		;  4) 16k, 32 byte, 1 way
    14		;  5) 16k, 32 byte, 2 way
    22		;  6) 16k, 32 byte, 4 way
    13		;  7) 16k, 64 byte, 1 way
    17		;  8) 16k, 64 byte, 2 way
    26		;  9) 16k, 64 byte, 4 way
    28		; 10) 32k, 32 byte, 2 way
    45		; 11) 32k, 32 byte, 4 way
    32		; 12) 32k, 64 byte, 2 way
    49))	; 13) 32k, 64 byte, 4 way

; C: L2 cache
(defvar *L2*
  '(0		;  1) 64k, 64 byte, 2 way
    11		;  2) 64k, 64 byte, 4 way
    3		;  3) 64k, 128 byte, 2 way
    16		;  4) 64k, 128 byte, 4 way
    10		;  5) 128k, 64 byte, 2 way
    21		;  6) 128k, 64 byte, 4 way
    14		;  7) 128k, 128 byte, 4 way
    27		;  8) 128k, 128 byte, 4 way
    30		;  9) 256k, 128 byte, 2 way
    55		; 10) 256k, 128 byte, 4 way
    75))	; 11) 256k, 128 byte, 8 way

; D: integer ALU
(defvar *iALU*
  '(0		; 1) 1 integer ALU
    8		; 2) 2 integer ALUs
    18		; 3) 3 integer ALUs
    30		; 4) 4 integer ALUs
    45		; 5) 5 integer ALUs
    60))	; 6) 6 integer ALUs

; E: integer MULT
(defvar *iMULT*
  '(0		; 1) 1 integer MULT
    12		; 2) 2 integer MULTs
    30		; 3) 3 integer MULTs
    55		; 4) 4 integer MULTs
    70		; 5) 5 integer MULTs
    100))	; 6) 6 integer MULTs

; F: integer DIV
(defvar *iDIV*
  '(0		; 1) 1 integer DIV
    13		; 2) 2 integer DIVs
    32		; 3) 3 integer DIVs
    59		; 4) 4 integer DIVs
    74		; 5) 5 integer DIVs
    110))	; 6) 6 integer DIVs

; G: floating-point ALU
(defvar *fALU*
  '(0		; 1) 1 floating-point ALU
    9		; 2) 2 floating-point ALUs
    19		; 3) 3 floating-point ALUs
    31		; 4) 4 floating-point ALUs
    48		; 5) 5 floating-point ALUs
    62))	; 6) 6 floating-point ALUs

; H: floating-point MULT
(defvar *fMULT*
  '(0		; 1) 1 floating-point MULT
    15		; 2) 2 floating-point MULTs
    33		; 3) 3 floating-point MULTs
    56		; 4) 4 floating-point MULTs
    78		; 5) 5 floating-point MULTs
    112))	; 6) 6 floating-point MULTs

; I: floating-point DIV
(defvar *fDIV*
  '(0		; 1) 1 floating-point DIV
    16		; 2) 2 floating-point DIVs
    33		; 3) 3 floating-point DIVs
    55		; 4) 4 floating-point DIVs
    76		; 5) 5 floating-point DIVs
    110))	; 6) 6 floating-point DIVs

; J: load/store queue
(defvar *lsQueue*
  '(0		; 1) depth 1
    9		; 2) depth 2
    19		; 3) depth 3
    30		; 4) depth 4
    53		; 5) depth 5
    82))	; 6) depth 6

; K: memory ports
(defvar *memPort*
  '(0		; 1) 1 memory port
    5		; 2) 2 memory ports
    14		; 3) 3 memory ports
    28		; 4) 4 memory ports
    50		; 5) 5 memory ports
    75))	; 6) 6 memory ports

; L: branch prediction scheme
(defvar *bpScheme*
  '(0		; 1) "not-taken" branch predictor
    0		; 2) "taken" branch predictor
    4		; 3) 512 bits, 1-bit dynamic branch predictor
    10		; 4) 1024 bits, 1-bit dynamic branch predictor
    18		; 5) 2048 bits, 1-bit dynamic branch predictor
    15		; 6) 1024 bits, 2-bit dynamic branch predictor
    21		; 7) 2048 bits, 2-bit dynamic branch predictor
    33))	; 8) 4096 bits, 2-bit dynamic branch predictor

; M: clock frequency
(defvar *cf*
  '(0		; 1) 100 MHz
    5		; 2) 110 MHz
    15		; 3) 120 MHz
    25		; 4) 130 MHz
    31		; 5) 140 MHz
    57))	; 6) 150 MHz

(defvar *num*
  ; A  B  C  D E F G H I J K L M
  '(13 13 11 6 6 6 6 6 6 6 6 8 6))

(defvar *total-option* 0)

(defun fluid (min max)
  (let ((option '(0 0 0 0 0 0 0 0 0 0 0 0 0))
	(cost   '(0 0 0 0 0 0 0 0 0 0 0 0 0 0))	; last one: total cost
	)
    (setf *total-option* 0)
    (choose-L1 option cost min max)
    *total-option*))

; A: 1 -> index = 0
(defun choose-L1 (option cost min max)
  (let* ((index 0)
	 (choice-num (1- (elt *num* index))))
    (loop for i from 0 to choice-num
	 do (let* ((option_next (copy-list option))
		   (cost_next (copy-list cost))
		   (choice-cost (elt *L1* i)))
	      (setf (elt option_next index) (1+ i))
	      (setf (elt cost_next index) choice-cost)
	      (setf (elt cost_next 13) (+ choice-cost (elt cost_next 13)))
	      (if (<= (elt cost_next 13) max)
		  (progn
		    (when (>= (elt cost_next 13) min)
		      (format t "option: ~a cost: ~a~%"
			      option_next cost_next)
		      (setf *total-option* (1+ *total-option*)))
		      ; call next option
		      (choose-DL1 option_next cost_next min max))
		  ; cost > max
		  nil)
	      ))
    ))

; B: 2 -> index = 1
(defun choose-DL1 (option cost min max)
  (let* ((index 1)
	 (choice-num (1- (elt *num* index))))
    (loop for i from 0 to choice-num
	 do (let* ((option_next (copy-list option))
		   (cost_next (copy-list cost))
		   (choice-cost (elt *DL1* i)))
	      (setf (elt option_next index) (1+ i))
	      (setf (elt cost_next index) choice-cost)
	      (setf (elt cost_next 13) (+ choice-cost (elt cost_next 13)))
	      (if (<= (elt cost_next 13) max)
		  (progn
		    (when (>= (elt cost_next 13) min)
		      ; a promising option
		      (format t "option: ~a cost: ~a~%"
			      option_next cost_next)
		      (setf *total-option* (1+ *total-option*)))
		      ; call next config
		      (choose-L2 option_next cost_next min max))
		  ; cost > max
		  nil)
	      ))
    ))

; C: 3 -> index = 2
(defun choose-L2 (option cost min max)
  (let* ((index 2)
	 (choice-num (1- (elt *num* index))))
    (loop for i from 0 to choice-num
	 do (let* ((option_next (copy-list option))
		   (cost_next (copy-list cost))
		   (choice-cost (elt *L2* i)))
	      (setf (elt option_next index) (1+ i))
	      (setf (elt cost_next index) choice-cost)
	      (setf (elt cost_next 13) (+ choice-cost (elt cost_next 13)))
	      (if (<= (elt cost_next 13) max)
		  (progn
		    (when (>= (elt cost_next 13) min)
		      ; a promising option
		      (format t "option: ~a cost: ~a~%"
			      option_next cost_next)
		      (setf *total-option* (1+ *total-option*)))
		      ; call next config
		      (choose-iALU option_next cost_next min max)
		    )
		  ; cost > max
		  nil)
	      ))
    ))

; D: 4 -> index = 3
(defun choose-iALU (option cost min max)
  (let* ((index 3)
	 (choice-num (1- (elt *num* index))))
    (loop for i from 0 to choice-num
	 do (let* ((option_next (copy-list option))
		   (cost_next (copy-list cost))
		   (choice-cost (elt *iALU* i)))
	      (setf (elt option_next index) (1+ i))
	      (setf (elt cost_next index) choice-cost)
	      (setf (elt cost_next 13) (+ choice-cost (elt cost_next 13)))
	      (if (<= (elt cost_next 13) max)
		  (progn
		    (when (>= (elt cost_next 13) min)
		      ; a promising option
		      (format t "option: ~a cost: ~a~%"
			      option_next cost_next)
		      (setf *total-option* (1+ *total-option*)))
		      ; call next config
		      (choose-iMULT option_next cost_next min max)
		    )
		  ; cost > max
		  nil)
	      ))
    ))

; E: 5 -> index = 4
(defun choose-iMULT (option cost min max)
  (let* ((index 4)
	 (choice-num (1- (elt *num* index))))
    (loop for i from 0 to choice-num
	 do (let* ((option_next (copy-list option))
		   (cost_next (copy-list cost))
		   (choice-cost (elt *iMULT* i)))
	      (setf (elt option_next index) (1+ i))
	      (setf (elt cost_next index) choice-cost)
	      (setf (elt cost_next 13) (+ choice-cost (elt cost_next 13)))
	      (if (<= (elt cost_next 13) max)
		  (progn
		    (when (>= (elt cost_next 13) min)
		      ; a promising option
		      (format t "option: ~a cost: ~a~%"
			      option_next cost_next)
		      (setf *total-option* (1+ *total-option*)))
		      ; call next config
		      (choose-iDIV option_next cost_next min max)
		    )
		  ; cost > max
		  nil)
	      ))
    ))

; F: 6 -> index = 5
(defun choose-iDIV (option cost min max)
  (let* ((index 5)
	 (choice-num (1- (elt *num* index))))
    (loop for i from 0 to choice-num
	 do (let* ((option_next (copy-list option))
		   (cost_next (copy-list cost))
		   (choice-cost (elt *iDIV* i)))
	      (setf (elt option_next index) (1+ i))
	      (setf (elt cost_next index) choice-cost)
	      (setf (elt cost_next 13) (+ choice-cost (elt cost_next 13)))
	      (if (<= (elt cost_next 13) max)
		  (progn
		    (when (>= (elt cost_next 13) min)
		      ; a promising option
		      (format t "option: ~a cost: ~a~%"
			      option_next cost_next)
		      (setf *total-option* (1+ *total-option*)))
		      ; call next config
		      (choose-fALU option_next cost_next min max)
		    )
		  ; cost > max
		  nil)
	      ))
    ))

; G: 7 -> index = 6
(defun choose-fALU (option cost min max)
  (let* ((index 6)
	 (choice-num (1- (elt *num* index))))
    (loop for i from 0 to choice-num
	 do (let* ((option_next (copy-list option))
		   (cost_next (copy-list cost))
		   (choice-cost (elt *fALU* i)))
	      (setf (elt option_next index) (1+ i))
	      (setf (elt cost_next index) choice-cost)
	      (setf (elt cost_next 13) (+ choice-cost (elt cost_next 13)))
	      (if (<= (elt cost_next 13) max)
		  (progn
		    (when (>= (elt cost_next 13) min)
		      ; a promising option
		      (format t "option: ~a cost: ~a~%"
			      option_next cost_next)
		      (setf *total-option* (1+ *total-option*)))
		      ; call next config
		      (choose-fMULT option_next cost_next min max)
		    )
		  ; cost > max
		  nil)
	      ))
    ))

; H: 8 -> index = 7
(defun choose-fMULT (option cost min max)
  (let* ((index 7)
	 (choice-num (1- (elt *num* index))))
    (loop for i from 0 to choice-num
	 do (let* ((option_next (copy-list option))
		   (cost_next (copy-list cost))
		   (choice-cost (elt *fMULT* i)))
	      (setf (elt option_next index) (1+ i))
	      (setf (elt cost_next index) choice-cost)
	      (setf (elt cost_next 13) (+ choice-cost (elt cost_next 13)))
	      (if (<= (elt cost_next 13) max)
		  (progn
		    (when (>= (elt cost_next 13) min)
		      ; a promising option
		      (format t "option: ~a cost: ~a~%"
			      option_next cost_next)
		      (setf *total-option* (1+ *total-option*)))
		      ; call next config
		      (choose-fDIV option_next cost_next min max)
		    )
		  ; cost > max
		  nil)
	      ))
    ))

; I: 9 -> index = 8
(defun choose-fDIV (option cost min max)
  (let* ((index 8)
	 (choice-num (1- (elt *num* index))))
    (loop for i from 0 to choice-num
	 do (let* ((option_next (copy-list option))
		   (cost_next (copy-list cost))
		   (choice-cost (elt *fDIV* i)))
	      (setf (elt option_next index) (1+ i))
	      (setf (elt cost_next index) choice-cost)
	      (setf (elt cost_next 13) (+ choice-cost (elt cost_next 13)))
	      (if (<= (elt cost_next 13) max)
		  (progn
		    (when (>= (elt cost_next 13) min)
		      ; a promising option
		      (format t "option: ~a cost: ~a~%"
			      option_next cost_next)
		      (setf *total-option* (1+ *total-option*)))
		      ; call next config
		      (choose-lsQueue option_next cost_next min max)
		    )
		  ; cost > max
		  nil)
	      ))
    ))

; J: 10 -> index = 9
(defun choose-lsQueue (option cost min max)
  (let* ((index 9)
	 (choice-num (1- (elt *num* index))))
    (loop for i from 0 to choice-num
	 do (let* ((option_next (copy-list option))
		   (cost_next (copy-list cost))
		   (choice-cost (elt *lsQueue* i)))
	      (setf (elt option_next index) (1+ i))
	      (setf (elt cost_next index) choice-cost)
	      (setf (elt cost_next 13) (+ choice-cost (elt cost_next 13)))
	      (if (<= (elt cost_next 13) max)
		  (progn
		    (when (>= (elt cost_next 13) min)
		      ; a promising option
		      (format t "option: ~a cost: ~a~%"
			      option_next cost_next)
		      (setf *total-option* (1+ *total-option*)))
		      ; call next config
		      (choose-memPort option_next cost_next min max)
		    )
		  ; cost > max
		  nil)
	      ))
    ))

; K: 11 -> index = 10
(defun choose-memPort (option cost min max)
  (let* ((index 10)
	 (choice-num (1- (elt *num* index))))
    (loop for i from 0 to choice-num
	 do (let* ((option_next (copy-list option))
		   (cost_next (copy-list cost))
		   (choice-cost (elt *memPort* i)))
	      (setf (elt option_next index) (1+ i))
	      (setf (elt cost_next index) choice-cost)
	      (setf (elt cost_next 13) (+ choice-cost (elt cost_next 13)))
	      (if (<= (elt cost_next 13) max)
		  (progn
		    (when (>= (elt cost_next 13) min)
		      ; a promising option
		      (format t "option: ~a cost: ~a~%"
			      option_next cost_next)
		      (setf *total-option* (1+ *total-option*)))
		      ; call next config
		      (choose-bpScheme option_next cost_next min max)
		    )
		  ; cost > max
		  nil)
	      ))
    ))

; L: 12 -> index = 11
(defun choose-bpScheme (option cost min max)
  (let* ((index 11)
	 (choice-num (1- (elt *num* index))))
    (loop for i from 0 to choice-num
	 do (let* ((option_next (copy-list option))
		   (cost_next (copy-list cost))
		   (choice-cost (elt *bpScheme* i)))
	      (setf (elt option_next index) (1+ i))
	      (setf (elt cost_next index) choice-cost)
	      (setf (elt cost_next 13) (+ choice-cost (elt cost_next 13)))
	      (if (<= (elt cost_next 13) max)
		  (progn
		    (when (>= (elt cost_next 13) min)
		      ; a promising option
		      (format t "option: ~a cost: ~a~%"
			      option_next cost_next)
		      (setf *total-option* (1+ *total-option*)))
		      ; call next config
		      (choose-cf option_next cost_next min max)
		    )
		  ; cost > max
		  nil)
	      ))
    ))

; M: 13 -> index = 12
(defun choose-cf (option cost min max)
  (let* ((index 12)
	 (choice-num (1- (elt *num* index))))
    (loop for i from 0 to choice-num
	 do (let* ((option_next (copy-list option))
		   (cost_next (copy-list cost))
		   (choice-cost (elt *cf* i)))
	      (setf (elt option_next index) (1+ i))
	      (setf (elt cost_next index) choice-cost)
	      (setf (elt cost_next 13) (+ choice-cost (elt cost_next 13)))
	      (if (<= (elt cost_next 13) max)
		  (progn
		    (when (>= (elt cost_next 13) min)
		      ; a promising option
		      (format t "option: ~a cost: ~a~%"
			      option_next cost_next)
		      (setf *total-option* (1+ *total-option*)))
		      ; end..
		      nil
		    )
		  ; cost > max
		  nil)
	      ))
    ))