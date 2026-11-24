;;; Tape Implementation

(defstruct tape
  head
  left
  right)

(defun tape-write (ta symbol)
  (make-tape
   :head symbol
   :left (tape-left ta)
   :right (tape-right ta)))

(defun move-left (ta)
  (make-tape
   :head (if (tape-left ta) (car (tape-left ta)) 0)
   :left (cdr (tape-left ta))
   :right (cons (tape-head ta) (tape-right ta))))

(defun move-right (ta)
  (make-tape
   :head  (if (tape-right ta) (car (tape-right ta)) 0)
   :left  (cons (tape-head ta) (tape-left ta))
   :right (cdr (tape-right ta))))

(defun print-tape (ta)
  (append (reverse (tape-left ta)) (list ">") (list (tape-head ta)) (list "<") (tape-right ta)))

(defun print-tape-nohead (ta)
  (append (reverse (tape-left ta)) (list (tape-head ta)) (tape-right ta)))

;;; Machine

(defstruct machine
  state
  tape
  instructions)

;; (tape state)
(defun apply-rule (ta rule)
  (if (eq rule :halt)
      (cons ta :halt)
      (let ((v (tape-write ta (car rule))))
	(cons
	 (if (eq (car (cdr rule)) :right) (move-right v) (move-left v))
	 (car (cdr (cdr rule)))))))

(defun get-rule (rules state)
  (if (eq state :halt)
      :halt
      (let ((curr (car rules)))
	(cond
	  ((eq (car curr) state) curr)
	  (t (get-rule (cdr rules) state))))))

(defun 0-rule (rule head)
  (if (eq rule :halt)
      :halt
      (if (eq head 0)
	  (car (cdr rule))
	  (car (cdr (cdr rule))))))

(defun update-machine (ma)
  (let ((ans
	  (let ((rules (machine-instructions ma))
		(ta (machine-tape ma))
		(state (machine-state ma)))
	    (apply-rule ta (0-rule (get-rule rules state) (tape-head ta))))))
    (make-machine
     :state (cdr ans)
     :tape (car ans)
     :instructions (machine-instructions ma))))

;;; Program

;; program (state rule-0 rule-1)
;; rule (value dir next-state) or :halt -halt
(defparameter the-instructions '((A (1 :right B) (1 :left B))
			   (B (1 :left C) (0 :right A))
			   (C :halt (1 :left B))))

(defparameter the-tape (make-tape
		  :head 0))

(defparameter the-machine (make-machine
		     :state 'A
		     :tape the-tape
		     :instructions the-instructions))
