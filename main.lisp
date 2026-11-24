;;Copyright 2026 (c) Gabriel Hill <cynth@cynthion21x.co.uk>
;;
;;Permission is hereby granted, free of charge, to any person obtaining a copy of this
;;software and associated documentation files (the “Software”), to deal in the Software
;;without restriction, including without limitation the rights to use, copy, modify,
;;merge, publish, distribute, sublicense, and/or sell copies of the Software, and to
;;permit persons to whom the Software is furnished to do so,
;;
;;subject to the following conditions:
;;
;;The above copyright notice and this permission notice shall be included in all
;;copies or substantial portions of the Software.
;;
;;THE SOFTWARE IS PROVIDED “AS IS”, WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED,
;;INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A
;;PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
;;HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
;;OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
;;SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

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
  (append (reverse (tape-left ta))
	  (list ">") (list (tape-head ta)) (list "<")
	  (tape-right ta)))

(defun print-tape-nohead (ta) ; without the > < pointing to the head
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

(defun 0-rule (rule head) ; gets the instructions based on the head (0 or 1)
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

(defun parse-string (str &optional (state 0))
  (let ((first (position #\_ str)))
    (if first
	(append
	 (parse-chunk (subseq str 0 first))
	 (parse-string (subseq str (1+ first) (length str))))
	(parse-chunk str))))

(defun parse-chunk (str state)
  )

;; Testing
;;1RB1LB_1LC0RA_---1LB
(defparameter the-instructions '((A (1 :right B) (1 :left B))
			   (B (1 :left C) (0 :right A))
			   (C :halt (1 :left B))))

(defparameter the-tape (make-tape
		  :head 0))

(defparameter the-machine (make-machine
		     :state 'A
		     :tape the-tape
		     :instructions the-instructions))
