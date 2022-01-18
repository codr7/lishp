(defpackage lishp
  (:use cl)
  (:export *results* *version*
	   eval-line
	   say start))

(in-package lishp)

(define-symbol-macro *version* 1)
(defparameter *results* (make-array 0 :adjustable t :fill-pointer 0))
(defvar *out*)

(defmethod eval-line (in)
  (let* ((fn (pop in))
	 (out (apply fn in)))
    (vector-push-extend out *results*)))

(defun say (spec &rest args)
  (apply #'format *out* spec args)
  (terpri *out*))

(defun start (&key (in *standard-input*) (out *standard-output*))
  (let ((*out* out))
    (format out "lishp v~a~%may the source be with you~%~%" *version*)
    (labels ((rec-line ()
	       (format out "$~a " (first (array-dimensions *results*)))
	       (let ((line (read-line in nil)))
		 (when line
		   (with-input-from-string (in line)
		     (labels ((rec-form (out)
				(let ((form (read in nil)))
				  (if form
				      (rec-form (cons form out))
				      (nreverse out)))))
		       (eval-line (rec-form nil))))
		   (rec-line)))))
      (rec-line))))

