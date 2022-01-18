(defpackage lishp
  (:use cl)
  (:export *results* *version*
	   eval-line
	   format-result
	   get-path get-symbols
	   ls
	   package! print-result
	   say start))

(in-package lishp)

(define-symbol-macro *version* 1)
(defparameter *results* (make-array 0 :fill-pointer 0))
(defvar *out*)
(defvar *path* nil)

(defmethod eval-line (in)
  (let ((fn (pop in)))
    (when fn
      (let ((out (apply fn in)))
	(vector-push-extend out *results*)
	out))))

(defmethod format-result (in)
  (with-output-to-string (out)
    (princ in out)))

(defmethod print-result (in)
  (format *out* "~a~%" (format-result in)))

(defmethod print-result ((in null)))

(defun get-path ()
  (if *path*
      (with-output-to-string (out)
	(dolist (p (reverse *path*))
	  (format out "~a>" (package-name p))))
      ">"))

(defun package! (&key (name (get-path)))
  (or (find-package name) (make-package name)))

(defun get-symbols ()
  (let (out)
    (do-symbols (s (package!))
      (push s out))
    (nreverse out)))

(defun ls (&rest args)
  (declare (ignore args))
  (format *out* "Contents of ~a:~%" (get-path))
  (get-symbols))

(defun say (spec &rest args)
  (apply #'format *out* spec args)
  (terpri *out*))

(defun start (&key (in *standard-input*) (out *standard-output*) (package 'lishp))
  (let ((*out* out)
	(*package* (find-package package)))
    (unless (eq *package* (find-package 'lishp))
      (use-package 'lishp))
    (format out "lishp v~a,~%may the source be with you!~%~%" *version*)
    (labels ((rec-line ()
	       (format out "$~a " (length *results*))
	       (let ((line (read-line in nil)))
		 (cond
		   ((string= line "")
		    (format out "~a~%" (get-path))
		    (rec-line))
		   ((string= line "q")
		    (format out "arrivederci!~%")
		    (return-from start))
		   (t
		    (with-input-from-string (in line)
		      (labels ((rec-form (out)
				 (let ((form (read in nil)))
				   (if form
				       (rec-form (cons form out))
				       (nreverse out)))))
			(print-result (eval-line (rec-form nil))))
		      (rec-line)))))))
      (rec-line))))

