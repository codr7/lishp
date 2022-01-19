(defpackage lishp
  (:use cl)
  (:export *results* *version*
	   cd
	   eval-line
	   format-result
	   get-path get-symbols
	   ls
	   md
	   print-result
	   say start))

(in-package lishp)

(define-symbol-macro *version* 1)

(defparameter *results* (make-array 0 :fill-pointer 0))
(defparameter *path* (list 'lishp))
(defvar *out*)

(defmethod eval-line (in)
  (let* ((fn (pop in)))
    (when fn
      (let* ((out (apply fn in)))
	(vector-push-extend out *results*)
	out))))

(defmethod format-result (in)
  (with-output-to-string (out)
    (princ in out)))

(defmethod format-result ((in package))
  (package-name in))

(defmethod format-result ((in symbol))
  (string-downcase (symbol-name in)))

(defmethod print-result (in)
  (format *out* "~a~%" (format-result in)))

(defmethod print-result ((in null)))

(defmethod print-result ((in list))
  (dolist (r in)
    (print-result r)))

(defun get-path ()
  (if *path*
      (with-output-to-string (out)
	(dolist (p (reverse *path*))
	  (format out "~a>" (string-downcase (symbol-name p)))))
      ">"))

(defun directory-package (&key (name (get-path)))
  (or (find-package name)
      (error "Directory not found: ~a" name)))

(defun get-symbols ()
  (let* (out)
    (do-symbols (s (directory-package))
      (push s out))
    (nreverse out)))

(defun ls (&rest args)
  (declare (ignore args))
  (format *out* "Contents of ~a:~%" (get-path))
  (mapcar (lambda (s)
	    (let ((v (symbol-value s)))
	      (if (typep v 'package) s v)))
	  (stable-sort (get-symbols)
		       (lambda (x y)
			 (string< (symbol-name x) (symbol-name y))))))

(defun say (spec &rest args)
  (apply #'format *out* spec args)
  (terpri *out*))

(defun md (&rest dirs)
  (dolist (d dirs)
    (let* ((*path* (cons d *path*))
	   (p (get-path)))
      (unless (find-package p)
	(let* ((*path* (rest *path*))
	       (*package* (directory-package))
	       (d (format nil "~a>" d)))
	  (setf (symbol-value (intern d)) (make-package p))))))
  nil)
  
(defun cd (dir &optional (create nil))
  (when (char= (char (symbol-name dir) 0) #\>)
    (setf *path* (list 'lishp))
    (setf dir (intern (subseq (symbol-name dir) 1))))
  
  (when create
    (md dir))

  (cond
    ((string= dir "<")
     (when (rest *path*)
       (pop *path*)))
    (t
     (let* ((*path* (cons dir *path*))
	   (p (get-path)))
       (unless (find-package p)
	 (error "Unknown directory: ~a" p)))
     
     (push dir *path*)))
    
  (get-path))

(defun start (&key (in *standard-input*) (out *standard-output*) (package 'lishp))
  (let* ((*out* out)
	(*package* (find-package package)))
    (unless (eq *package* (find-package 'lishp))
      (use-package 'lishp))
    (unless (find-package "lishp>")
      (setf (symbol-value 'lishp>) (make-package "lishp>")))
    (format out "lishp v~a,~%may the source be with you!~%~%" *version*)
    (labels ((rec-line ()
	       (format out "$~a " (length *results*))
	       (let* ((line (read-line in nil)))
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
				 (let* ((form (read in nil)))
				   (if form
				       (rec-form (cons form out))
				       (nreverse out)))))
			(print-result (eval-line (rec-form nil))))
		      (rec-line)))))))
      (rec-line))))

