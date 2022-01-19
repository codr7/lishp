(defpackage lishp
  (:use cl)
  (:export *results* *version*
	   cd
	   eval-line
	   format-result
	   get-dir-keys get-path
	   ls
	   md
	   print-result
	   say start))

(in-package lishp)

(define-symbol-macro *version* 1)

(defparameter *debug* nil)
(defparameter *results* (make-array 0 :fill-pointer 0))
(defparameter *path* nil)

(defvar *out*)

(defmacro dohash ((k v tbl) &body body)
  (let (($i (gensym)) ($k (gensym)) ($next (gensym)) ($ok (gensym)))
    `(with-hash-table-iterator (,$i ,tbl)
       (tagbody
	  ,$next
	  (multiple-value-bind (,$ok ,$k ,v) (,$i)
	    (declare (ignorable ,v))
	    (when ,$ok
	      (let ((,k ,$k))
		(declare (ignorable ,k))
		,@body
		(go ,$next))))))))

(defstruct dir
  (name (error "uissing name"))
  (entries (make-hash-table)))

(defparameter *root-dir* (make-dir :name ">"))
(defparameter *dir* (list *root-dir*))

(defun get-dir-keys (&key dir)
  (let (out)
    (dohash (k v (dir-entries (or dir (first *dir*))))
      (push k out))
    (nreverse out)))

(defun find-entry (key)
  (dolist (d *dir*)
    (let ((v (gethash key (dir-entries d))))
      (when v
	(return-from find-entry v))))
  
  (when (fboundp key)
    (fdefinition key)))

(defun get-path ()
  (if *path*
      (with-output-to-string (out)
	(dolist (p (reverse *path*))
	  (format out "~a>" (string-downcase (symbol-name p)))))
      ">"))

(defun get-entry (key)
  (or (find-entry key)
      (error "not found: ~a~a" (get-path) (string-downcase (symbol-name key)))))

(defmethod eval-line (in)
  (let* ((fn (pop in)))
    (when fn
      (let* ((out (apply (get-entry fn) in)))
	(vector-push-extend out *results*)
	out))))

(defmethod format-result (in)
  (with-output-to-string (out)
    (princ in out)))

(defmethod format-result ((in dir))
  (format nil "~a> (~a)" (dir-name in) (hash-table-count (dir-entries in))))

(defmethod format-result ((in symbol))
  (string-downcase (symbol-name in)))

(defmethod print-result (in)
  (format *out* "~a~%" (format-result in)))

(defmethod print-result ((in null)))

(defmethod print-result ((in list))
  (dolist (r in)
    (print-result r)))

(defun ls (&rest args)
  (declare (ignore args))
  (format *out* "Contents of ~a:~%" (get-path))
  (mapcar (lambda (k)
	    (gethash k (dir-entries (first *dir*))))
	  (stable-sort (get-dir-keys)
		       (lambda (x y)
			 (string< (symbol-name x) (symbol-name y))))))

(defun say (spec &rest args)
  (apply #'format *out* spec args)
  (terpri *out*))

(defun md (&rest dirs)
  (dolist (d dirs)
    (unless (gethash d (dir-entries (first *dir*)))
      (setf (gethash d (dir-entries (first *dir*))) (make-dir :name (string-downcase (symbol-name d))))))
  nil)

(defun cd (dir &optional (create nil))
  (let ((dp (symbol-name dir)))
    (when (char= (char dp 0) #\>)
      (setf *path* nil)
      (setf *dir* (list *root-dir*))
      (setf dp (subseq dp 1)))
    (when (zerop (length dp))
      (return-from cd ">"))
    (setf dir (intern dp)))
  
  (when create
    (md dir))

  (cond
    ((string= dir "<")
     (pop *dir*)
     (pop *path*))
    (t
     (let* ((entries (dir-entries (first *dir*))))
       (unless (gethash dir entries)
	 (error "not found: ~a" (string-downcase (symbol-name dir))))
       (push (gethash dir entries) *dir*))
     (push dir *path*)))
  
  (get-path))

(defun start (&key (in *standard-input*) (out *standard-output*))
  (let* ((*out* out))
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
			(handler-case
			    (print-result (eval-line (rec-form nil)))
			  (error (e)
			    (when *debug*
			      (error e))
			    (format t "~a~%" e))))
		      (rec-line)))))))
      (rec-line))))

