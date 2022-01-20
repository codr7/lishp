(defpackage lishp
  (:use cl)
  (:import-from sb-ext *posix-argv* save-lisp-and-die)
  
  (:export *version*
	   cd
	   eval-line
	   find-entry format-result
	   get-dir-keys get-entry get-path
	   ls
	   md
	   print-result
	   shell say start))

(in-package lishp)

(define-symbol-macro *version* 1)

(define-symbol-macro *debug* (slot-value *shell* 'debug?))
(define-symbol-macro *paths* (slot-value *shell* 'paths))
(define-symbol-macro *path* (first *paths*))
(define-symbol-macro *root* (slot-value *shell* 'root))
(define-symbol-macro *dirs* (slot-value *shell* 'dirs))
(define-symbol-macro *dir* (first *dirs*))
(define-symbol-macro *results* (slot-value *shell* 'results))

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
  (name (error "missing name"))
  (entries (make-hash-table)))

(defclass shell ()
  ((debug? :initform nil :reader debug?)
   (paths :initform nil :reader paths)
   (root :initform (make-dir :name ">") :reader root)
   (dirs :initform nil :reader dirs)
   (results :initform (make-array 0 :fill-pointer 0) :reader results)))

(defmethod initialize-instance :after ((self shell) &key)
  (with-slots (root dirs) self
    (push root dirs)))
  
(defparameter *shell* (make-instance 'shell))

(defun get-dir-keys (&key dir)
  (let (out)
    (dohash (k v (dir-entries (or dir *dir*)))
      (push k out))
    (nreverse out)))

(defun find-entry (key)
  (dolist (d *dirs*)
    (let ((v (gethash key (dir-entries d))))
      (when v
	(return-from find-entry v))))
  
  (when (fboundp key)
    (fdefinition key)))

(defun get-path ()
  (if *paths*
      (with-output-to-string (out)
	(dolist (p (reverse *paths*))
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
  (format *out* "contents of ~a:~%" (get-path))
  (mapcar (lambda (k)
	    (gethash k (dir-entries *dir*)))
	  (stable-sort (get-dir-keys)
		       (lambda (x y)
			 (string< (symbol-name x) (symbol-name y))))))

(defun say (spec &rest args)
  (apply #'format *out* spec args)
  (terpri *out*))

(defun md (&rest dirs)
  (dolist (d dirs)
    (when (gethash d (dir-entries *dir*))
      (error "duplicate entry: ~a" (string-downcase (symbol-name d))))
    (setf (gethash d (dir-entries *dir*)) (make-dir :name (string-downcase (symbol-name d)))))
  nil)

(defun cd (dir &optional (create nil))
  (let ((dp (symbol-name dir)))
    (when (char= (char dp 0) #\>)
      (setf *paths* nil)
      (setf *dirs* (list *root*))
      (setf dp (subseq dp 1)))
    (when (zerop (length dp))
      (return-from cd ">"))
    (setf dir (intern dp)))
  
  (when create
    (md dir))

  (cond
    ((string= dir "<")
     (pop *dirs*)
     (pop *paths*))
    (t
     (let* ((entries (dir-entries *dir*)))
       (unless (gethash dir entries)
	 (error "not found: ~a" (string-downcase (symbol-name dir))))
       (push (gethash dir entries) *dirs*))
     (push dir *paths*)))
  
  (get-path))

(defun start (&key (shell *shell*) (in *standard-input*) (out *standard-output*))
  (let* ((*shell* shell)
	 (*out* out))
    (labels ((rec-line ()
	       (format out "$~a " (length *results*))
	       (let* ((line (read-line in nil)))
		 (cond
		   ((string= line "")
		    (format out "~a~%" (get-path))
		    (rec-line))
		   ((string= line "q")
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

(defun save-changes (path)
  (save-lisp-and-die path :toplevel #'main :purify t))

(defun main ()
  (format t "lishp v~a,~%may the source be with you!~%~%" *version*)
  (start)
  (format t "saving changes...~%")
  (save-changes (first *posix-argv*))
  (format t "arrivederci!~%"))
