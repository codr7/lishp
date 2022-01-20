(defpackage lishp
  (:use cl)
  (:import-from sb-ext *posix-argv* save-lisp-and-die)
  
  (:export *debug* *dir* *dirs* *path* *paths* *results* *root* *version*
	   cd
	   eval-entry eval-line
	   find-entry format-result
	   _get get-dir-keys get-entry get-path
	   ls
	   main md
	   print-result
	   rm rm-entry
	   _set shell say start))

(in-package lishp)

(define-symbol-macro *version* 1)

(define-symbol-macro *debug* (slot-value *shell* 'debug?))
(define-symbol-macro *paths* (slot-value *shell* 'paths))
(define-symbol-macro *path* (first *paths*))
(define-symbol-macro *root* (slot-value *shell* 'root))
(define-symbol-macro *dirs* (slot-value *shell* 'dirs))
(define-symbol-macro *dir* (first *dirs*))
(define-symbol-macro *results* (slot-value *shell* 'results))
(define-symbol-macro *stack* (slot-value *shell* 'stack))

(defvar *out*)

(defmacro dohash ((k v tbl) &body body)
  (let* (($i (gensym)) ($k (gensym)) ($next (gensym)) ($ok (gensym)))
    `(with-hash-table-iterator (,$i ,tbl)
       (tagbody
	  ,$next
	  (multiple-value-bind (,$ok ,$k ,v) (,$i)
	    (declare (ignorable ,v))
	    (when ,$ok
	      (let* ((,k ,$k))
		(declare (ignorable ,k))
		,@body
		(go ,$next))))))))

(defstruct dir
  (name (error "missing name"))
  (entries (make-hash-table)))

(defun dir-count (&optional (dir *dir*))
  (hash-table-count (dir-entries dir)))

(defun get-dir-keys (&key dir)
  (let* (out)
    (dohash (k v (dir-entries (or dir *dir*)))
      (push k out))
    (nreverse out)))

(defmethod str! ((val string))
  val)

(defmethod str! ((val symbol))
  (string-downcase (symbol-name val)))

(defun find-entry (key &key (dirs *dirs*))
  (let ((p *paths*))
    (dolist (d dirs)
      (let* ((v (gethash key (dir-entries d))))
	(when v
	  (return-from find-entry (values v p))))
      (pop p))
    (values nil p)))

(defun format-path (&optional (path *paths*))
  (if path
      (with-output-to-string (out)
	(dolist (p (reverse path))
	  (format out "~a>" (str! p))))
      ">"))

(defun get-entry (key &key (dirs *dirs*))
  (multiple-value-bind (v p) (find-entry key :dirs dirs)
    (unless v
      (error "not found: ~a~a" (get-path) (str! key)))
    (values v p)))

(defmethod format-entry (key val)
  key)

(defun push-stack (val)
  (push val *stack*))

(defun pop-stack ()
  (pop *stack*))

(defun _get (&optional key)
  (multiple-value-bind (v p) (get-entry (or key (pop-stack)))
    (format *out* "~a~a:~%" (format-path p) (str! key))
    v))

(defun _set (key &optional val)
  (setf (gethash key (dir-entries *dir*)) (or val (pop-stack)))
  nil)

(defmethod eval-entry ((val function) args)
  (apply val args))

(defmethod eval-line (in)
  (let* ((fn (pop in)))
    (when fn
      (let* ((out (eval-entry (get-entry fn) in)))
	(push-stack out)
	(vector-push-extend out *results*)
	out))))

(defmethod format-result (in)
  (with-output-to-string (out)
    (princ in out)))

(defmethod format-result ((in symbol))
  (str! in))

(defmethod print-result (in)
  (format *out* "~a~%" (format-result in)))

(defmethod print-result ((in null)))

(defmethod print-result ((in list))
  (dolist (r in)
    (print-result r)))

(defun say (spec &rest args)
  (apply #'format *out* spec args)
  (terpri *out*))

(defun rm-entry (key)
  (unless (remhash key (dir-entries *dir*))
    (error "not found: ~a" key)))

(defun rm (&rest paths)
  (dolist (p paths)
    (rm-entry p)))

(defmethod format-entry (key (val function))
  (format nil "~a()" (str! key)))

(defmethod format-entry (key (val dir))
  (format nil "~a> (~a)" (str! key) (dir-count val)))

(defun ls (&rest args)
  (declare (ignore args))
  (format *out* "contents of ~a:~%" (format-path))
  (mapcar (lambda (k)
	    (format-entry k (gethash k (dir-entries *dir*))))
	  (stable-sort (get-dir-keys)
		       (lambda (x y)
			 (string< (symbol-name x) (symbol-name y))))))

(defun md (&rest dirs)
  (dolist (d dirs)
    (when (gethash d (dir-entries *dir*))
      (error "duplicate entry: ~a" (str! d)))
    (setf (gethash d (dir-entries *dir*)) (make-dir :name (str! d))))
  nil)

(defun cd (dir &optional (create nil))
  (let* ((dp (symbol-name dir)))
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
	 (error "not found: ~a" (str! dir)))
       (push (gethash dir entries) *dirs*))
     (push dir *paths*)))
  
  (format-path))

(defclass shell ()
  ((debug? :initform nil :reader debug?)
   (paths :initform nil :reader paths)
   (root :initform (make-dir :name ">") :reader root)
   (dirs :initform nil :reader dirs)
   (results :initform (make-array 0 :fill-pointer 0) :reader results)
   (stack :initform (make-array 0 :fill-pointer 0) :reader stack)))

(defun bind (dir key val)
  (setf (gethash key (dir-entries dir)) val))

(defmethod initialize-instance :after ((self shell) &key)
  (with-slots (root dirs) self
    (bind root 'cd #'cd)
    (bind root 'get #'_get)
    (bind root 'ls #'ls)
    (bind root 'md #'md)
    (bind root 'rm #'rm)
    (bind root 'say #'say)
    (bind root 'set #'_set)
    (push root dirs)))

(defparameter *shell* (make-instance 'shell))

(defun start (&key (*shell* *shell*) (in *standard-input*) (out *standard-output*))
  (let* ((*out* out))
    (labels ((rec-line ()
	       (format out "$~a " (length *results*))
	       (force-output out)

	       (let* ((line (read-line in nil)))
		 (cond
		   ((string= line "")
		    (format out "~a~%" (format-path))
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

(declaim (ftype (function ()) main))

(defun save-changes (path)
  (save-lisp-and-die path :toplevel #'main :purify t :executable t))

(defun main ()
  (in-package lishp)
  (format t "lishp v~a,~%may the source be with you!~%~%" *version*)
  (start)
  (format t "saving the world...~%")
  (save-changes (first *posix-argv*)))
