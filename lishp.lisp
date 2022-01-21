(defpackage lishp
  (:use cl)
  (:import-from sb-ext *posix-argv* save-lisp-and-die)
  (:import-from local-time +utc-zone+ format-timestring now timestamp today)
  (:export *debug* *dir* *dirs* *path* *paths* *results* *root* *version*
	   cd cp cp-entry
	   eval-entry eval-line
	   find-entry format-path format-result
	   _get get-dir-keys get-entry
	   ls
	   main md mv
	   print-result
	   rm rm-entry
	   _set shell say start))

(in-package lishp)

(define-symbol-macro *version* 1)

(define-symbol-macro *debug?* (slot-value *shell* 'debug?))
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

(defun make-dir ()
  (make-hash-table))

(defmethod str! ((val string))
  val)

(defmethod str! ((val symbol))
  (string-downcase (symbol-name val)))

(defmethod sym! ((val symbol))
  val)

(defmethod sym! ((val string))
  (intern (string-upcase val)))

(defun find-entry (key &key (dirs *dirs*))
  (let* ((p *paths*))
    (dolist (d dirs)
      (let* ((v (gethash key d)))
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

(defun parse-path (in paths dirs &key md?)
  (let* ((s (str! in)))
    (cond
      ((zerop (length s))
       (values nil paths dirs))
      ((char= (char s 0) #\<)
       (parse-path (sym! (subseq s 1)) (rest paths) (rest dirs) :md? md?))
      (t
       (let* ((i (position #\> s)))
	 (if i
	     (progn
	       (when (zerop i)
		 (return-from parse-path (parse-path (sym! (subseq s 1)) nil (list *root*))))
	       (let* ((p (sym! (subseq s 0 i)))
		      (v (gethash p (first dirs))))
		 (unless v
		   (if md?
		       (progn
			 (setf v (make-dir))
			 (setf(gethash p (first dirs)) v))
		       (error "not found: ~a~a" (format-path paths) (str! p))))
		 (if (= i (length s))
		     (values nil (cons p paths) (cons v dirs))
		     (parse-path (sym! (subseq s (1+ i))) (cons p paths) (cons v dirs) :md? md?))))
	     (values in paths dirs)))))))

(defun get-entry (key &key (dirs *dirs*) (paths *paths*))
  (multiple-value-bind (v p) (find-entry key :dirs dirs)
    (unless v
      (error "not found: ~a~a" (format-path paths) (str! key)))
    (values v p)))

(defmethod format-entry (key val)
  (format nil "~a=~a" (str! key) (format-result val)))

(defun push-stack (val)
  (push val *stack*))

(defun pop-stack ()
  (pop *stack*))

(defun _get (key)
  (multiple-value-bind (v p) (get-entry key)
    (format *out* "~a~a:~%" (format-path p) (str! key))
    v))

(defun _set (key &optional val)
  (setf (gethash key *dir*) (or val (pop-stack)))
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

(defmethod format-result (val)
  (with-output-to-string (out)
    (princ val out)))

(defmethod format-result ((val function))
  (multiple-value-bind (l c? n) (function-lambda-expression val)
    (declare (ignore l c?))
    (format nil "~a()" (str! n))))

(defmethod format-result ((val symbol))
  (str! val))

(defmethod format-result ((val timestamp))
  (let ((f (get-entry 'time-format))
	(z (get-entry 'time-zone)))
    (format-timestring nil val :format f :timezone z)))

(defmethod print-result (val)
  (format *out* "~a~%" (format-result val)))

(defmethod print-result ((val hash-table))
  (let ((ks (get-dir-keys :dir val)))
    (dolist (k ks)
      (format *out* "~a~%" (format-entry k (gethash k val))))))

(defmethod print-result ((val list))
  (dolist (r val)
    (print-result r)))

(defmethod print-result ((val null))
  (declare (ignore val)))

(defun say (spec &rest args)
  (apply #'format *out* spec args)
  (terpri *out*))

(defun rm-entry (key)
  (multiple-value-bind (key ps ds) (parse-path key *paths* *dirs*)
    (unless (remhash key (first ds))
      (error "not found: ~a~a" (format-path ps) key))))

(defun rm (&rest paths)
  (dolist (p paths)
    (rm-entry p)))

(defun mv (src dst)
  (multiple-value-bind (src ps ds) (parse-path src *paths* *dirs*)
    (let* ((v (gethash src (first ds))))
      (unless v
	(error "not found: ~a~a" (format-path ps) src))
      (remhash src (first ds))

      (multiple-value-bind (dst ps ds) (parse-path dst *paths* *dirs*)
	(declare (ignore ps))
	(setf (gethash (or dst src) (first ds)) v))))
  nil)

(defmethod cp-entry (val)
  val)

(defmethod cp-entry ((val hash-table))
  (make-hash-table 
   :test (hash-table-test val)
   :rehash-size (hash-table-rehash-size val)
   :rehash-threshold (hash-table-rehash-threshold val)
   :size (hash-table-size val)))

(defmethod cp-entry ((val list))
  (copy-list val))

(defmethod cp-entry ((val string))
  (copy-seq val))

(defun cp (src dst)
  (multiple-value-bind (src ps ds) (parse-path src *paths* *dirs*)
    (let ((v (gethash src (first ds))))
      (unless v
	(error "not found: ~a~a" (format-path ps) src))
      (multiple-value-bind (dst ps ds) (parse-path dst *paths* *dirs*)
	(declare (ignore ps))
	(setf (gethash dst (first ds)) (cp-entry v)))))
  nil)

(defmethod format-entry (key (val function))    
  (format nil "~a()" (str! key)))

(defmethod format-entry (key (val hash-table))
  (format nil "~a> (~a)" (str! key) (dir-count val)))

(defun get-dir-keys (&key dir)
  (let* (out)
    (dohash (k v (or dir *dir*))
      (push k out))
    (stable-sort (nreverse out)
		 (lambda (x y)
		   (string< (symbol-name x) (symbol-name y))))))

(defun ls (&rest args)
  (declare (ignore args))
  (format *out* "contents of ~a:~%" (format-path))
  (mapcar (lambda (k)
	    (format-entry k (gethash k *dir*)))
	  (get-dir-keys)))

(defun md (&rest paths)
  (dolist (p paths)
    (multiple-value-bind (p ps ds) (parse-path p *paths* *dirs* :md? t)
      (when p
	(when (gethash p (first ds))
	  (error "duplicate entry: ~a~a" ps (str! p)))
	(setf (gethash p (first ds)) (make-dir)))))
  nil)

(defun cd (path &optional (md? nil))
  (multiple-value-bind (p ps ds) (parse-path path *paths* *dirs* :md? md?)
    (setf *paths* ps *dirs* ds)
    
    (when p
      (push p *paths*)
      (let ((d (gethash p *dir*)))
	(unless d
	  (if md?
	      (progn
		(setf d (make-dir))
		(setf (gethash p *dir*) d))
	      (error "not found: ~a~a" ps p)))
	(push d *dirs*))))
  
  (format-path))

(defclass shell ()
  ((debug? :initform nil :reader debug?)
   (paths :initform nil :reader paths)
   (root :initform (make-dir) :reader root)
   (dirs :initform nil :reader dirs)
   (results :initform (make-array 0 :fill-pointer 0) :reader results)
   (stack :initform (make-array 0 :fill-pointer 0) :reader stack)))

(defun bind (dir key val)
  (setf (gethash key dir) val))

(defmethod initialize-instance :after ((self shell) &key)
  (with-slots (root dirs) self
    (bind root 'cd #'cd)
    (bind root 'cp #'cp)
    (bind root 'get #'_get)
    (bind root 'ls #'ls)
    (bind root 'md #'md)
    (bind root 'mv #'mv)
    (bind root 'now #'now)
    (bind root 'rm #'rm)
    (bind root 'say #'say)
    (bind root 'set #'_set)
    (bind root 'today #'today)
    (bind root 'time-format '(:year #\- (:month 2) #\- (:day 2) #\space (:hour 2) #\: (:min 2)))
    (bind root 'time-zone +utc-zone+)
    (push root dirs)))

(defparameter *shell* (make-instance 'shell))

(defun dir-count (&optional (dir *dir*))
  (hash-table-count dir))

(defun start (&key (*shell* *shell*) (in *standard-input*) (out *standard-output*))
  (let* ((*out* out))
    (labels ((rec-line ()
	       (format out "~a " (length *results*))
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
			    (when *debug?*
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
  (format t "saving world...~%")
  (save-changes (first *posix-argv*)))
