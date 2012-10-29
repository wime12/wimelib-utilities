;;;; wimelib-utilities.lisp

(in-package #:wimelib-utilities)

(defmacro with-collectors ((&rest collectors) &body body)
  (flet ((gen-names (len &optional (string-or-integer ""))
	   (do ((i 0 (1+ i))
		(names nil (cons (gensym string-or-integer) names)))
	       ((>= i len) names))))
    (let* ((len (length collectors))
	   (heads (gen-names len "HEAD"))
	   (tails (gen-names len "TAIL")))
      `(let* (,@(mapcar (lambda (head) `(,head (list nil))) heads)
	      ,@(mapcar (lambda (head tail) `(,tail ,head)) heads tails))
	 (flet (,@(mapcar
		   (lambda (collector tail)
		     `(,collector (x)
				  (setf (cdr ,tail) (setf ,tail (list x)))))
		   collectors tails))
	   ,@body)
	 (values ,@(mapcar (lambda (head) `(cdr ,head)) heads))))))

(defmacro collecting (&body body)
  `(with-collectors (collect) ,@body))

(defun split-at (separator sequence)
  (let ((pos (position separator sequence)))
    (if pos
	(values
	 (subseq sequence 0 pos)
	 (subseq sequence (1+ pos)))
	(values sequence nil))))

(defun split (separator sequence)
  (collecting
    (labels ((split (sequence)
	       (when sequence
		 (multiple-value-bind (p r) (split-at separator sequence)
		   (collect p)
		   (split r)))))
      (split sequence))))

(defmacro with-unique-names ((&rest names) &body body)
  `(let ,(mapcar #'make-binding names)
     ,@body))

(defun make-binding (name)
  (etypecase name
    (symbol
     `(,name (gensym ,(symbol-name name))))
    ((cons symbol (cons string null))
     `(,(first name) (gensym ,(second name))))))

(defmacro once-only ((&rest names) &body body)
  (let ((gensyms (mapcar #'(lambda (n) (declare (ignore n)) (gensym)) names)))
    `(let (,@(mapcar #'(lambda (g n) `(,g (gensym ,(symbol-name n))))
		     gensyms names))
       `(let (,,@(mapcar #'(lambda (g n) ``(,,g ,,n)) gensyms names))
	  ,(let (,@(mapcar #'(lambda (n g) `(,n ,g)) names gensyms))
		,@body)))))

(defun ensure-list (x)
  (if (listp x) x (list x)))

(defun plist-alist (plist)
  (collecting
    (do ((tail plist (cddr tail)))
	((endp tail))
      (collect (list (car tail) (cadr tail))))))
