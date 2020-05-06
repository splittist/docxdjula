;;;; ginjish-compiler.lisp

(cl:defpackage #:ginjish-compiler
  (:use #:cl))

(cl:in-package #:ginjish-compiler)

(defun name-equal (a b)
  (string-equal (princ-to-string a) (princ-to-string b)))

(defgeneric load-value (map key)
  (:method ((map list) (key integer))
    (elt map key))
  (:method ((map list) key)
    (if (listp (car map))
	(cdr (assoc key map :test #'name-equal))
	(cadr (member key map :test #'name-equal))))
  (:method ((map hash-table) key) ; FIXME symbols and strings as separate namespaces?
    (gethash key map))
  (:method ((map standard-object) key) ; FIXME accessors?
    (loop for slot in (closer-mop:class-slots (class-of map))
       for name = (closer-mop:slot-definition-name slot)
       when (and (slot-boundp map name)
		 (name-equal name key))
       do (return (slot-value map name))))
  (:method (map (key integer))
    (elt map key)))

(defun set-plist-value (list key value)
  (loop for (k v) on list by #'cddr
     with collected = nil
     collect k into res
     if (name-equal k key)
     do (setf collected t)
     and collect value into res
     else collect v into res
     finally
       (unless collected
	 (setf res (list* key value res)))
       (return res)))

(defgeneric set-load-value (map key value) ; FIXME missing a step?
  (:method :around (map key value)
    (declare (ignore map key))
    (values value (call-next-method)))
  (:method ((map list) (key integer) value)
    (setf (elt map key) value)
    map)
  (:method ((map list) key value)
    (if (consp (car map))
	(let ((acons (assoc key map :test #'name-equal)))
	  (if acons
	      (progn
		(rplacd acons value)
		map)
	      (list* (cons key value) map)))
	(set-plist-value map key value)))
  (:method ((map hash-table) key value)
    (setf (gethash key map) value)
    map)
  (:method ((map standard-object) key value) ; FIXME accessors?
    (loop for slot in (closer-mop:class-slots (class-of map))
       for name = (closer-mop:slot-definition-name slot)
       when (name-equal name key)
       do (setf (slot-value map name) value)
	 (return map)))
  (:method (map (key integer) value)
    (setf (elt map key) value)
    map))

(define-setf-expander load-value (place key
				  &aux (new-val (gensym "NEW-VAL"))
				    (place-store (gensym "PLACE"))
				  &environment env)
  (declare (ignore env))
  (values ()
	  ()
	  `(,new-val)
	  `(progn
	     (multiple-value-bind (,new-val ,place-store)
		 (set-load-value ,place ,key ,new-val)
	       (setf ,place ,place-store)
	       ,new-val))
	  `(load-value ,place ,key)))

(defclass context ()
  ((%map :initarg :map)
   (%parent :initarg :parent :initform nil)))

(defun make-context (parent map)
  (make-instance 'context :parent parent :map map))

(defmethod load-value ((map context) key)
  (with-slots (%map %parent) map
    (or (load-value %map key)
	(and %parent (load-value %parent key)))))

(defmethod save-value ((map context) key value)
  (with-slots (%map) map
    (save-value %map key value)))

(defgeneric truthy (thing)
  (:method ((thing (eql 0)))
    nil)
  (:method ((thing (eql 0.0)))
    nil)
  (:method ((thing string))
    (not (string= "" thing)))
  (:method ((thing sequence))
    (< 0 (length thing)))
  (:method ((thing hash-table))
    (< 0 (hash-table-count thing)))
  (:method ((thing (eql nil)))
    nil)
  (:method (thing)
    thing))

(defgeneric compile-element (element)
  (:method ((element cons))
    (if (symbolp (first element))
	(compile-tagged-element (first element) (rest element))
	(error "Unknown element: ~S" element))))

(defmethod compile-element ((element (eql :comment)))
  (constantly nil))

(defmethod compile-element (element)
  (constantly element))

(defvar *context*)

(defvar *filters*)

(defvar *tests*)

(defgeneric compile-tagged-element (tag rest))

(defmethod compile-tagged-element ((tag (eql :matter)) rest)
  (alexandria:named-lambda :matter (stream)
    (princ (first rest) stream)))

(defmethod compile-tagged-element ((tag (eql :suite)) rest)
  (let ((contents (mapcar #'compile-element rest)))
    (alexandria:named-lambda :suite (stream)
      (dolist (element contents)
	(funcall element stream)))))

(defmethod compile-tagged-element ((tag (eql :string)) rest)
  (constantly (first rest)))

(defgeneric print-expression (thing stream)
  (:method (thing stream)
    (princ thing stream))
  (:method ((thing list) stream)
    (princ "[" stream)
    (loop for l on thing
       do (print-expression (car l) stream)
       when (cdr l)
	 do (princ ", " stream))
    (princ "]" stream))
  (:method ((thing hash-table) stream)
    (princ "{" stream)
    (loop for k on (alexandria:hash-table-keys thing)
	 do (print-expression (car k) stream)
	 (princ ":" stream)
	 (print-expression (gethash (car k) thing) stream)
       when (cdr k)
	 do (princ ", " stream))
    (princ "}" stream)))

(defmethod compile-tagged-element ((tag (eql :expression)) rest)
  (let ((expr (compile-element (first rest))))
    (alexandria:named-lambda :expression (stream)
      (print-expression (funcall expr stream) stream))))

(defmethod compile-tagged-element ((tag (eql :if-expr)) rest)
  (let ((test (compile-element (first rest)))
	(then (compile-element (second rest)))
	(else (compile-element (third rest))))
    (alexandria:named-lambda :if-expr (stream)
      (if (truthy (funcall test stream))
	  (funcall then stream)
	  (funcall else stream)))))

(defmethod compile-tagged-element ((tag (eql :or)) rest)
  (let ((left (compile-element (first rest)))
	(right (compile-element (second rest))))
    (alexandria:named-lambda :or (stream)
      (or (truthy (funcall left stream))
	  (truthy (funcall right stream))))))

(defmethod compile-tagged-element ((tag (eql :and)) rest)
  (let ((left (compile-element (first rest)))
	(right (compile-element (second rest))))
    (alexandria:named-lambda :and (stream)
      (and (truthy (funcall left stream))
	   (truthy (funcall right stream))))))

(defmethod compile-tagged-element ((tag (eql :not)) rest)
  (let ((expr (compile-element (first rest))))
    (alexandria:named-lambda :not (stream)
      (not (truthy (funcall expr stream))))))

(defgeneric gte (left right)
  (:method ((left string) (right string))
    (and (string>= left right) t))
  (:method ((left number) (right number))
    (>= left right))
  (:method ((left list) (right list))
    (let ((leftlen (length left))
	  (rightlen (length right))
	  (mismatch (mismatch left right)))
      (cond
	((null mismatch) ; same
	 t)
	((>= mismatch leftlen) ; right longer
	 nil)
	((>= mismatch rightlen) ; left longer
	 t)
	(t
	 (gte (elt left mismatch) (elt right mismatch)))))))

(defmacro define-comparison-compiler (tag func)
  `(defmethod compile-tagged-element ((tag (eql ,tag)) rest)
     (let ((left (compile-element (first rest)))
	   (right (compile-element (second rest))))
       (alexandria:named-lambda ,tag (stream)
	 (,func (funcall left stream)
		(funcall right stream))))))

(define-comparison-compiler :gte gte)

(defgeneric lte (left right)
  (:method ((left string) (right string))
    (and (string<= left right) t))
  (:method ((left number) (right number))
    (<= left right))
  (:method ((left list) (right list))
    (let ((leftlen (length left))
	  (rightlen (length right))
	  (mismatch (mismatch left right)))
      (cond
	((null mismatch) ; same
	 t)
	((>= mismatch leftlen) ; right longer
	 t)
	((>= mismatch rightlen) ; left longer
	 nil)
	(t
	 (lte (elt left mismatch) (elt right mismatch)))))))

(define-comparison-compiler :lte lte)

(defgeneric lt (left right)
  (:method ((left string) (right string))
    (and (string< left right) t))
  (:method ((left number) (right number))
    (< left right))
  (:method ((left list) (right list))
    (let ((leftlen (length left))
	  (rightlen (length right))
	  (mismatch (mismatch left right)))
      (cond
	((null mismatch) ; same
	 nil)
	((>= mismatch leftlen) ; right longer
	 t)
	((>= mismatch rightlen) ; left longer
	 nil)
	(t
	 (lt (elt left mismatch) (elt right mismatch)))))))

(define-comparison-compiler :lt lt)

(defgeneric gt (left right)
  (:method ((left string) (right string))
    (and (string> left right) t))
  (:method ((left number) (right number))
    (> left right))
  (:method ((left list) (right list))
    (let ((leftlen (length left))
	  (rightlen (length right))
	  (mismatch (mismatch left right)))
      (cond
	((null mismatch) ; same
	 nil)
	((>= mismatch leftlen) ; right longer
	 nil)
	((>= mismatch rightlen) ; left longer
	 t)
	(t
	 (gt (elt left mismatch) (elt right mismatch)))))))

(define-comparison-compiler :gt gt)

(defgeneric pequal (left right)
  (:method ((left string) (right string))
    (and (string= left right) t))
  (:method ((left number) (right number))
    (= left right))
  (:method ((left list) (right list))
    (let ((leftlen (length left))
	  (rightlen (length right))
	  (mismatch (mismatch left right)))
      (cond
	((null mismatch) ; same
	 t)
	((>= mismatch leftlen) ; right longer
	 nil)
	((>= mismatch rightlen) ; left longer
	 nil)
	(t
	 (pequal (elt left mismatch) (elt right mismatch))))))
  (:method ((left hash-table) (right hash-table))
    (let ((leftlist (alexandria:hash-table-plist left))
	  (rightlist (alexandria:hash-table-plist right)))
      (equal leftlist rightlist)))
  (:method (left right)
    nil))

(define-comparison-compiler :equal pequal)

(defun not-pequal (left right)
  (not (pequal left right)))

(define-comparison-compiler :not-equal not-pequal)

(defgeneric in (item collection)
  (:method (item (collection sequence))
    (find item collection :test #'equal)) ; FIXME alist, plist?
  (:method ((item string) (collection string))
    (serapeum:string*= item collection))
  (:method (item (collection hash-table))
    (nth-value 1 (gethash collection item))))

(define-comparison-compiler :in in)

(defun not-in (item collection)
  (not (in item collection)))

(define-comparison-compiler :not-in not-in)

(defgeneric plus (left right)
  (:method ((left number) (right number))
    (+ left right))
  (:method ((left list) (right list))
    (append left right))
  (:method ((left string) (right string))
    (concatenate 'string left right))) ; FIXME vectors?

(defmacro define-binary-compiler (tag func)
  `(defmethod compile-tagged-element ((tag (eql ,tag)) rest)
     (let ((left (compile-element (first rest)))
	   (right (compile-element (second rest))))
       (alexandria:named-lambda ,tag (stream)
	 (,func (funcall left stream)
		(funcall right stream))))))

(define-binary-compiler :plus plus)

(defgeneric minus (left right)
  (:method ((left number) (right number))
    (- left right)))

(define-binary-compiler :minus minus)

(defgeneric pconcatenate (left right)
  (:method (left right)
    (concatenate 'string (princ-to-string left) (princ-to-string right))))

(define-binary-compiler :concatenate pconcatenate)

(defgeneric mul (left right)
  (:method ((left number) (right number))
    (* left right))
  (:method ((left sequence) (right number))
    (serapeum:repeat-sequence left right))
  (:method ((left number) (right sequence))
    (serapeum:repeat-sequence right left)))

(define-binary-compiler :mul mul)

(defun pfloor (left right)
  (floor left right))

(define-binary-compiler :floor pfloor)

(defun div (left right)
  (/ left right))

(define-binary-compiler :div div)

(defgeneric pmod (left right)
  (:method ((left number) (right number))
    (mod left right))
  (:method ((left string) right)
    (error "printf-style string formatting with '%' not implemented.")))

(define-binary-compiler :mod pmod)

(defmethod compile-tagged-element ((tag (eql :uplus)) rest)
  (let ((expr (compile-element (first rest))))
    expr))

(defmethod compile-tagged-element ((tag (eql :uminus)) rest)
  (let ((expr (compile-element (first rest))))
    (alexandria:named-lambda :uminus (stream)
      (- (funcall expr stream)))))

(defmethod compile-tagged-element ((tag (eql :pow)) rest)
  (let ((base (compile-element (first rest)))
	(power (compile-element (second rest))))
    (alexandria:named-lambda :pow (stream)
      (expt (funcall base stream) (funcall power stream)))))

;;; TODO :test :test-not and :filter

(defmethod compile-tagged-element ((tag (eql :tuple)) rest) ; FIXME tuples are lists
  (let ((elements (mapcar #'compile-element rest)))
    (alexandria:named-lambda :tuple (stream)
      (mapcar (alexandria:rcurry #'funcall stream) elements))))

(defmethod compile-tagged-element ((tag (eql :list)) rest)
  (let ((elements (mapcar #'compile-element rest)))
    (alexandria:named-lambda :list (stream)
      (mapcar (alexandria:rcurry #'funcall stream) elements))))

(defmethod compile-tagged-element ((tag (eql :set)) rest) ; FIXME sets are lists
  (let ((elements (mapcar #'compile-element rest)))
    (alexandria:named-lambda :set (stream)
      (mapcar (alexandria:rcurry #'funcall stream) elements))))

(defmethod compile-tagged-element ((tag (eql :dict)) rest)
  (let ((elements
	 (loop for (key value) in rest
	    collecting (list (compile-element key)
			     (compile-element value)))))
    (alexandria:named-lambda :dict (stream)
      (let ((ht (make-hash-table :test 'equal)))
	(loop for (key value) in elements
	   do (setf (gethash (funcall key stream) ht)
		    (funcall value stream)))
	ht))))

(defmethod compile-tagged-element ((tag (eql :identifier)) rest)
  (alexandria:named-lambda :identifier (stream)
    (declare (ignore stream))
    (load-value *context* (first rest))))

(defmethod compile-tagged-element ((tag (eql :get-attr)) rest)
  (let ((primary (compile-element (first rest)))
	(identifier (second (second rest))))
    (alexandria:named-lambda :get-attr (stream)
      (load-value (funcall primary stream)
		  identifier))))

(defclass slice ()
  ((%lower-bound :initarg :lower-bound :reader lower-bound)
   (%upper-bound :initarg :upper-bound :reader upper-bound)
   (%stride :initarg :stride :reader stride)))

(defmethod print-object ((object slice) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "[~A:~A:~A]" (lower-bound object) (upper-bound object) (stride object))))

(defun make-slice (lower upper stride)
  (make-instance 'slice :lower-bound lower :upper-bound upper :stride stride))

(defmethod compile-tagged-element ((tag (eql :proper-slice)) rest)
  (let ((args (mapcar #'compile-element rest)))
    (alexandria:named-lambda :proper-slice (stream)
      (apply #'make-slice (mapcar (alexandria:rcurry #'funcall stream) args)))))

(defgeneric get-item (object indices)
  (:method ((object list) indices)
    (if (= 1 (length indices))
	(let ((index (first indices)))
	  (if (integerp index)
	      (progn
		(when (minusp index)
		  (incf index (length object)))
		(elt object index))
	      (load-value object index)))
	(load-value object indices)))
  (:method ((object string) indices)
    (unless (and (= 1 (length indices))
		 (integerp (first indices)))
      (error "Malformed attribute for string"))
    (let ((index (first indices)))
      (when (minusp index)
	(incf index (length object)))
      (string (elt object (first indices)))))
  (:method ((object hash-table) indices)
    (gethash indices object)))

(defmethod compile-tagged-element ((tag (eql :get-item)) rest)
  (let ((primary (compile-element (first rest)))
	(indices (mapcar #'compile-element (second rest))))
    (alexandria:named-lambda :get-item (stream)
      (get-item (funcall primary stream)
		(mapcar (alexandria:rcurry #'funcall stream) indices)))))

(defgeneric get-slice (object slice)
  (:method ((object list) (slice slice))
    (loop for index from (or (lower-bound slice) 0)
       by (or (stride slice) 1)
       below (or (upper-bound slice) (length object))
       collecting (elt object index)))
  (:method ((object string) (slice slice))
    (let ((chars (get-slice (coerce object 'list) slice)))
      (coerce chars 'string))))

(defmethod compile-tagged-element ((tag (eql :slicing)) rest)
  (let ((primary (compile-element (first rest)))
	(slice (compile-element (second rest))))
    (alexandria:named-lambda :slicing (stream)
      (get-slice (funcall primary stream)
		 (funcall slice stream)))))

;;; TODO :call

(defmethod compile-tagged-element ((tag (eql :raw)) rest)
  (alexandria:named-lambda :raw (stream)
    (princ (first rest) stream)))

;;; TODO :for

(defmethod compile-tagged-element ((tag (eql :if)) rest)
  (let ((test (compile-element (first rest)))
	(then (compile-element (second rest)))
	(elifs (alexandria:when-let ((it (fourth rest)))
		 (loop for (test consequent) in it
		    collecting (list (compile-element test)
				     (compile-element consequent)))))
	(else (alexandria:when-let ((it (third rest)))
		(compile-element it))))
    (alexandria:named-lambda :if (stream)
      (or (and (truthy (funcall test stream))
	       (or (funcall then stream) t))
	  (and elifs
	       (loop with triggered = nil
		  for (test consequent) in elifs
		  when (truthy (funcall test stream))
		  do (funcall consequent stream)
		    (setf triggered t)
		    (loop-finish)
		  finally (return triggered)))
	  (and else (funcall else stream))))))

(defmethod compile-tagged-element ((tag (eql :assign)) rest)
  (let ((expr (compile-element (second rest)))
	(targets (first rest)))
    (if (= 1 (length targets))
	(alexandria:named-lambda :assign (stream)
	  (setf (load-value *context* (first targets))
		(funcall expr stream)))
	(alexandria:named-lambda :assign (stream)
	  (let ((seq (funcall expr stream)))
	    (dotimes (index (length targets))
	      (setf (load-value *context* (elt targets index))
		    (elt seq index))))))))

#|

;;; compilation and rendering

(defclass template ()
  ())

(defclass compiled-template ()
  ((%template-function :initarg :template-function :accessor template-function))
  (:metaclass closer-mop:funcallable-standard-class))

(defmethod initialize-instance :after ((compiled-template compiled-template) &rest initargs)
  (declare (ignore initargs))
  (closer-mop:set-funcallable-instance-function
   compiled-template
   (template-function compiled-template)))

(defgeneric compile-template (template &key &allow-other-keys)
  (:method ((string string) &key &allow-other-keys)
    (let ((*autoescape* *autoescape*))
      (let ((function (parse 'template string)))
	(make-instance 'compiled-template :template-function function)))))

(defgeneric render-template (template stream context &key &allow-other-keys)
  (:method ((template compiled-template) stream (context context) &key &allow-other-keys)
    (let ((*context* context))
      (funcall template stream))))

|#
