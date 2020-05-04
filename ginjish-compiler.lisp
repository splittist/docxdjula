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

(defclass context ()
  ((%map :initarg :map)
   (%parent :initarg :parent :initform nil)))

(defun make-context (parent map)
  (make-instance 'context :parent parent :map map))

(defmethod load-value ((map context) key)
  (with-slots (map parent) map
    (or (load-value map key)
	(and parent (load-value parent key)))))

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
    (princ thing stream)))

(defmethod compile-tagged-element ((tag (eql :expression)) rest)
  (let ((expr (compile-element (first rest))))
    (alexandria:named-lambda :expression (stream)
      (print-expression (funcall expr stream) stream))))

(defmethod compile-tagged-element ((tag (eql :if)) rest)
  (let ((test (compile-element (first rest)))
	(then (compile-element (second rest)))
	(else (compile-element (third rest))))
    (alexandria:named-lambda :if (stream)
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
    (loop for l on left
       for r on right
       while (equal (car l) (car r))
	 do (format t "~A ~A~%" l r)
       finally
	 (return
	   (cond ((and l (null r))
		  t)
		 ((and r (null l))
		  nil)
		 ((and (null l) (null r))
		  t)
		 (t
		  (gte (car l) (car r))))))))

  

(defmethod 

#|
(defvar *context*)



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
