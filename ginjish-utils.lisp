;;;; ginjish-utils.lisp

(cl:in-package #:ginjish-utils)

(defgeneric symbolize (thing)
  (:method ((thing symbol))
    thing)
  (:method ((thing string))
    (serapeum:with-standard-input-syntax
     (let ((*read-eval* nil)
           (*package* (find-package "GINJISH-COMPILER"))) ; FIXME good choice?
       (read-from-string thing)))))

(defun read-keyword (string)
  (serapeum:with-standard-input-syntax
    (let ((*package* (find-package :keyword))
          (*read-eval* nil))
      (read-from-string string))))

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
  (:method ((map standard-object) key)
    #+(or)(closer-mop:ensure-finalized map) ; accessing an instance, surely
    (or
     (loop for slot in (alexandria:mappend
                        #'closer-mop:class-direct-slots
                        (closer-mop:class-precedence-list (class-of map)))
        for readers = (closer-mop:slot-definition-readers slot)
        do (alexandria:when-let ((reader
                                  (car
                                   (member key readers
                                           :test #'name-equal))))
             (return (funcall reader map))))
     (loop for slot in (closer-mop:class-slots (class-of map))
        for name = (closer-mop:slot-definition-name slot)
        when (and (slot-boundp map name)
                  (name-equal name key))
        do (return (slot-value map name)))))
  (:method ((map sequence) (key integer))
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
  (:method ((map standard-object) key value) ; FIXME test writers
    (or
     (loop for slot in (alexandria:mappend
                        #'closer-mop:class-direct-slots
                        (closer-mop:class-precedence-list (class-of map)))
        for writers = (closer-mop:slot-definition-writers slot)
          then (closer-mop:slot-definition-writers slot)
        do (alexandria:when-let ((writer
                                  (car
                                   (member key writers
                                           :key #'second
                                           :test #'name-equal))))
             (funcall (fdefinition writer) value map)
             (return map)))
    (loop for slot in (closer-mop:class-slots (class-of map))
       for name = (closer-mop:slot-definition-name slot)
       when (name-equal name key)
       do (setf (slot-value map name) value)
         (return map))))
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
  ((%map :initarg :map :reader context-map)
   (%parent :initarg :parent :initform nil :reader context-parent)))

(defmethod print-object ((object context) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (with-slots (%map %parent) object
      (print-expression %map stream)
      (format stream " (~A)" %parent))))

(defun make-context (parent map)
  (make-instance 'context :parent parent :map map))

(defmethod load-value ((map context) key)
  (with-slots (%map %parent) map
    (or (load-value %map key)
        (and %parent (load-value %parent key)))))

(defmethod set-load-value ((map context) key value)
  (setf (load-value (slot-value map '%map) key)
        value)
  map)

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

(defgeneric print-expression (thing stream &optional recursive)
  (:method (thing stream &optional recursive)
    (declare (ignore recursive))
    (princ thing stream))
  (:method ((thing integer) stream &optional recursive)
    (declare (ignore recursive))
    (princ thing stream))
  (:method ((thing float) stream &optional recursive)
    (declare (ignore recursive))
    (let ((*read-default-float-format* 'double-float))
      (princ thing stream)))
  (:method ((thing rational) stream &optional recursive)
    (declare (ignore recursive))
    (let ((*read-default-float-format* 'double-float))
      (princ (float thing) stream)))
  (:method ((thing string) stream &optional recursive)
    (when recursive (princ "'" stream))
    (princ thing stream)
    (when recursive (princ "'" stream)))
  (:method ((thing (eql nil)) stream &optional recursive)
    (declare (ignore recursive))
    (princ "False" stream))
  (:method ((thing (eql t)) stream &optional recursive)
    (declare (ignore recursive))
    (princ "True" stream))
  (:method ((thing list) stream &optional recursive)
    (declare (ignore recursive))
    (cond ((alexandria:proper-list-p thing)
           (princ "[" stream)
           (loop for l on thing
              do (print-expression (car l) stream t)
              when (cdr l)
              do (princ ", " stream))
           (princ "]" stream))
          (t
           (princ "(" stream)
           (loop for l on thing
              do (print-expression (car l) stream t)
              when (cdr l)
              do (princ ", " stream)
              finally (print-expression l stream t))
           (princ ")" stream))))
  (:method ((thing hash-table) stream &optional recursive)
    (declare (ignore recursive))
    (princ "{" stream)
    (loop for k on (alexandria:hash-table-keys thing)
         do (print-expression (car k) stream t)
         (princ ":" stream)
         (print-expression (gethash (car k) thing) stream t)
       when (cdr k)
         do (princ ", " stream))
    (princ "}" stream)))

(defun print-expression-to-string (thing)
  (with-output-to-string (s)
    (print-expression thing s)))

(defgeneric copy-map (map)
  (:method ((map list))
    (copy-list map))
  (:method ((map hash-table))
    (alexandria:copy-hash-table map))
  (:method ((map sequence))
    (alexandria:copy-sequence (type-of map) map)))

(defun getf-name (plist name)
  (loop for (key val) on plist by #'cddr
     when (name-equal name key)
     do (return-from getf-name (values val t))
     finally (return (values nil nil))))

(defgeneric to-list (thing) ; FIXME more general iterable?
  (:method (thing)
    (coerce thing 'list))
  (:method ((thing hash-table))
    (alexandria:hash-table-alist thing)))

