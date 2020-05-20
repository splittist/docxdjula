;;;; ginjish-compiler.lisp

(cl:in-package #:ginjish-compiler)

(defgeneric symbolize (thing)
  (:method ((thing symbol))
    thing)
  (:method ((thing string))
    (serapeum:with-standard-input-syntax
     (let ((*read-eval* nil)
           (*package* (find-package "GINJISH-COMPILER"))) ; FIXME good choice?
       (read-from-string thing)))))

(defun find-filter (name)
  (find-symbol (symbol-name (symbolize name)) (find-package "GINJISH.FILTERS")))

(defun apply-filters (value filters stream)
  (loop for (name args) in filters
     with result = (funcall value stream)
     do (alexandria:if-let ((fn (find-filter name)))
          (let ((arguments (mapcar (alexandria:rcurry #'funcall stream) args)))
            (setf result (apply fn result arguments)))
          (error "Unknown filter '~A'" name))
     finally (return result)))

(defun find-test (name)
  (find-symbol (symbol-name (symbolize name)) (find-package "GINJISH.TESTS")))

(defun apply-test (value name args stream)
  (alexandria:if-let ((fn (find-test name)))
    (let ((arguments (mapcar (alexandria:rcurry #'funcall stream) args)))
      (apply fn (funcall value stream) arguments))
    (error "Unknown test '~A'" name)))

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
   (%parent :initarg :parent :initform nil)))

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

(defgeneric compile-element (element)
  (:method ((element cons))
    (if (symbolp (first element))
        (compile-tagged-element (first element) (rest element))
        (error "Unknown element: ~S" element))))

(defmethod compile-element ((element (eql :comment)))
  (constantly nil))

(defmethod compile-element (element)
  (constantly element))

(defvar *autoescape* nil)

(defvar *context*)

(defvar *trim-blocks* nil)

(defvar *lstrip-blocks* nil)

(defgeneric compile-tagged-element (tag rest))

(defmethod compile-tagged-element ((tag (eql :matter)) rest)
  (alexandria:named-lambda :matter (stream)
    (princ (first rest) stream)))

(defmethod compile-tagged-element ((tag (eql :newlines)) rest)
  (alexandria:named-lambda :newlines (stream)
    (loop repeat (first rest) do (terpri stream))))

(defmethod compile-tagged-element ((tag (eql :left-ws+)) rest)
  (alexandria:named-lambda :left-ws+ (stream)
    (princ (first rest) stream)))

(defmethod compile-tagged-element ((tag (eql :left-ws)) rest)
  (alexandria:named-lambda :left-ws (stream)
    (unless *lstrip-blocks* (princ (first rest) stream))))

(defmethod compile-tagged-element ((tag (eql :right-newline)) rest)
  (alexandria:named-lambda :right-newline (stream)
    (unless *trim-blocks* (terpri stream))))

(defmethod compile-tagged-element ((tag (eql :leading-ws)) rest)
  (alexandria:named-lambda :leading-ws (stream)
    (declare (ignore stream))
    (values)))

(defmethod compile-tagged-element ((tag (eql :trailing-ws)) rest)
  (alexandria:named-lambda :trailing-ws (stream)
    (declare (ignore stream))
    (values)))

(defmethod compile-tagged-element ((tag (eql :suite)) rest)
  (let ((contents (mapcar #'compile-element rest)))
    (alexandria:named-lambda :suite (stream)
      (dolist (element contents)
        (funcall element stream)))))

(defmethod compile-tagged-element ((tag (eql :template)) rest)
  (let ((contents (mapcar #'compile-element rest)))
    (alexandria:named-lambda :template (stream)
      (dolist (element contents)
        (funcall element stream)))))

(defmethod compile-tagged-element ((tag (eql :string)) rest)
  (constantly (first rest)))

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
    (serapeum:true (find item collection :test #'equal))) ; FIXME alist, plist?
  (:method ((item string) (collection string))
    (serapeum:true (serapeum:string*= item collection)))
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
    (concatenate 'string
                 (print-expression-to-string left)
                 (print-expression-to-string right))))

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

(defmethod compile-tagged-element ((tag (eql :filter)) rest)
  (let ((value (compile-element (first rest)))
        (filters (loop for (name args) in (rest rest)
                    collect (list name (mapcar #'compile-element args)))))
    (alexandria:named-lambda :filter (stream)
      (apply-filters value filters stream))))

(defmethod compile-tagged-element ((tag (eql :test)) rest)
  (destructuring-bind (value-form (name args)) rest
    (let ((value (compile-element value-form))
          (arguments (mapcar #'compile-element args)))
      (alexandria:named-lambda :test (stream)
        (apply-test value name arguments stream)))))

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
    (gethash (if (serapeum:single indices)
                 (first indices)
                 indices)
             object)))

(defmethod compile-tagged-element ((tag (eql :get-item)) rest)
  (let ((primary (compile-element (first rest)))
        (indices (mapcar #'compile-element (second rest))))
    (alexandria:named-lambda :get-item (stream)
      (get-item (funcall primary stream)
                (mapcar (alexandria:rcurry #'funcall stream) indices)))))

(defmethod compile-tagged-element ((tag (eql :invoke)) rest)
  (let ((func (compile-element (first rest)))
        (args (mapcar #'compile-element (second rest))))
    (alexandria:named-lambda :invoke (stream)
      (apply (funcall func stream)
             (mapcar (alexandria:rcurry #'funcall stream) args)))))

(defgeneric get-slice (object slice)
  (:method ((object list) (slice slice))
    (let ((stride (or (stride slice) 1))
          (start (or (lower-bound slice) 0))
          (end (or (upper-bound slice) (length object))))
      (if (minusp stride)
          (loop for index
             from (1- end)
             by (abs stride)
             above (1- start)
             collecting (elt object index))
          (loop for index
             from start
             by stride
             below end
             collecting (elt object index)))))
  (:method ((object string) (slice slice))
    (let ((chars (get-slice (coerce object 'list) slice)))
      (coerce chars 'string))))

(defmethod compile-tagged-element ((tag (eql :slicing)) rest)
  (let ((primary (compile-element (first rest)))
        (slice (compile-element (second rest))))
    (alexandria:named-lambda :slicing (stream)
      (get-slice (funcall primary stream)
                 (funcall slice stream)))))

(defmethod compile-tagged-element ((tag (eql :raw)) rest)
  (alexandria:named-lambda :raw (stream)
    (princ (first rest) stream)))

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

(defmethod compile-tagged-element ((tag (eql :assign)) rest) ; FIXME save in compilation environment for exporting
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

(defmethod compile-tagged-element ((tag (eql :block-set)) rest) ; FIXME save in compilation environment for exporting
  (let ((target (first rest))
        (filter (loop for (name  args) in (rest (second rest))
                     collect (list name (mapcar #'compile-element args))))
        (suite (compile-element (third rest))))
    (alexandria:named-lambda :block-set (stream)
      (setf (load-value *context* target)
            (apply-filters
             (lambda (stream)
               (declare (ignore stream))
               (with-output-to-string (s)
                 (funcall suite s)))
             filter stream)))))

(defmethod compile-tagged-element ((tag (eql :with)) rest)
  (let ((assignments
         (loop for (target exprs) in (first rest)
            collecting (list target (compile-element exprs))))
        (suite (compile-element (second rest))))
    (alexandria:named-lambda :with (stream)
      (let ((scope '())) ; FIXME environment
        (loop for (target exprs) in assignments
           if (= 1 (length target))
           do (setf (load-value scope (first target))
                    (funcall exprs stream))
           else
           do (let ((seq (funcall exprs stream)))
                (dotimes (index (length target))
                  (setf (load-value scope (elt target index))
                        (elt seq index))))
           finally
             (let ((*context* (make-context *context* scope)))
               (funcall suite stream)))))))

(defmethod compile-tagged-element ((tag (eql :include)) rest)
  (let ((expr (compile-element (first rest)))
        (ignore-missing (second rest))
        (context (third rest)))
    (alexandria:named-lambda :include (stream)
      (let* ((candidates (funcall expr stream))
             (compiled-template
              (if (listp candidates)
                  (loop for name in candidates 
                     when (load-template *loader* name)
                     return it)
                  (load-template *loader* candidates))))
        (if (null compiled-template)
            (if ignore-missing
                nil
                (error "Missing template: ~A" candidates))
            (if context
                (funcall compiled-template stream)
                (let ((*context* nil)) ; FIXME basic context with standard globals
                  (funcall compiled-template stream))))))))

(defun save-block (name suite)
  (setf (load-value *blocks* name) suite))

(defun find-block (name)
  (load-value *blocks* name))

(defmethod compile-tagged-element ((tag (eql :block)) rest)
  (let ((name (first rest))
        (scoped (second rest))
        (suite (compile-element (third rest))))
    (save-block name suite)
    (alexandria:named-lambda :block (stream)
      (let ((compiled-block (find-block name)))
        (if scoped
            (funcall compiled-block stream)
            (let ((*context* nil)) ; FIXME basic context with standard globals
              (funcall compiled-block stream)))))))

(define-condition extending () ())

(defmethod compile-tagged-element ((tag (eql :extends)) rest)
  (let ((template-name (compile-element (first rest))))
    (alexandria:named-lambda :extends (stream)
      (let ((super-template (funcall template-name stream)))
        (etypecase super-template
          (string (setf super-template (load-template *loader* super-template)))
          (compiled-template nil))
        (if (null super-template)
            (error "Missing super template: ~A" template-name)
            (let ((*blocks* (append *blocks* (template-blocks super-template))))
              (funcall super-template stream)
              (signal 'extending)))))))

(defun split-parameters (parameters)
  (loop with start-keywords = nil
           for (name default supplied) in parameters
           when supplied do (setf start-keywords t)
           if start-keywords
           collect (or (and default
                            (list name
                                  (let ((s (make-string-output-stream))) ; FIXME kludge
                                    (funcall (compile-element default) s))))
                       name)
           into keywords
           else collect name into positionals
             finally (return (list positionals keywords))))

(defun getf-name (plist name)
  (loop for (key val) on plist by #'cddr
     when (name-equal name key)
     do (return-from getf-name (values val t))
     finally (return (values nil nil))))

(defmethod compile-tagged-element ((tag (eql :macro)) rest)
    (let ((name (first rest))
          (parameters (second rest))
          (body (compile-element (third rest))))
      (destructuring-bind (positionals keywords)
          (split-parameters parameters)
        (setf (load-value *context* name)
              (lambda (&rest args)
                (with-output-to-string (stream)
                  (let ((scope (make-hash-table :test 'equal)))
                    (loop for param in positionals
                       for arg in args
                       do (setf (load-value scope param) arg))
                    (when keywords
                      (loop with keyargs = (subseq args (length positionals))
                         for (name value) in keywords
                         do (setf (load-value scope name)
                                  (or (getf-name keyargs name) value))))
                    (let ((*context* (make-context *context* scope)))
                      (funcall body stream)))))))
      (constantly nil)))

(defgeneric to-list (thing) ; FIXME more general iterable?
  (:method (thing)
    (coerce thing 'list))
  (:method ((thing hash-table))
    (alexandria:hash-table-alist thing)))

(defmethod compile-tagged-element ((tag (eql :for)) rest)
  (let ((targets (first rest))
        (source (compile-element (second rest)))
        (filter (alexandria:when-let ((it (third rest)))
                  (compile-element it)))
        (body (compile-element (fourth rest)))
        (else (alexandria:when-let ((it (fifth rest)))
                (compile-element it)))
        (recursivep (sixth rest)))
    (declare (ignore recursivep))
    (alexandria:named-lambda :for (stream)
      (let ((s (to-list (funcall source stream))))
        (if (null s)
            (and else (funcall else stream))
            (let ((scope (make-hash-table :test 'equal))
                  (loop-dict (make-hash-table :test 'equal)))
              (dotimes (index (length targets))
                (setf (load-value scope (elt targets index))
                      nil))
              (setf (load-value scope "loop") loop-dict)
              (let ((*context* (make-context *context* scope)))
                (flet ((load-targets (this)
                         (if (serapeum:single targets)
                             (setf (load-value *context* (first targets))
                                   this)
                             (dotimes (i (length targets))
                               (setf (load-value *context* (elt targets i))
                                     (elt this i))))))
                  (when filter
                    (setf s (loop for item in s
                               do (load-targets item)                  
                               when (funcall filter stream)
                               collect item)))
                  (let ((length (length s)))
                    (setf (gethash "length" loop-dict)
                          length
                          (gethash "changed" loop-dict)
                          (alexandria:compose #'serapeum:true (serapeum:distinct)))
                    (loop 
                       for previtem = nil then this
                       for (this . rest) on s
                       for nextitem = (first rest)
                       for index from 1
                       for index0 from 0
                       for revindex downfrom length
                       for revindex0 downfrom (1- length)
                       for first = t then nil
                       for last = (= index length)
                       do (setf (gethash "index" loop-dict) index
                                (gethash "index0" loop-dict) index0
                                (gethash "revindex" loop-dict) revindex
                                (gethash "revindex0" loop-dict) revindex0
                                (gethash "first" loop-dict) first
                                (gethash "last" loop-dict) last
                                (gethash "previtem" loop-dict) previtem
                                (gethash "nextitem" loop-dict) nextitem)
                       ;; depth depth0 cycle
                         (load-targets this)
                         (funcall body stream)))))))))))





;;; compilation and rendering

(defvar *loader*)

(defclass dict-loader ()
  ((%dict :initarg :dict :initform (make-hash-table :test 'equal) :accessor template-dict)))

(defmethod print-object ((object dict-loader) stream)
  (print-unreadable-object (object stream :identity t :type t)
    (format stream "(~D templates)" (hash-table-count (template-dict object)))))

(defun make-dict-loader (&rest keys-vals)
  (make-instance 'dict-loader
                 :dict (apply #'serapeum:dict keys-vals)))

(defgeneric load-template (loader name &key &allow-other-keys))

(defmethod load-template ((dict-loader dict-loader) (name string) &key &allow-other-keys)
  (let ((string (gethash name (template-dict dict-loader))))
    (if string
        (compile-template-string string)
        nil)))

(defvar *blocks*)
(defvar *linked-templates*)
(defparameter *globals* (list "range" 'ginjish-builtins::range "lipsum" 'ginjish-builtins::lipsum))

(defun compile-template-string (string)
  (let* ((*blocks* '())
         (*linked-templates* '())
         (*context* (make-context *globals* (make-hash-table :test 'equal)))
         (*autoescape* *autoescape*))
    (let ((function (compile-element (esrap:parse 'ginjish-grammar::template string))))
      (make-instance 'compiled-template
                     :template-function function
                     :blocks *blocks*
                     :linked-templates *linked-templates*
                     :top-level (context-map *context*)))))

(defclass compiled-template ()
  ((%template-function :initarg :template-function :accessor template-function)
   (%blocks :initarg :blocks :accessor template-blocks)
   (%linked-templates :initarg :linked-templates :accessor template-linked-templates)
   (%top-level :initarg :top-level :accessor template-top-level))
  (:metaclass closer-mop:funcallable-standard-class))

(defmethod initialize-instance :after ((compiled-template compiled-template) &rest initargs)
  (declare (ignore initargs))
  (closer-mop:set-funcallable-instance-function
   compiled-template
   (template-function compiled-template)))

(defgeneric render-template (template stream context &key &allow-other-keys)
  (:method ((template compiled-template) stream context &key &allow-other-keys)
    (serapeum:with-string (s stream)
      (let* ((*blocks* (template-blocks template))
             (template-context (make-context *globals* (template-top-level template)))
             (*context* (make-context template-context context)))
        (handler-case
            (funcall template s)
          (extending () nil))))))


