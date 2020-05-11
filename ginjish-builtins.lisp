;;;; ginjish-builtins.lisp

(cl:in-package #:ginjish-builtins)

(defmacro define-filter (name args &body body)
  (let* ((filter-package (find-package "GINJISH.FILTERS"))
	 (function-name (intern (symbol-name name) filter-package)))
    (multiple-value-bind (body decls docstring)
	(alexandria:parse-body body :documentation t)
      `(progn
	 (defun ,function-name (,@args) ; FIXME error handling
	   ,@(serapeum:unsplice docstring)
	   ,@decls
	   ,@body)
	 (export ',function-name ,filter-package)))))

(defmacro define-test (name args &body body)
  (let* ((test-package (find-package "GINJISH.TESTS"))
	 (function-name (intern (symbol-name name) test-package)))
    (multiple-value-bind (body decls docstring)
	(alexandria:parse-body body :documentation t)
      `(progn
	 (defun ,function-name (,@args) ; FIXME error handling
	   ,@(serapeum:unsplice docstring)
	   ,@decls
	   ,@body)
	 (export ',function-name ,test-package)))))

;;; filters

#|

abs
attr
batch
capitalize
center
count
d
default
dictsort
e
escape
filesizeformat
first
float
forceescape
format
groupby
indent
int
join
last
length
list
lower
map
min
max
pprint
random
reject
rejectattr
replace
reverse
round
safe
select
selectattr
slice
sort
string
striptags
sum
title
trim
truncate
unique
upper
urlencode - see do-urlencode
urlize
wordcount
wordwrap
xmlattr
tojson

|#

(define-filter abs (n)
  (abs n))

;;; TODO attr

(defgeneric make-sequence-like (seq len &key initial-element)
  (:method ((seq list) len &key initial-element)
    (make-list len :initial-element initial-element))
  (:method ((seq string) len &key initial-element)
    (make-string len :initial-element initial-element)))

(defun do-batch (seq n &optional fill)
  (let ((batches (serapeum:batches seq n))
	(rem (rem (length seq) n)))
    (if (and fill (not (zerop rem)))
	(let ((last (alexandria:lastcar batches))
	      (new (make-sequence-like seq n :initial-element fill)))
	  (dotimes (i rem)
	    (setf (elt new i) (elt last i)))
	  (append (butlast batches) (list new)))
	batches)))

(define-filter batch (seq n &optional fill)
  (do-batch seq n fill))

(defun do-slice (seq n &optional fill)
  (let ((length (length seq)))
    (multiple-value-bind (normal-bucket-size oversized-buckets)
	(truncate length n)
      (let ((bucket-sizes (make-list n :initial-element normal-bucket-size)))
	(dotimes (i oversized-buckets)
	  (incf (elt bucket-sizes i)))
	(loop for start = 0 then (+ start delta)
	   for delta in bucket-sizes
	   collect (if (and fill (= delta normal-bucket-size))
		       (append (subseq seq start (+ start delta)) (list fill))
		       (subseq seq start (+ start delta))))))))

(define-filter slice (seq n &optional fill)
  (do-slice seq n fill))

(define-filter capitalize (s)
  (if (zerop (length s))
      s
      (replace s (string-upcase (elt s 0)) :end1 1)))

(defun do-center (s &optional (width 80))
  (let* ((len (length s))
	 (remainder (- width len))
	 (leading (floor remainder 2))
	 (total (+ leading len)))
    (serapeum:pad-end (serapeum:pad-start s total) width)))

(define-filter center (s &optional (width 80))
  (do-center s width))

(defun do-default (thing default &optional undefined)
  (declare (ignore undefined)) ; FIXME - change from jinja
  (if thing ; FIXME - is 0 truthy?
      thing
      default))

(define-filter default (thing default &optional undefined)
  (do-default thing default undefined))

(define-filter d (thing default)
  (do-default thing default))

(defgeneric thing< (left right)
  (:method ((left number) (right number))
    (< left right))
  (:method ((left string) (right string))
    (string< left right))
  (:method (left right)
    (string< (princ-to-string left) (princ-to-string right))))

(defgeneric thing> (left right)
  (:method ((left number) (right number))
    (> left right))
  (:method ((left string) (right string))
    (string> left right))
  (:method (left right)
    (string> (princ-to-string left) (princ-to-string right))))

(defgeneric thing-lessp (left right)
  (:method ((left number) (right number))
    (< left right))
  (:method ((left string) (right string))
    (string-lessp left right))
  (:method (left right)
    (string-lessp (princ-to-string left) (princ-to-string right))))

(defgeneric thing-greaterp (left right)
  (:method ((left number) (right number))
    (> left right))
  (:method ((left string) (right string))
    (string-greaterp left right))
  (:method (left right)
    (string-greaterp (princ-to-string left) (princ-to-string right))))

(defgeneric do-dictsort (dict &key case-sensitive by reverse)
  (:method ((dict hash-table) &key case-sensitive (by :key) reverse) ; FIXME alist / plist
    (let ((alist (alexandria:hash-table-alist dict))
	  (key (ecase by (:key #'car)(:value #'cdr)))
	  (pred (cond ((and case-sensitive reverse) #'thing>)
		      (case-sensitive #'thing<)
		      (reverse #'thing-greaterp)
		      (t #'thing-lessp))))
      (sort alist pred :key key))))

(defun ensure-keyword (thing) ; FIXME non-strings; general calling convention
  (if (keywordp thing)
      thing
      (ginjish-grammar::read-keyword thing)))

(define-filter dictsort (dict &key case_sensitive (by :key) reverse)
  (do-dictsort dict :case-sensitive case_sensitive
	       :by (ensure-keyword by)
	       :reverse reverse))

(defparameter *escape-table*
  (serapeum:dict 'eql
   #\& "&amp;"
   #\< "&lt;"
   #\> "&gt;"
   #\' "&apos;"
   #\" "&#34;"))

(defun escape (s)
  (serapeum:escape s *escape-table*)) ; FIXME already escaped strings

(define-filter escape (s)
  (escape s))

(define-filter e (s)
  (escape s))

(define-filter trim (s &optional (chars " "))
  (string-trim chars s))

(defun do-striptags (s)
  (let ((root (plump:parse s)))
    (serapeum:trim-whitespace
     (serapeum:collapse-whitespace
      (with-output-to-string (stream)
	(labels ((r (node)
		   (loop for child across (plump:children node)
		      do (typecase child
			   (plump:comment nil)
			  (plump:textual-node (write-string (plump:text child) stream))
			  (plump:nesting-node (r child))))))
	  (r root)))))))

(define-filter striptags (s)
  (do-striptags s))

(define-filter filesizeformat (s &optional iec)
  (serapeum:format-file-size-human-readable nil s :flavor (if iec :iec :si) :space t :suffix "B"))

(define-filter first (seq)
  (elt seq 0))

(defun do-float (n &optional (default 0.0))
  (handler-bind ((type-error
		  #'(lambda (condition)
		      (declare (ignore condition))
		      (return-from do-float default))))
    (float n 0d0))) ; FIXME double?

(define-filter float (n &optional (default 0.0))
  (do-float n default))

(define-filter forceescape (s)
  (escape s)) ; FIXME

(defun do-format (s format-arguments) ; FIXME ????
  (apply #'format nil s format-arguments))

(define-filter format (s &rest format-arguments)
  (do-format s format-arguments))

(define-filter length (seq)
  (length seq))

(define-filter list (thing)
  (ginjish-compiler::to-list thing)) ; FIXME utils

(define-filter join (strings &optional sep)
  (serapeum:string-join strings sep))

;;; tests

(define-test even (n)
  (evenp n))

