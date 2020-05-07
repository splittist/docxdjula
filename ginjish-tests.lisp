;;;; ginjish-tests.lisp

(cl:defpackage #:ginjish-tests
  (:use #:cl #:parachute))

(cl:in-package #:ginjish-tests)

;;; parser

(define-test parser)

(define-test stringliteral
  :parent parser
  (is equal
      '(:string "abc")
      (esrap:parse 'ginjish-grammar::stringliteral "\"abc\""))
  (is equal
      '(:string "abc")
      (esrap:parse 'ginjish-grammar::stringliteral "'abc'")))

(define-test integer
  :parent parser
  (is =
      7
      (esrap:parse 'ginjish-grammar::integer "7"))
  (is =
      2147483647
      (esrap:parse 'ginjish-grammar::integer "2147483647"))
  (is =
      127
      (esrap:parse 'ginjish-grammar::integer "0o177"))
  (is =
      311
      (esrap:parse 'ginjish-grammar::integer "0b100110111"))
  (is =
      3
      (esrap:parse 'ginjish-grammar::integer "3"))
  (is =
      79228162514264337593543950336
      (esrap:parse 'ginjish-grammar::integer "79228162514264337593543950336"))
  (is =
      255
      (esrap:parse 'ginjish-grammar::integer "0o377"))
  (is =
      3735928559
      (esrap:parse 'ginjish-grammar::integer "0xdeadbeef"))
  (is =
      100000000000
      (esrap:parse 'ginjish-grammar::integer "100_000_000_000"))
  (is =
      229
      (esrap:parse 'ginjish-grammar::integer "0b_1110_0101")))

(define-test floatnumber
  :parent parser
  (is =
      3.14
      (esrap:parse 'ginjish-grammar::floatnumber "3.14"))
  (is =
      10.0
      (esrap:parse 'ginjish-grammar::floatnumber "10."))
  (is =
      0.001
      (esrap:parse 'ginjish-grammar::floatnumber ".001"))
  (is =
      1d100
      (esrap:parse 'ginjish-grammar::floatnumber "1e100"))
  (is =
      3.14e-10
      (esrap:parse 'ginjish-grammar::floatnumber "3.14e-10"))
  (is =
      0.0
      (esrap:parse 'ginjish-grammar::floatnumber "0e0"))
  (is =
      3.141593
      (esrap:parse 'ginjish-grammar::floatnumber "3.14_15_93")))
      
(define-test identifier
  :parent parser
  (is eq
      t
      (esrap:parse 'ginjish-grammar::identifier "True"))
  (is eq
      t
      (esrap:parse 'ginjish-grammar::identifier "true"))
  (is eq
      nil
      (esrap:parse 'ginjish-grammar::identifier "False"))
  (is eq
      nil
      (esrap:parse 'ginjish-grammar::identifier "false"))
  (is equal
      '(:identifier "foo")
      (esrap:parse 'ginjish-grammar::identifier "foo"))
  (is equal
      '(:identifier "__foo47__")
      (esrap:parse 'ginjish-grammar::identifier "__foo47__")))

(define-test literal
  :parent parser
  (is =
      3
      (esrap:parse 'ginjish-grammar::literal "3"))
  (is equal
      '(:string "foo")
      (esrap:parse 'ginjish-grammar::literal "'foo'")))

(define-test expression
  :parent parser
  (is equal
      '(:minus (:plus 3 4) 7)
      (esrap:parse 'ginjish-grammar::expression "3 + 4 - 7"))
  (is equal
      '(:plus 3 (:mul 4 7))
      (esrap:parse 'ginjish-grammar::expression "3 + 4 * 7"))
  (is equal
      '(:mul 3 (:minus 4 7))
      (esrap:parse 'ginjish-grammar::expression "3 * (4 - 7)"))
  (is equal
      '(:if-expr (:lt (:identifier "a") (:identifier "b"))
	t
	(:get-item
	 (:get-attr (:identifier "default") (:identifier "x"))
	 (3 4)))
      (esrap:parse 'ginjish-grammar::expression "true if a < b else default.x[3,4]"))
  (is equal
      '(:if-expr (:lt (:identifier "a") (:identifier "b"))
	t
	(:slicing
	 (:get-attr (:identifier "default") (:identifier "x"))
	 (:proper-slice 3 4 nil)))
      (esrap:parse 'ginjish-grammar::expression "true if a < b else default.x[3:4]")))

(define-test enclosure
  :parent parser
  (is equal
      1
      (esrap:parse 'ginjish-grammar::enclosure "(1)"))
  (is equal
      '(:tuple 1)
      (esrap:parse 'ginjish-grammar::enclosure "(1,)"))
  (is equal
      '(:list 1)
      (esrap:parse 'ginjish-grammar::enclosure "[1]"))
  (is equal
      '(:set 1)
      (esrap:parse 'ginjish-grammar::enclosure "{1}"))
  (is equal
      '(:dict (1 2))
      (esrap:parse 'ginjish-grammar::enclosure "{1:2}"))
  (is equal
      '(:list 1 2 3)
      (esrap:parse 'ginjish-grammar::enclosure "[1, 2 , 3 ]")))

(define-test call
  :parent parser
  (is equal
      '(:call (:identifier "foo") (1 2))
      (esrap:parse 'ginjish-grammar::call "foo(1,2)"))
  (is equal
      '(:call (:identifier "foo") ((:identifier "a") (:a 1)))
      (esrap:parse 'ginjish-grammar::call "foo(a,a=1)")))

(define-test filter
  :parent parser
  (is equal
      '(:filter (:identifier "foo") (:identifier "upper") (:call (:identifier "truncate") (12)))
      (esrap:parse 'ginjish-grammar::expression "foo | upper|truncate(12)")))

(define-test test
  :parent parser
  (is equal
      '(:test (:identifier "foo") (:identifier "zero"))
      (esrap:parse 'ginjish-grammar::expression "foo is zero"))
  (is equal
      '(:test-not (:identifier "foo") (:identifier "zero"))
      (esrap:parse 'ginjish-grammar::expression "foo is not zero")))

;;; compiler

(define-test compiler)

(defclass load-value-example ()
  ((a :initarg :a)
   (b :initarg :b)))

(define-test load-value
  :parent compiler
  (is equal
      1
      (ginjish-compiler::load-value '(1 3 3) 0))
  (is equal
      #\f
      (ginjish-compiler::load-value "foo" 0))
  (is equal
      1
      (ginjish-compiler::load-value '(a 1 b 2) 'a))
  (is equal
      1
      (ginjish-compiler::load-value '((a . 1) (b . 2)) 'a))
  (is equal
      1
      (ginjish-compiler::load-value (serapeum:dict "a" 1 "b" 2) "a"))
  (is equal
      1
      (ginjish-compiler::load-value (make-instance 'load-value-example :a 1 :b 2) 'a)))

(define-test setf-load-value
  :parent compiler
  (is equal
      '(1 3 3)
      (let ((*context* '(2 3 3))) (setf (ginjish-compiler::load-value *context* 0) 1) *context*))
  (is equal
      '(a 1)
      (let ((*context* nil)) (setf (ginjish-compiler::load-value *context* 'a) 1) *context*))
  (is equal
      "foo"
      (let ((*context* "woo")) (setf (ginjish-compiler::load-value *context* 0) #\f) *context*))
  (is equal
      '((a . 2) (b . 2))
      (let ((*context* '((a . 1) (b . 2))))
	(setf (ginjish-compiler::load-value *context* 'a) 2)
	*context*))
  (is equal
      '(("a" . 2) ("b" . 2))
      (let ((*context* (serapeum:dict "a" 1 "b" 2)))
	(setf (ginjish-compiler::load-value *context* "a") 2)
	(sort (alexandria:hash-table-alist *context*) #'string< :key #'car)))
  (is equal
      2
      (let ((*context* (make-instance 'load-value-example :a 1 :b 2)))
	(setf (ginjish-compiler::load-value *context* 'a) 2)
	(slot-value *context* 'a))))
  

(define-test truthy
  :parent compiler
  (false (ginjish-compiler::truthy nil))
  (true (ginjish-compiler::truthy t))
  (false (ginjish-compiler::truthy 0))
  (true (ginjish-compiler::truthy 1))
  (false (ginjish-compiler::truthy 0.0))
  (true (ginjish-compiler::truthy 1.0))
  (false (ginjish-compiler::truthy ""))
  (true (ginjish-compiler::truthy "a"))
  (false (ginjish-compiler::truthy #()))
  (true (ginjish-compiler::truthy #(1)))
  (false (ginjish-compiler::truthy (make-hash-table)))
  (true (ginjish-compiler::truthy (serapeum:dictq a 1))))
      
(define-test gte
  :parent compiler
  (true (ginjish-compiler::gte 1 0))
  (false (ginjish-compiler::gte 0 1))
  (true (ginjish-compiler::gte 1 1))
  (true (ginjish-compiler::gte "foo" "bar"))
  (false (ginjish-compiler::gte "bar" "foo"))
  (true (ginjish-compiler::gte "bar" "bar"))
  (true (ginjish-compiler::gte '(1 2) '(1)))
  (false (ginjish-compiler::gte '(1) '(1 2)))
  (true (ginjish-compiler::gte '(2) '(1)))
  (true (ginjish-compiler::gte '(1 2 3) '(1 2 3)))
  (true (ginjish-compiler::gte '(2) '(1 2 3)))
  (true (ginjish-compiler::gte '(1 (2) 3) '(1 (1) 3))))

(defun if-test-helper (string &optional env)
  (let ((ginjish-compiler::*context* env))
    (with-output-to-string (s)
      (funcall (ginjish-compiler::compile-element
		(esrap:parse 'ginjish-grammar::suite
			     string))
	       s))))

(define-test if*
  :parent compiler)

(define-test if-simple
  :parent if*
  (is string=
      "..."
      (if-test-helper "{% if true %}...{% endif %}")))

(define-test if-elif
  :parent if*
  (is string=
      "..."
      (if-test-helper "{% if false %}XXX{% elif true %}...{% else %}XXX{% endif %}")))

(define-test if-elif-deep
  :parent if*
  (is string=
      "0"
      (if-test-helper (format nil "{% if a == 0 %}0~{{% elif a == ~D %}~:*~D~}{% else %}x{% endif %}" (alexandria:iota 999 :start 1)) '("a" 0)))
  (is string=
      "10"
      (if-test-helper (format nil "{% if a == 0 %}0~{{% elif a == ~D %}~:*~D~}{% else %}x{% endif %}" (alexandria:iota 999 :start 1)) '("a" 10)))
  (is string=
      "999"
      (if-test-helper (format nil "{% if a == 0 %}0~{{% elif a == ~D %}~:*~D~}{% else %}x{% endif %}" (alexandria:iota 999 :start 1)) '("a" 999)))
  (is string=
      "x"
      (if-test-helper (format nil "{% if a == 0 %}0~{{% elif a == ~D %}~:*~D~}{% else %}x{% endif %}" (alexandria:iota 999 :start 1)) '("a" 1000))))

(define-test else
  :parent if*
  (is string=
      "..."
      (if-test-helper "{% if false %}XXX{% else %}...{% endif %}")))

(define-test empty
  :parent if*
  (is string=
      "[]"
      (if-test-helper "[{% if true %}{% else %}{% endif %}]")))

(define-test complete
  :parent if*
  (is string=
      "C"
      (if-test-helper "{% if a %}A{% elif b %}B{% elif c == d %}C{% else %}D{% endif %}" '("a" 0 "b" nil "c" 42 "d" 42.0))))

(define-test no-scope
  :parent if*
  (is string=
      "1"
      (if-test-helper "{% if a %}{% set foo = 1 %}{% endif %}{{ foo }}" '("a" t)))
  (is string=
      "1"
      (if-test-helper "{% if true %}{% set foo = 1 %}{% endif %}{{ foo }}")))

(define-test with
  :parent compiler
  (is string=
      "42 = 23|1 = 2"
      (
