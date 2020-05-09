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
      '(:call (:identifier "foo") ((:identifier "a") :a 1))
      (esrap:parse 'ginjish-grammar::call "foo(a,a=1)")))

(define-test filter
  :parent parser
  (is equal
      '(:filter (:identifier "foo") ("upper" ()) ("truncate" (12)))
      (esrap:parse 'ginjish-grammar::expression "foo | upper|truncate(12)")))

(define-test test
  :parent parser
  (is equal
      '(:test (:identifier "foo") ("zero" ()))
      (esrap:parse 'ginjish-grammar::expression "foo is zero"))
  (is equal
      '(:test-not (:identifier "foo") ("zero" ()))
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
      (ginjish-compiler::load-value (list "a" 1 'b 2) 'a))
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
      '(a 1 b 2)
      (let ((*context* '(b 2))) (setf (ginjish-compiler::load-value *context* 'a) 1) *context*))
  (is equal
      '(a 1 b nil)
      (let ((*context* '(a nil b nil)))
	(setf (ginjish-compiler::load-value *context* 'a) 1)
	*context*))
  (is equal
      '(a nil)
      (let ((*context* nil))
	(setf (ginjish-compiler::load-value *context* 'a) nil)
	*context*))
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

(define-test if-else
  :parent if*
  (is string=
      "..."
      (if-test-helper "{% if false %}XXX{% else %}...{% endif %}")))

(define-test if-empty
  :parent if*
  (is string=
      "[]"
      (if-test-helper "[{% if true %}{% else %}{% endif %}]")))

(define-test if-complete
  :parent if*
  (is string=
      "C"
      (if-test-helper "{% if a %}A{% elif b %}B{% elif c == d %}C{% else %}D{% endif %}" '("a" 0 "b" nil "c" 42 "d" 42.0))))

(define-test if-no-scope
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
      (if-test-helper "{% with a=42, b=23 %}{{ a }} = {{ b }}{% endwith %}|{{ a }} = {{ b }}"
		      '(a 1 b 2)))
  (is string=
      "1|2|3|4|5"
      (if-test-helper "{% with a=1, b=2, c=b, d=e, e=5 %}{{ a }}|{{ b }}|{{ c }}|{{ d }}|{{ e }}{% endwith %}"
		      '(b 3 e 4))))

(define-test for
  :parent compiler)

(define-test for-simple
  :parent for
  (is string=
      "0123456789"
      (if-test-helper "{% for item in seq %}{{ item }}{% endfor %}" `(seq ,(alexandria:iota 10)))))

(define-test for-else
  :parent for
  (is string=
      "..."
      (if-test-helper "{% for item in seq %}XXX{% else %}...{% endfor %}")))

(define-test for-else-scoping
  :parent for
  (is string=
      "42"
      (if-test-helper "{% for item in [] %}{% else %}{{ item }}{% endfor %}" '(item 42))))

(define-test for-empty-blocks
  :parent for
  (is string=
      "<>"
      (if-test-helper "<{% for item in seq %}{% else %}{% endfor %}>")))

	
(define-test for-context-vars
  :parent for
  (is string=
      "1|0|3|2|True|False|3###2|1|2|1|False|False|3###3|2|1|0|False|True|3###"
      (if-test-helper "{% for item in seq %}{{ loop.index }}|{{ loop.index0 }}|{{ loop.revindex }}|{{ loop.revindex0 }}|{{ loop.first }}|{{ loop.last }}|{{ loop.length }}###{% endfor %}"
		      (list 'seq '(1 2 3)))))

(define-test for-cycling
  :parent for
  (is string=
      "<1><2><1><2><1><2><1><2>"
      (if-test-helper "{% for item in seq %}{{loop.cycle('<1>', '<2>') }}{% endfor %}{% for item in seq %}{{ loop.cycle(*through) }}{% endfor %}"
		      (list 'seq (alexandria:iota 4) 'through (list "<1>" "<2>")))))

(define-test for-lookaround
  :parent for
  (is string=
      "x-0-1|0-1-2|1-2-3|2-3-x|"
      (if-test-helper "{% for item in seq %}{{ loop.previtem|default('x') }}-{{ item }}-{{ loop.nextitem|default('x') }}|{% endfor %}"
		      (list "seq" (alexandria:iota 4)))))

(define-test for-changed
  :parent for
  (is string=
      "True,False,True,True,False,True,True,False,False,"
      (if-test-helper "{% for item in seq %}{{ loop.changed(item) }},{% endfor %}"
		      (list 'seq nil nil 1 2 2 3 4 4 4))))

(define-test for-scope
  :parent for
  (is string=
      ""
      (if-test-helper "{% for item in seq %}{% endfor %}{{ item }}" (list 'seq (alexandria:iota 10)))))

(define-test for-varlen
  :parent for
  (is string=
      "01234"
      (if-test-helper "{% for item in iter %}{{ item }}{% endfor %}" (list 'iter (alexandria:iota 5)))))

(define-test for-noniter ; FIXME is nil a better response?
  :parent for
  (fail (if-test-helper "{% for item in none %}...{% endfor %}")))

(define-test for-recursive ; FIXME install function LOOP in global namespace to call this body?
  :parent for
  (is string=
      "[1<[1][2]>][2<[1][2]>][3<[a]>]"
      (if-test-helper "{% for item in seq recursive %}[{{ loop.previtem.a if loop.previtem is defined else 'x' }}.{{item.a }}.{{ loop.nextitem.a if loop.nextitem is defined else 'x'}}{% if item.b %}<{{ loop(item.b) }}>{% endif %}]{% endfor %}"
		      (list 'seq
			    (list 'a 1 'b (list (list 'a 1) (list 'a 2)))
			    (list 'a 2 'b (list (list 'a 1) (list 'a 2)))
			    (list 'a 3 'b (list (list 'a "a")))))))

(define-test for-recursive-lookaround
  :parent for
  (is string=
      "[x.1.2<[x.1.2][1.2.x]>][1.2.3<[x.1.2][1.2.x]>][2.3.x<[x.a.x]>]"
      (if-test-helper "{% for item in seq recursive %}[{{ loop.previtem.a if loop.previtem is defined else 'x' }}.{{
            item.a }}.{{ loop.nextitem.a if loop.nextitem is defined else 'x'
            }}{% if item.b %}<{{ loop(item.b) }}>{% endif %}]{% endfor %}"
		      (list 'seq
			    (list 'a 1 'b (list (list 'a 1) (list 'a 2)))
			    (list 'a 2 'b (list (list 'a 1) (list 'a 2)))
			    (list 'a 3 'b (list (list 'a "a")))))))

(define-test for-recursive-depth0
  :parent for
  (is string=
      "[0:1<[1:1][1:2]>][0:2<[1:1][1:2]>][0:3<[1:a]>]"
      (if-test-helper "{% for item in seq recursive %}[{{ loop.depth0 }}:{{ item.a }}{% if item.b %}<{{ loop(item.b) }}>{% endif %}]{% endfor %}"
		      (list 'seq
			    (list 'a 1 'b (list (list 'a 1) (list 'a 2)))
			    (list 'a 2 'b (list (list 'a 1) (list 'a 2)))
			    (list 'a 3 'b (list (list 'a "a")))))))

(define-test for-recursive-depth
  :parent for
  (is string=
      "[1:1<[2:1][2:2]>][1:2<[2:1][2:2]>][1:3<[2:a]>]"
      (if-test-helper "{% for item in seq recursive %}[{{ loop.depth }}:{{ item.a }}{% if item.b %}<{{ loop(item.b) }}>{% endif %}]{% endfor %}"
		      (list 'seq
			    (list 'a 1 'b (list (list 'a 1) (list 'a 2)))
			    (list 'a 2 'b (list (list 'a 1) (list 'a 2)))
			    (list 'a 3 'b (list (list 'a "a")))))))

(define-test for-looploop
  :parent for
  (is string=
      "[1|1][1|2][2|1][2|2]"
      (if-test-helper "{% for row in table %}{% set rowloop = loop %}{% for cell in row %}[{{ rowloop.index }}|{{ loop.index }}]{% endfor %}{% endfor %}"
		      (list 'table (list "ab" "cd")))))

(define-test for-reversed-bug
  :parent for
  (is string=
      "1,2,3"
      (if-test-helper "{% for i in items %}{{ i }}{% if not loop.last %},{% endif %}{% endfor %}"
		      (list 'items (reverse (list 3 2 1))))))

(define-test for-loop-errors
  :parent for
  (fail (if-test-helper "{% for item in [1] if loop.index
                                      == 0 %}...{% endfor %}")))

(define-test for-loop-filter ; test, rather
  :parent for
  (is string=
      "[0][2][4][6][8]"
      (if-test-helper "{% for item in range(10) if item is even %}[{{ item }}]{% endfor %}")))

(define-test for-loop-unassignable ; FIXME is nil a better answer?
  :parent for
  (fail (if-test-helper "{% for loop in seq %}...{% endfor %}")))

(define-test for-scoped-special-var
  :parent for
  (is string=
      "[True|True|False][False|True|False]"
      (if-test-helper "{% for s in seq %}[{{ loop.first }}{% for c in s %}|{{ loop.first }}{% endfor %}]{% endfor %}"
		      (list 'seq (list "ab" "cd")))))

(define-test for-scoped-loop-var
  :parent for
  (is string=
      "TrueFalse"
      (if-test-helper "{% for x in seq %}{{ loop.first }}{% for y in seq %}{% endfor %}{% endfor %}"
		      (list 'seq "ab")))
  (is string=
      "TrueFalseTrueFalse"
      (if-test-helper "{% for x in seq %}{% for y in seq %}{{ loop.first }}{% endfor %}{% endfor %}"
		      (list 'seq "ab"))))

(define-test for-recursive-empty-loop-iter
  :parent for
  (is string=
      ""
      (if-test-helper "{% for item in foo recursive %}{% endfor %}"
		      (list 'foo nil))))

;; TODO call_in_loop scoping_bug

(define-test for-unpacking
  :parent for
  (is string=
      "1|2|3"
      (if-test-helper "{% for a, b, c in [[1, 2, 3]] %}{{ a }}|{{ b }}|{{ c }}{% endfor %}")))

(define-test for-intended-scoping-with-set
  :parent for
  (is string=
      "010203"
      (if-test-helper "{% for item in seq %}{{ x }}{% set x = item %}{{ x }}{% endfor %}"
		      (list 'x 0 'seq (list 1 2 3))))
  (is string=
      "919293"
      (if-test-helper "{% set x = 9 %}{% for item in seq %}{{ x }}{% set x = item %}{{ x }}{% endfor %}"
		      (list 'x 0 'seq (list 1 2 3)))))
