;;;; ginjish-tests.lisp

(cl:defpackage #:ginjish-tests
  (:use #:cl #:parachute))

(cl:in-package #:ginjish-tests)

(define-test all)

;;; parser

(define-test parser
  :parent all)

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
      '(:invoke (:identifier "foo") (1 2))
      (esrap:parse 'ginjish-grammar::call "foo(1,2)"))
  (is equal
      '(:invoke (:identifier "foo") ((:identifier "a") :a 1))
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

(define-test compiler
  :parent all)

(defclass load-value-example ()
  ((a :initarg :a :accessor lve-a)
   (b :initarg :b :accessor lve-b)))

(defclass load-value-example2 (load-value-example)
  ((c :initarg :c :accessor lve-c)))

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
      (ginjish-compiler::load-value (make-instance 'load-value-example :a 1 :b 2) 'a))
  (is equal
      1
      (ginjish-compiler::load-value (make-instance 'load-value-example2 :a 1 :b 2 :c 3) 'a))
  (is equal
      1
      (ginjish-compiler::load-value (make-instance 'load-value-example :a 1 :b 2) 'lve-a))
  (is equal
      1
      (ginjish-compiler::load-value (make-instance 'load-value-example2 :a 1 :b 2 :c 3) 'lve-a)))

(define-test setf-load-value
  :parent compiler
  (is equal
      '(1 3 3)
      (let ((*context* (list 2 3 3))) (setf (ginjish-compiler::load-value *context* 0) 1) *context*))
  (is equal
      '(a 1)
      (let ((*context* nil)) (setf (ginjish-compiler::load-value *context* 'a) 1) *context*))
  (is equal
      '(a 1 b 2)
      (let ((*context* (list 'b 2))) (setf (ginjish-compiler::load-value *context* 'a) 1) *context*))
  (is equal
      '(a 1 b nil)
      (let ((*context* (list 'a nil 'b nil)))
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
      (let ((*context* (list (cons 'a  1) (cons 'b  2))))
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
        (slot-value *context* 'a)))
  (is equal
      2
      (let ((*context* (make-instance 'load-value-example :a 1 :b 2)))
        (setf (ginjish-compiler::load-value *context* 'lve-a) 2)
        (lve-a *context*)))
  (is equal
      2
      (let ((*context* (make-instance 'load-value-example2 :a 1 :b 2 :c 3)))
        (setf (ginjish-compiler::load-value *context* 'lve-a) 2)
        (lve-a *context*))))

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
                (esrap:parse 'ginjish-grammar::template
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
  (skip "TODO do later"
    (is string=
        "<1><2><1><2><1><2><1><2>"
        (if-test-helper "{% for item in seq %}{{loop.cycle('<1>', '<2>') }}{% endfor %}{% for item in seq %}{{ loop.cycle(through[1],through[2]) }}{% endfor %}"
                        (list 'seq (alexandria:iota 4) 'through (list "<1>" "<2>"))))))

(define-test for-lookaround
  :parent for
  (is string=
      "x-0-1|0-1-2|1-2-3|2-3-x|"
      (if-test-helper "{% for item in seq %}{{ loop.previtem|default('x') }}-{{ item }}-{{ loop.nextitem|default('x') }}|{% endfor %}"
                      (list "seq" (alexandria:iota 4)))))

(define-test for-changed ; FIXME - loop.changed(nil) is always False
  :parent for
  (is string=
      "True,False,True,True,False,True,True,False,False,"
      (if-test-helper "{% for item in seq %}{{ loop.changed(item) }},{% endfor %}"
                      (list 'seq '(t t 1 2 2 3 4 4 4)))))

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
  (skip "TODO do later"
    (is string=
        "[1<[1][2]>][2<[1][2]>][3<[a]>]"
        (if-test-helper "{% for item in seq recursive %}[{{ loop.previtem.a if loop.previtem is defined else 'x' }}.{{item.a }}.{{ loop.nextitem.a if loop.nextitem is defined else 'x'}}{% if item.b %}<{{ loop(item.b) }}>{% endif %}]{% endfor %}"
                        (list 'seq
                              (list 'a 1 'b (list (list 'a 1) (list 'a 2)))
                            (list 'a 2 'b (list (list 'a 1) (list 'a 2)))
                            (list 'a 3 'b (list (list 'a "a"))))))))

(define-test for-recursive-lookaround
  :parent for
  (skip "TODO do later"
    (is string=
        "[x.1.2<[x.1.2][1.2.x]>][1.2.3<[x.1.2][1.2.x]>][2.3.x<[x.a.x]>]"
        (if-test-helper "{% for item in seq recursive %}[{{ loop.previtem.a if loop.previtem is defined else 'x' }}.{{
            item.a }}.{{ loop.nextitem.a if loop.nextitem is defined else 'x'
            }}{% if item.b %}<{{ loop(item.b) }}>{% endif %}]{% endfor %}"
                        (list 'seq
                              (list 'a 1 'b (list (list 'a 1) (list 'a 2)))
                              (list 'a 2 'b (list (list 'a 1) (list 'a 2)))
                              (list 'a 3 'b (list (list 'a "a"))))))))

(define-test for-recursive-depth0
  :parent for
  (skip "TODO do later"
    (is string=
        "[0:1<[1:1][1:2]>][0:2<[1:1][1:2]>][0:3<[1:a]>]"
        (if-test-helper "{% for item in seq recursive %}[{{ loop.depth0 }}:{{ item.a }}{% if item.b %}<{{ loop(item.b) }}>{% endif %}]{% endfor %}"
                        (list 'seq
                              (list 'a 1 'b (list (list 'a 1) (list 'a 2)))
                              (list 'a 2 'b (list (list 'a 1) (list 'a 2)))
                              (list 'a 3 'b (list (list 'a "a"))))))))

(define-test for-recursive-depth
  :parent for
  (skip "TODO do later"
    (is string=
        "[1:1<[2:1][2:2]>][1:2<[2:1][2:2]>][1:3<[2:a]>]"
        (if-test-helper "{% for item in seq recursive %}[{{ loop.depth }}:{{ item.a }}{% if item.b %}<{{ loop(item.b) }}>{% endif %}]{% endfor %}"
                        (list 'seq
                              (list 'a 1 'b (list (list 'a 1) (list 'a 2)))
                              (list 'a 2 'b (list (list 'a 1) (list 'a 2)))
                              (list 'a 3 'b (list (list 'a "a"))))))))

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

(define-test for-loop-errors ; FIXME will <undefined> help?
  :parent for
  (fail (if-test-helper "{% for item in [1] if loop.index
                                      == 0 %}...{% endfor %}")))

(define-test for-loop-filter ; test, rather
  :parent for
  (is string=
      "[0][2][4][6][8]"
      (if-test-helper "{% for item in range(10) if item is even %}[{{ item }}]{% endfor %}"
                      (list (cons "range" 'ginjish-builtins::range)))))

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

;;; macro

(define-test macro
  :parent compiler)

(define-test macro-simple
  :parent macro
  (is string=
      "1,spam,eggs|2,spam,Ni|3,Ecki,eggs"
      (if-test-helper "{% macro foo(bar,baz='spam',quux='eggs') %}{{bar}},{{baz}},{{quux}}{% endmacro %}{{ foo(1)}}|{{foo(2,quux='Ni')}}|{{foo(3,baz='Ecki')}}"))
  (is string=
      "27,4,2"
      (if-test-helper "{% macro foo(bar,baz,quux) %}{{quux}},{{baz}},{{bar}}{% endmacro %}{{foo(1+1,2*2,3**3)}}")))

;;; include

(define-test include
  :parent compiler)

(define-test include-simple
  :parent include
  (is string=
      "hello world"
      (let ((ginjish-compiler::*loader* (ginjish-compiler::make-dict-loader "foo" "hello" "bar" "world")))
        (ginjish-compiler::render-template
         (ginjish-compiler::compile-template-string "{% include 'foo' %} {% include 'bar' %}") nil nil))))

(define-test include-list
  :parent include
  (is string=
      "hello world"
      (let ((ginjish-compiler::*loader* (ginjish-compiler::make-dict-loader "foo" "hello" "bar" "world")))
        (ginjish-compiler::render-template
         (ginjish-compiler::compile-template-string "{% include 'foo' %} {% include ['bar','quux'] %}") nil nil))))

(define-test include-missing
  :parent include
  (is string=
      "hello world"
      (let ((ginjish-compiler::*loader* (ginjish-compiler::make-dict-loader "foo" "hello" "bar" "world")))
        (ginjish-compiler::render-template
         (ginjish-compiler::compile-template-string "{% include 'foo' %} {% include ['quux','bar'] %}") nil nil)))
  (fail
   (let ((ginjish-compiler::*loader* (ginjish-compiler::make-dict-loader "foo" "hello" "bar" "world")))
        (ginjish-compiler::render-template
         (ginjish-compiler::compile-template-string "{% include 'foo' %} {% include 'quux' %}") nil nil)))
  (is string=
      "hello "
      (let ((ginjish-compiler::*loader* (ginjish-compiler::make-dict-loader "foo" "hello" "bar" "world")))
        (ginjish-compiler::render-template
         (ginjish-compiler::compile-template-string "{% include 'foo' %} {% include 'quux' ignore missing %}") nil nil)))
  (is string=
      "hello "
      (let ((ginjish-compiler::*loader* (ginjish-compiler::make-dict-loader "foo" "hello" "bar" "world")))
        (ginjish-compiler::render-template
         (ginjish-compiler::compile-template-string "{% include 'foo' %} {% include ['quux'] ignore missing %}") nil nil))))

(define-test include-context
  :parent include
  (is string=
      "hello world"
      (let ((ginjish-compiler::*loader* (ginjish-compiler::make-dict-loader "foo" "{{ quux }}" "bar" "world")))
        (ginjish-compiler::render-template
         (ginjish-compiler::compile-template-string "{% include 'foo' %} {% include 'bar' %}") nil (list "quux" "hello"))))
  (is string=
      " world"
      (let ((ginjish-compiler::*loader* (ginjish-compiler::make-dict-loader "foo" "{{ quux }}" "bar" "world")))
        (ginjish-compiler::render-template
         (ginjish-compiler::compile-template-string "{% include 'foo' without context %} {% include 'bar' %}") nil (list "quux" "hello")))))


;;; block / extends

(defun block-test-helper (template-string context &rest templates)
  (let ((ginjish-compiler::*loader* (apply #'ginjish-compiler::make-dict-loader templates)))
    (ginjish-compiler::render-template
     (ginjish-compiler::compile-template-string template-string) nil context)))

(define-test block-extends
  :parent compiler)

(define-test block-simple
  :parent block-extends
  (is string=
      "hello world"
      (block-test-helper "{% block foo %}hello{% endblock %} {% block bar %}world{% endblock %}" nil)))

(define-test block-extends-simple
  :parent block-extends
  (is string=
      "hello world"
      (block-test-helper "{% extends 'foo' %}{% block a %}hello{% endblock %}{% block b %}world{% endblock %}" nil "foo" "{% block a %}{% endblock %} {% block b %}{% endblock %}"))
  (is string=
      "hello world"
      (block-test-helper "{% extends 'foo' %}{% block a %}hello{% endblock %}{% block b %}world{% endblock %}" nil "foo" "{% block a %}goodbye{% endblock %} {% block b %}potato{% endblock %}"))
  (is string=
      "hello potato"
      (block-test-helper "{% extends 'foo' %}{% block a %}hello{% endblock %}" nil "foo" "{% block a %}goodbye{% endblock %} {% block b %}potato{% endblock %}")))

(define-test block-extends-nested
  :parent block-extends
  (is string=
      "(hello world)"
      (block-test-helper "{% extends 'foo' %}{% block a %}hello{% endblock %}" nil
                         "foo" "{% extends 'bar' %}{% block b %}world{% endblock %}"
                         "bar" "({% block a %}{% endblock %} {% block b %}{% endblock %})")))

(defparameter LAYOUTTEMPLATE  "|{% block block1 %}block 1 from layout{% endblock %}
|{% block block2 %}block 2 from layout{% endblock %}
|{% block block3 %}
{% block block4 %}nested block 4 from layout{% endblock %}
{% endblock %}|")

(defparameter LEVEL1TEMPLATE "{% extends 'layout' %}
{% block block1 %}block 1 from level1{% endblock %}")

(defparameter LEVEL2TEMPLATE "{% extends 'level1' %}
{% block block2 %}{% block block5 %}nested block 5 from level2{%
endblock %}{% endblock %}")

(defparameter LEVEL3TEMPLATE "{% extends 'level2' %}
{% block block5 %}block 5 from level3{% endblock %}
{% block block4 %}block 4 from level3{% endblock %}")

(defparameter LEVEL4TEMPLATE "{% extends 'level3' %}
{% block block3 %}block 3 from level4{% endblock %}")

(defparameter WORKINGTEMPLATE "{% extends 'layout' %}
{% block block1 %}
  {% if false %}
    {% block block2 %}
      this should workd
    {% endblock %}
  {% endif %}
{% endblock %}")

(defparameter DOUBLEEXTENDS "{% extends 'layout' %}
{% extends 'layout' %}
{% block block1 %}
  {% if false %}
    {% block block2 %}
      this should workd
    {% endblock %}
  {% endif %}
{% endblock %}")

(define-test block-extends-layout
  :parent block-extends
  (is string=
      "|block 1 from layout|block 2 from layout|nested block 4 from layout|"
      (let ((ginjish-compiler::*trim-blocks* t))
        (block-test-helper LAYOUTTEMPLATE nil
                           "layout" LAYOUTTEMPLATE
                           "level1" LEVEL1TEMPLATE
                           "level2" LEVEL2TEMPLATE
                           "level3" LEVEL3TEMPLATE
                           "level4" LEVEL4TEMPLATE
                           "working" WORKINGTEMPLATE
                           "doublee" DOUBLEEXTENDS))))

(define-test block-extends-level1
  :parent block-extends
  (is string=
      "|block 1 from level1|block 2 from layout|nested block 4 from layout|"
      (let ((ginjish-compiler::*trim-blocks* t))
        (block-test-helper LEVEL1TEMPLATE nil
                           "layout" LAYOUTTEMPLATE
                           "level1" LEVEL1TEMPLATE
                           "level2" LEVEL2TEMPLATE
                           "level3" LEVEL3TEMPLATE
                           "level4" LEVEL4TEMPLATE
                           "working" WORKINGTEMPLATE
                           "doublee" DOUBLEEXTENDS))))

(define-test block-extends-level2
  :parent block-extends
  (is string=
      "|block 1 from level1|nested block 5 from level2|nested block 4 from layout|"
      (let ((ginjish-compiler::*trim-blocks* t))
        (block-test-helper LEVEL2TEMPLATE nil
                           "layout" LAYOUTTEMPLATE
                           "level1" LEVEL1TEMPLATE
                           "level2" LEVEL2TEMPLATE
                           "level3" LEVEL3TEMPLATE
                           "level4" LEVEL4TEMPLATE
                           "working" WORKINGTEMPLATE
                           "doublee" DOUBLEEXTENDS))))

(define-test block-extends-level3
  :parent block-extends
  (is string=
      "|block 1 from level1|block 5 from level3|block 4 from level3|"
      (let ((ginjish-compiler::*trim-blocks* t))
        (block-test-helper LEVEL3TEMPLATE nil
                           "layout" LAYOUTTEMPLATE
                           "level1" LEVEL1TEMPLATE
                           "level2" LEVEL2TEMPLATE
                           "level3" LEVEL3TEMPLATE
                           "level4" LEVEL4TEMPLATE
                           "working" WORKINGTEMPLATE
                           "doublee" DOUBLEEXTENDS))))

(define-test block-extends-level4
  :parent block-extends
  (is string=
      "|block 1 from level1|block 5 from level3|block 3 from level4|"
      (let ((ginjish-compiler::*trim-blocks* t))
        (block-test-helper LEVEL4TEMPLATE nil
                           "layout" LAYOUTTEMPLATE
                           "level1" LEVEL1TEMPLATE
                           "level2" LEVEL2TEMPLATE
                           "level3" LEVEL3TEMPLATE
                           "level4" LEVEL4TEMPLATE
                           "working" WORKINGTEMPLATE
                           "doublee" DOUBLEEXTENDS))))

;;; import

(define-test import
  :parent compiler)

(define-test import-simple
  :parent import
  (is string=
      "hello world"
      (block-test-helper "{% import 'foo' as foo %}{{foo.bar()}}"
                         nil
                         "foo" "{% macro bar() %}hello world{% endmacro %}"))
  (is string=
      "hello world"
      (block-test-helper "{% import 'foo' as foo %}{{foo.bar()}}{{foo.quux}}"
                         nil
                         "foo" "{% macro bar() %}hello {% endmacro %}{% set quux = ' world' %}"))
  (is string=
      "hello world"
      (block-test-helper "{% import 'foo' as foo %}{{foo.bar()}}{{foo.baz()}}"
                         nil
                         "foo" "{% macro baz() %} world{% endmacro %}{% macro bar() %}hello{% endmacro %}")))

(define-test from-simple
  :parent import
  (is string=
      "hello world"
      (block-test-helper "{% from 'foo' import bar as b %}{{b()}}"
                         nil
                         "foo" "{% macro bar() %}hello world{% endmacro %}"))
  (is string=
      "hello world"
      (block-test-helper "{% from 'foo' import bar, baz as quux %}{{bar()}}{{quux()}}"
                         nil
                         "foo" "{% macro baz() %} world{% endmacro %}{% macro bar() %}hello{% endmacro %}")))

;;; syntax

(define-test syntax
  :parent compiler)

(define-test syntax-slicing
  :parent syntax
  (is string=
      "[1, 2, 3]|[3, 2, 1]"
      (if-test-helper "{{ [1, 2, 3][:] }}|{{ [1, 2, 3][::-1] }}")))

(define-test syntax-attr
  :parent syntax
  (is string=
      "42|42"
      (if-test-helper "{{ foo.bar }}|{{ foo['bar'] }}"
                      (list 'foo (serapeum:dict "bar" 42)))))

(define-test syntax-subscript
  :parent syntax
  (is string=
      "0|2"
      (if-test-helper "{{ foo[0] }}|{{ foo[-1] }}"
                      (list 'foo (list 0 1 2)))))

;; tuple

(define-test syntax-math
  :parent syntax
  (is string=
      "1.5|8"
      (if-test-helper "{{ (1 + 1 * 2) - 3 / 2 }}|{{ 2**3 }}")))

(define-test syntax-div
  (is string=
      "1|1.5|1"
      (if-test-helper "{{ 3 // 2 }}|{{ 3 / 2 }}|{{ 3 % 2 }}")))

(define-test syntax-unary
  :parent syntax
  (is string=
      "3|-3"
      (if-test-helper "{{ +3 }}|{{ -3 }}")))

(define-test syntax-concat
  :parent syntax
  (is string=
      "[1, 2]foo"
      (if-test-helper "{{ [1, 2] ~ 'foo' }}")))

(define-test syntax-inop
  :parent syntax
  (is string=
      "True|False"
      (if-test-helper "{{ 1 in [1, 2, 3] }}|{{ 1 not in [1, 2, 3] }}")))

(define-test syntax-bool
  :parent syntax
  (is string=
      "False|True|True"
      (if-test-helper "{{ true and false }}|{{ false or true }}|{{ not false }}")))

(define-test syntax-grouping
  :parent syntax
  (is string=
      "False"
      (if-test-helper "{{ (true and false) or (false and true) and not false }}")))

(define-test syntax-django-attr
  :parent syntax
  (skip "TODO do we really want to support this?"
    (is string=
        "1|1"
        (if-test-helper "{{ [1, 2, 3].0 }}|{{ [[1]].0.0 }}"))))

(define-test syntax-conditional-expression
  :parent syntax
  (is string=
      "0"
      (if-test-helper "{{ 0 if true else 1 }}"))
  (is string=
      "<>"
      (if-test-helper "<{{ 1 if false }}>")))

(define-test syntax-filter-priority
  :parent syntax
  (is string=
      "FOOBAR"
      (if-test-helper "{{ 'foo'|upper + 'bar'|upper }}")))

(define-test syntax-test-chaining
  :parent syntax
  (skip "TODO related to binding of filter/test?"
    (is string=
        "True"
        (if-test-helper "{{ 42 is string or 42 is number }}")))
  (fail (if-test-helper "{{ foo is string is sequence }}")))

(define-test syntax-string-concatenation
  :parent syntax
  (is string=
      "foobarbaz"
      (if-test-helper "{{ 'foo' \"bar\" 'baz' }}")))

(define-test syntax-notin
  :parent syntax
  (is string=
      "False"
      (if-test-helper "{{ not 42 in bar }}"
                      (list 'bar (alexandria:iota 100)))))

(define-test syntax-operator-precedence
  :parent syntax
  (is string=
      "5"
      (if-test-helper "{{ 2 * 3 + 4 % 2 + 1 - 2 }}")))

(define-test syntax-implicit-subscribed-tuple ; FIXME - changed test
  :parent syntax
  (is string=
      "eggs"
      (if-test-helper "{{ foo[1, 2] }}"
                      (list 'foo (serapeum:dict '(1 2) "eggs")))))

(define-test syntax-raw
  :parent syntax
  (is string=
      "{{ FOO }} and {% BAR %}"
      (if-test-helper "{% raw %}{{ FOO }} and {% BAR %}{% endraw %}")))

(define-test syntax-localset
  :parent syntax
  (is string=
      "0"
      (if-test-helper "{% set foo = 0 %}{% for item in [1, 2] %}{% set foo = 1 %}{% endfor %}{{ foo }}")))

(define-test syntax-parse-unary
  :parent syntax
  (is string=
      "-42"
      (if-test-helper "{{ -foo['bar'] }}"
                      (list 'foo (serapeum:dict "bar" 42))))
  (skip "TODO Jinja syntax reverses power and unary"
    (is string=
      "42"
      (if-test-helper "{{ -foo[\"bar\"]|abs }}"
                      (list 'foo (serapeum:dict "bar" 42))))))

;;; lstrip-blocks

(define-test lstrip-blocks
  :parent all)

(cl-interpol:enable-interpol-syntax)

(define-test lstrip-lstrip
  :parent lstrip-blocks
  (is string=
      #?"\n"
      (let ((ginjish-compiler::*lstrip-blocks* t)
            (ginjish-compiler::*trim-blocks* nil))
        (if-test-helper #?"    {% if True %}\n    {% endif %}"))))

(define-test lstrip-trim
  :parent lstrip-blocks
  (is string=
      ""
      (let ((ginjish-compiler::*lstrip-blocks* t)
            (ginjish-compiler::*trim-blocks* t))
        (if-test-helper #?"    {% if True %}\n    {% endif %}"))))

(define-test lstrip-no-lstrip
  :parent lstrip-blocks
  (is string=
      #?"    \n    "
      (let ((ginjish-compiler::*lstrip-blocks* nil)
            (ginjish-compiler::*trim-blocks* nil))
        (if-test-helper #?"    {% if True %}\n    {% endif %}"))))

(define-test lstrip-blocks-false-with-no-lstrip
  :parent lstrip-blocks
  (is string=
      #?"    \n    "
      (let ((ginjish-compiler::*lstrip-blocks* nil)
            (ginjish-compiler::*trim-blocks* nil))
        (if-test-helper #?"    {% if True %}\n    {% endif %}")))
  (is string=
      #?"    \n    "
      (let ((ginjish-compiler::*lstrip-blocks* nil)
            (ginjish-compiler::*trim-blocks* nil))
        (if-test-helper #?"    {%+ if True %}\n    {% endif %}"))))

(define-test lstrip-endline
  :parent lstrip-blocks
  (is string=
      #?"    hello\n    goodbye"
      (let ((ginjish-compiler::*lstrip-blocks* t)
            (ginjish-compiler::*trim-blocks* nil))
        (if-test-helper #?"    hello{% if True %}\n    goodbye{% endif %}"))))

(define-test lstrip-inline
  :parent lstrip-blocks
  (is string=
      "hello    "
      (let ((ginjish-compiler::*lstrip-blocks* t)
            (ginjish-compiler::*trim-blocks* nil))
        (if-test-helper #?"    {% if True %}hello    {% endif %}"))))

(define-test lstrip-nested
  :parent lstrip-blocks
  (is string=
      "a b c "
      (let ((ginjish-compiler::*lstrip-blocks* t)
            (ginjish-compiler::*trim-blocks* nil))
        (if-test-helper "    {% if True %}a {% if True %}b {% endif %}c {% endif %}"))))

(define-test lstrip-left-chars
  :parent lstrip-blocks
  (is string=
      #?"    abc \n        hello"
      (let ((ginjish-compiler::*lstrip-blocks* t)
            (ginjish-compiler::*trim-blocks* nil))
        (if-test-helper "    abc {% if True %}
        hello{% endif %}"))))

(define-test lstrip-embedded-strings
  :parent lstrip-blocks
  (is string=
      " {% str %} "
      (let ((ginjish-compiler::*lstrip-blocks* t)
            (ginjish-compiler::*trim-blocks* nil))
        (if-test-helper "    {% set x = \" {% str %} \" %}{{ x }}"))))

(define-test lstrip-preserve-leading-newlines
  :parent lstrip-blocks
  (is string=
      #?"\n\n\n"
      (let ((ginjish-compiler::*lstrip-blocks* t)
            (ginjish-compiler::*trim-blocks* nil))
        (if-test-helper #?"\n\n\n{% set hello = 1 %}"))))

(define-test lstrip-comment
  :parent lstrip-blocks
  (is string=
      #?"\nhello\n"
      (let ((ginjish-compiler::*lstrip-blocks* t)
            (ginjish-compiler::*trim-blocks* nil))
        (if-test-helper "    {# if True #}
hello
    {#endif#}"))))

(define-test lstrip-angle-bracket-simple
  :parent lstrip-blocks
  (is string=
      "hello    "
      (ginjish-grammar::with-delimiters
          ("<%" "%>" "${" "}" "<%#" "%>")
        (let ((ginjish-compiler::*lstrip-blocks* t)
              (ginjish-compiler::*trim-blocks* t))
          (if-test-helper "    <% if True %>hello    <% endif %>")))))

(define-test lstrip-angle-bracket-comment
  :parent lstrip-blocks
  (is string=
      "hello    "
      (ginjish-grammar::with-delimiters
          ("<%" "%>" "${" "}" "<%#" "%>")
        (let ((ginjish-compiler::*lstrip-blocks* t)
              (ginjish-compiler::*trim-blocks* t))
          (if-test-helper "    <%# if True %>hello    <%# endif %>")))))

(define-test lstrip-php-syntax-with-manual
  :parent lstrip-blocks
  (is string=
      "01234"
      (ginjish-grammar::with-delimiters
          ("<?" "?>" "<?=" "?>" "<!--" "-->")
        (let ((ginjish-compiler::*lstrip-blocks* t)
              (ginjish-compiler::*trim-blocks* t))
          (if-test-helper "<!-- I'm a comment, I'm not interesting -->
    <? for item in seq -?>
        <?= item ?>
    <?- endfor ?>"
                          (list 'seq (alexandria:iota 5)))))))

(define-test lstrip-erb-syntax-with-manual
  :parent lstrip-blocks
  (is string=
      "01234"
      (ginjish-grammar::with-delimiters
          ("<%" "%>" "<%=" "%>" "<%#" "%>")
        (let ((ginjish-compiler::*lstrip-blocks* t)
              (ginjish-compiler::*trim-blocks* t))
          (if-test-helper "<%# I'm a comment, I'm not interesting -%>
    <% for item in seq -%>
        <%= item %>
    <%- endfor %>"
                          (list 'seq (alexandria:iota 5)))))))

(define-test lstrip-blocks-outside-with-new-line
  :parent lstrip-blocks
  (is string=
      #?"(\na=1 b=2 \n  )"
      (let ((ginjish-compiler::*lstrip-blocks* t)
            (ginjish-compiler::*trim-blocks* nil))
        (if-test-helper #?"  {% if kvs %}(\n   {% for k, v in kvs %}{{ k }}={{ v }} {% endfor %}\n  ){% endif %}"
                        (list 'kvs '(("a"  1) ("b"  2)))))))

(define-test lstrip-trim-blocks-outside-with-new-line
  :parent lstrip-blocks
  (is string=
      #?"(\na=1 b=2   )"
      (let ((ginjish-compiler::*lstrip-blocks* t)
            (ginjish-compiler::*trim-blocks* t))
        (if-test-helper #?"  {% if kvs %}(\n   {% for k, v in kvs %}{{ k }}={{ v }} {% endfor %}\n  ){% endif %}"
                        (list 'kvs '(("a"  1) ("b"  2)))))))

(define-test lstrip-blocks-inside-with-new-line
  :parent lstrip-blocks
  (is string=
      #?"  (\na=1 b=2 \n)"
      (let ((ginjish-compiler::*lstrip-blocks* t)
            (ginjish-compiler::*trim-blocks* nil))
        (if-test-helper #?"  ({% if kvs %}\n   {% for k, v in kvs %}{{ k }}={{ v }} {% endfor %}\n  {% endif %})"
                        (list 'kvs '(("a"  1) ("b"  2)))))))

(define-test lstrip-trim-blocks-inside-with-new-line
  :parent lstrip-blocks
  (is string=
      #?"  (a=1 b=2 )"
      (let ((ginjish-compiler::*lstrip-blocks* t)
            (ginjish-compiler::*trim-blocks* t))
        (if-test-helper #?"  ({% if kvs %}\n   {% for k, v in kvs %}{{ k }}={{ v }} {% endfor %}\n  {% endif %})"
                        (list 'kvs '(("a"  1) ("b"  2)))))))

(define-test lstrip-blocks-without-new-line
  :parent lstrip-blocks
  (is string=
      "   a=1 b=2   "
      (let ((ginjish-compiler::*lstrip-blocks* t)
            (ginjish-compiler::*trim-blocks* nil))
        (if-test-helper "  {% if kvs %}   {% for k, v in kvs %}{{ k }}={{ v }} {% endfor %}  {% endif %}"
                        (list 'kvs '(("a"  1) ("b"  2)))))))

;;; filters

(define-test filters
  :parent all)

(define-test filters-capitalize
  :parent filters
  (is string=
      "Foo bar"
      (if-test-helper "{{ 'foo bar'|capitalize }}")))

(define-test filters-center
  :parent filters
  (is string=
      "   foo   "
      (if-test-helper "{{ 'foo'|center(9) }}")))

(define-test filters-default
  :parent filters
  (is string=
      "no|False|no|yes"
      (if-test-helper "{{ missing|default('no') }}|{{ false|default('no') }}|{{ false|default('no',true) }}|{{ given|default('no') }}"
                      (list 'given "yes"))))

(define-test filters-dictsort
  :parent filters
  (is string=
      "[('aa', 0), ('AB', 3), ('b', 1), ('c', 2)]"
      (if-test-helper "{{ foo|dictsort }}"
                      (list 'foo (serapeum:dict "aa" 0 "b" 1 "c" 2 "AB" 3))))
  (is string=
      "[('AB', 3), ('aa', 0), ('b', 1), ('c', 2)]"
      (if-test-helper "{{ foo|dictsort(case_sensitive=True) }}"
                      (list 'foo (serapeum:dict "aa" 0 "b" 1 "c" 2 "AB" 3))))
  (is string=
      "[('aa', 0), ('b', 1), ('c', 2), ('AB', 3)]"
      (if-test-helper "{{ foo|dictsort(by='value') }}"
                      (list 'foo (serapeum:dict "aa" 0 "b" 1 "c" 2 "AB" 3))))
  (is string=
      "[('c', 2), ('b', 1), ('AB', 3), ('aa', 0)]"
      (if-test-helper "{{ foo|dictsort(reverse=True) }}"
                      (list 'foo (serapeum:dict "aa" 0 "b" 1 "c" 2 "AB" 3)))))

(define-test filters-batch
  :parent filters
  (is string=
      "[[0, 1, 2], [3, 4, 5], [6, 7, 8], [9]]|[[0, 1, 2], [3, 4, 5], [6, 7, 8], [9, 'X', 'X']]"
      (if-test-helper "{{ foo|batch(3)|list }}|{{ foo|batch(3, 'X')|list }}"
                      (list 'foo (alexandria:iota 10)))))

(define-test filters-slice
  :parent filters
  (is string=
      "[[0, 1, 2, 3], [4, 5, 6], [7, 8, 9]]|[[0, 1, 2, 3], [4, 5, 6, 'X'], [7, 8, 9, 'X']]"
      (if-test-helper "{{ foo|slice(3)|list }}|{{ foo|slice(3, 'X')|list }}"
                      (list 'foo (alexandria:iota 10)))))

(define-test filters-escape
  :parent filters
  (is string=
      "&lt;&#34;&gt;&amp;"
      (if-test-helper "{{ '<\">&'|escape }}")))

(define-test filters-trim
  :parent filters
  (is string=
      "..stays.."
      (if-test-helper "{{ foo|trim }}"
                      (list 'foo "  ..stays..")))
  (is string=
      "  ..stays"
      (if-test-helper "{{ foo|trim('.') }}"
                      (list 'foo "  ..stays..")))
  (is string=
      "stays"
      (if-test-helper "{{ foo|trim(' .') }}"
                      (list 'foo "  ..stays.."))))

(define-test filters-striptags
  :parent filters
  (is string=
      "just a small example link to a webpage"
      (if-test-helper "{{ foo|striptags }}"
                      (list 'foo "  <p>just a small   
 <a href=\"#\">example</a> link</p>
<p>to a webpage</p> <!-- <p>and some commented stuff</p> -->"))))

(define-test filters-filesizeformat
  :parent filters
  (is string=
      "100 Bytes|1.0 kB|1.0 MB|1.0 GB|1.0 TB|100 Bytes|1000 Bytes|976.6 KiB|953.7 MiB|931.3 GiB"
      (if-test-helper (concatenate 'string
                                    "{{ 100|filesizeformat }}|"
                                    "{{ 1000|filesizeformat }}|"
                                    "{{ 1000000|filesizeformat }}|"
                                    "{{ 1000000000|filesizeformat }}|"
                                    "{{ 1000000000000|filesizeformat }}|"
                                    "{{ 100|filesizeformat(true) }}|"
                                    "{{ 1000|filesizeformat(true) }}|"
                                    "{{ 1000000|filesizeformat(true) }}|"
                                    "{{ 1000000000|filesizeformat(true) }}|"
                                    "{{ 1000000000000|filesizeformat(true) }}")))
  (is string=
      "300 Bytes|3.0 kB|3.0 MB|3.0 GB|3.0 TB|300 Bytes|2.9 KiB|2.9 MiB"
      (if-test-helper (concatenate 'string
                                   "{{ 300|filesizeformat }}|"
                                   "{{ 3000|filesizeformat }}|"
                                   "{{ 3000000|filesizeformat }}|"
                                   "{{ 3000000000|filesizeformat }}|"
                                   "{{ 3000000000000|filesizeformat }}|"
                                   "{{ 300|filesizeformat(true) }}|"
                                   "{{ 3000|filesizeformat(true) }}|"
                                   "{{ 3000000|filesizeformat(true) }}"))))

(define-test filters-first
  :parent filters
  (is string=
      "0"
      (if-test-helper "{{ foo|first }}"
                      (list 'foo (alexandria:iota 10)))))

(define-test filters-float
  :parent filters
  (is string=
      "42.0"
      (if-test-helper "{{ value|float }}" (list 'value "42")))
  (is string=
      "0.0"
      (if-test-helper "{{ value|float }}" (list 'value "abc")))
  (is string=
      "32.32"
      (if-test-helper "{{ value|float }}" (list 'value "32.32"))))

(define-test filters-float-default
  :parent filters
  (is string=
      "1.0"
      (if-test-helper "{{ value|float(1.0) }}" (list 'value "abc"))))

(define-test filters-format ; FIXME this is CL:FORMAT
  :parent filters
  (is string=
      "a|b"
      (if-test-helper "{{ '~A|~A'|format('a', 'b') }}")))

(define-test filters-indent-multiline-template
  :parent filters
  (is string=
      #?"\n  foo bar\n  \"baz\"\n"
      (if-test-helper "{{ foo|indent(width=2) }}"
                      (list 'foo #?"\nfoo bar\n\"baz\"\n")))
  (is string=
      #?"  \n  foo bar\n  \"baz\"\n"
      (if-test-helper "{{ foo|indent(width=2, first=true) }}"
                      (list 'foo #?"\nfoo bar\n\"baz\"\n")))
  (is string=
      #?"\n  foo bar\n  \"baz\"\n  "
      (if-test-helper "{{ foo|indent(width=2, blank=true) }}"
                      (list 'foo #?"\nfoo bar\n\"baz\"\n")))
  (is string=
      #?"  \n  foo bar\n  \"baz\"\n  "
      (if-test-helper "{{ foo|indent(width=2, blank=true, first=true) }}"
                      (list 'foo #?"\nfoo bar\n\"baz\"\n"))))

(define-test filters-indent
  :parent filters
  (is string=
      "jinja"
      (if-test-helper "{{ 'jinja'|indent }}"))
  (is string=
      "    jinja"
      (if-test-helper "{{ 'jinja'|indent(first=true) }}"))
  (is string=
      "jinja"
      (if-test-helper "{{ 'jinja'|indent(blank=true) }}")))

(cl-interpol:disable-interpol-syntax)
       


;;; builtins

(define-test builtins
  :parent all)

