;;;; ginjish-grammar.lisp

(cl:defpackage #:ginjish-grammar
  (:use #:cl #:esrap))

(cl:in-package #:ginjish-grammar)

;;; whitespace

(defrule ws (+ (or #\Space #\Tab #\Page))
  (:constant nil))

(defrule ws* (* (or #\Space #\Tab #\Page))
  (:constant nil))

;;; strings

(defun not-doublequote (char)
  (not (eql #\" char)))

(defun not-singlequote (char)
  (not (eql #\' char)))

(defrule doublequote-string-char (or (not-doublequote character) (and #\\ #\")))

(defrule singlequote-string-char (or (not-singlequote character) (and #\\ #\')))

(defrule stringliteral (and (or doublequote-string singlequote-string)
			    (* (and ws (or doublequote-string singlequote-string))))
  (:lambda (s)
    (let ((strings (list* (first s) (mapcar #'second (second s)))))
      `(:string ,(apply #'concatenate 'string strings)))))

(defrule doublequote-string (and #\" (* doublequote-string-char) #\")
  (:destructure (q1 string q2)
		(declare (ignore q1 q2))
		(text string)))

(defrule singlequote-string (and #\' (* singlequote-string-char) #\')
  (:destructure (q1 string q2)
		(declare (ignore q1 q2))
		(text string)))


;;; numbers

(defun strip-underlines (list)
  (remove-if (lambda (char) (string= #\_ char)) list))

(defrule digit (character-ranges (#\0 #\9)))

(defrule nonzerodigit (character-ranges (#\1 #\9)))

(defrule bindigit (or #\0 #\1))

(defrule octdigit (character-ranges (#\0 #\7)))

(defrule hexdigit (character-ranges (#\0 #\9) (#\a #\f) (#\A #\F)))

(defrule integer (or decinteger bininteger octinteger hexinteger))

(defrule decinteger (or nonzerodecinteger zerodecinteger))

(defrule nonzerodecinteger (and nonzerodigit (* (or #\_ digit)))
  (:destructure (first rest)
    (push first rest)
    (parse-integer (text (strip-underlines rest)) :radix 10)))

(defrule zerodecinteger (and (+ #\0) (! (or #\b #\o #\x)) (* (or #\_ #\0)))
  (:constant 0))

(defrule bininteger (and #\0 (or #\b #\B) (+ (or #\_ bindigit)))
  (:destructure (zero b list)
    (declare (ignore zero b))
    (parse-integer (text (strip-underlines list)) :radix 2)))

(defrule octinteger (and #\0 (or #\o #\O) (+ (or #\_ octdigit)))
  (:destructure (zero o list)
    (declare (ignore zero o))
    (parse-integer (text (strip-underlines list)) :radix 8)))

(defrule hexinteger (and #\0 (or #\x #\X) (+ (or #\_ hexdigit)))
  (:destructure (zero x list)
    (declare (ignore zero x))
    (parse-integer (text (strip-underlines list)) :radix 16)))

(defrule digitpart (and digit (* (or #\_ digit)))
  (:destructure (first rest)
    (push first rest)
    (let* ((digits (strip-underlines rest))
	   (length (length digits)))
      (cons
       (parse-integer (text (strip-underlines rest)) :radix 10)
       length))))

(defrule floatnumber (or exponentfloat pointfloat))

(defrule fraction (and #\. digitpart)
  (:destructure (dot suff)
    (declare (ignore dot))
    (* (car suff) (expt 10 (- (cdr suff))))))

(defrule pointfloat (or pointfloat-fraction pointfloat-nofraction))

(defrule pointfloat-fraction (and (? digitpart) fraction)
  (:destructure (whole fraction)
    (float (+ (or (car whole) 0) fraction))))

(defrule pointfloat-nofraction (and digitpart #\.)
  (:destructure (whole dot)
    (declare (ignore dot))
    (float (car whole))))

(defrule exponent (and (or #\e #\E) (? (or #\+ #\-)) digitpart)
  (:destructure (e sign digitpart)
    (declare (ignore e))
    (let ((sign (if (string= #\- sign) -1 +1)))
      (expt 10 (* sign (car digitpart))))))

(defrule exponentfloat (and (or pointfloat digitpart) exponent)
  (:destructure (first exp)
    (etypecase first
      (cons (float (* (car first) exp) 1.0d0))
      (number (float (* first exp) 1.0d0)))))

#|
(mapcar (lambda (num) (parse 'floatnumber num)) '("3.14" "10." ".001" #+(or)"1e100" "3.14e-10" "0e0" "3.14_15_93"))

1e100 -> floating point overflow
|#

;;; identifiers

(defun alpha_ (char)
  (or (alpha-char-p char) (char= #\_ char)))

(defun alphanumeric_ (char)
  (or (alphanumericp char) (char= #\_ char)))

(defrule id-start (alpha_ character))

(defrule id-cont (alphanumeric_ character))

(defrule name (and id-start (* id-cont))
  (:text t))

(defrule identifier (and id-start (* id-cont)) ; FIXME load and store identifiers?
  (:lambda (i)
    (let ((s (text i)))
      (serapeum:string-case s
	(("true" "True") t)
	(("false" "False") nil)
	(("none" "None") nil) ; FIXME is this right?
	(t `(:identifier ,s))))))

;;; literals

(defrule literal (or stringliteral floatnumber integer))

;;; expression

(defrule expression conditional-expression)

(defrule conditional-expression (and or-test
				     (? (and ws "if" ws or-test ws "else" ws expression)))
  (:lambda (c)
	   (if (second c)
	       `(:if-expr ,(fourth (second c)) ,(first c) ,(eighth (second c)))
	       (first c))))

(defrule or-test (or or-test-sub and-test))

(defrule or-test-sub (and or-test ws "or" ws and-test)
  (:lambda (o)
    `(:or ,(first o) ,(fifth o))))

(defrule and-test (or and-test-sub not-test))

(defrule and-test-sub (and and-test ws "and" ws not-test)
  (:lambda (a)
    `(:and ,(first a) ,(fifth a))))

(defrule not-test (or not-test-sub comparison))

(defrule not-test-sub (and "not" ws not-test)
  (:lambda (n)
    `(:not ,(third n))))

(defun chain-comparisons (c)
  `(:and
    ,@(loop for left = (first c) then right
	 for (nil op nil right) in (second c)
	 collecting (list op left right))))

(defrule comparison (and #+(or)or-expr a-expr
			 (* (and ws* comp-operator ws* #+(or)or-expr a-expr)))
  (:lambda (c)
    (case (length (second c))
      (0 (first c))
      (1 (list (second (first (second c))) (first c) (fourth (first (second c)))))
      (t (chain-comparisons c)))))

#+(or)(defrule comp-operator (or ">=" "<=" "!=" "<" ">" "=="
			   (and "is" (? (and ws "not")))
			   (and (? (and "not" ws)) "in"))
  (:lambda (c)
    (if (consp c)
	(if (equal "is" (first c))
	    (if (equal "not" (second (second c)))
		:is-not
		:is)
	    (if (equal "not" (first (first c)))
		:not-in
		:in))
	(alexandria:make-keyword c))))

(defun c-op->lisp (op)
  (serapeum:string-case op
    (">=" :gte)
    ("<=" :lte)
    ("!=" :not-equal)
    ("<"  :lt)
    (">"  :gt)
    ("==" :equal)
    ("in" :in)
    ("not-in" :not-in)
    (t (error "c-op->lisp got: ~S" op))))

(defrule comp-operator (or ">=" "<=" "!=" "<" ">" "=="
			   not-in)
  (:function c-op->lisp))

(defrule not-in (and (? (and "not" ws)) "in")
  (:lambda (n)
    (if (first n) "not-in" "in")))

#|
(defrule or-expr (or or-expr-sub xor-expr))

(defrule or-expr-sub (and or-expr ws* "|" ws* xor-expr) ; Jinja "|" is filter
  (:lambda (o)
    (list :\| (first o) (fifth o))))

(defrule xor-expr (or xor-expr-sub and-expr))

(defrule xor-expr-sub (and xor-expr ws* "^" ws* and-expr)
  (:lambda (x)
    (list (alexandria:make-keyword (third x)) (first x) (fifth x))))

(defrule and-expr (or and-expr-sub shift-expr))

(defrule and-expr-sub (and and-expr ws* "&" ws* shift-expr)
  (:lambda (a)
    (list (alexandria:make-keyword (third a)) (first a) (fifth a))))

(defrule shift-expr (or shift-expr-sub a-expr))

(defrule shift-expr-sub (and shift-expr ws* (or "<<" ">>") ws* a-expr)
  (:lambda (s)
    (list (alexandria:make-keyword (third s)) (first s) (fifth s))))
|#

(defrule a-expr (or a-expr-sub #+(or)m-expr concat-expr))

(defrule a-expr-sub (or (and a-expr ws* "+" ws* #+(or)m-expr concat-expr)
			(and a-expr ws* "-" ws* #+(or)m-expr concat-expr))
  (:lambda (a)
    (list (serapeum:string-case (third a) (("+") :plus) (("-") :minus)) (first a) (fifth a))))

(defrule concat-expr (or concat-expr-sub m-expr))

(defrule concat-expr-sub (and concat-expr (+ (and (and ws* "~" ws*) m-expr)))
  (:lambda (c)
    `(:concatenate ,(first c) ,@(mapcar #'second (second c)))))

(defrule m-expr (or m-expr-sub u-expr))

(defun m-op->lisp (op)
  (serapeum:string-case op
    ("*"  :mul)
    ("//" :floor)
    ("/"  :div)
    ("%"  :mod) ; FIXME also printf-style string formatting
    (t (error "m-op->lisp got: ~S" op))))

(defrule m-expr-sub (or (and m-expr ws* "*" ws* u-expr)
			#+(or)(and m-expr ws* "@" ws* u-expr)
			(and m-expr ws* "//" ws* u-expr)
			(and m-expr ws* "/" ws* u-expr)
			(and m-expr ws* "%" ws* u-expr))
  (:lambda (m)
    (list (m-op->lisp (third m)) (first m) (fifth m))))

;; Jinja seems to have u-expr and power reversed

(defrule u-expr (or u-expr-sub power))

(defrule u-expr-sub (or (and "-" ws* u-expr)
			(and "+" ws* u-expr)
			#+(or)(and "~" ws* u-expr)) ;; Jinja "~" is binary string concat
  (:lambda (u)
    (list (serapeum:string-case (third u) (("+") :uplus) (("-") :uminus)) (third u))))

(defrule power (and filtered-primary (? (and ws* "**" ws* u-expr)))
  (:lambda (p)
    (if (null (second p))
	(first p)
	(list :pow (first p) (fourth (second p))))))

(defrule filtered-primary (and primary (? filter-expr))
  (:lambda (f)
	   (if (second f)
	       `(,(first (second f)) ,(first f) ,@(rest (second f)))
	       (first f))))

(defrule filter-expr (or test-expr filters))

(defrule filters (* (and (and ws* "|" ws*) primary))
  (:lambda (f)
	   (when f
	     `(:filter ,@(mapcar #'second f)))))

(defrule test-expr (and (and ws* "is") (? (and ws "not")) ws primary)
  (:lambda (e)
    `(,(if (second (second e)) :test-not :test) ,(fourth e))))

;;; atom

(defrule atom (or identifier literal enclosure))

(defrule enclosure (or parenth-form list-display set-display dict-display))

(defrule parenth-form (and "("  ws* (? expression-list) ws* ")")
  (:function third))

(defrule expression-list (and expression (* (and ws* "," ws* expression)) (? (and ws* ",")))
  (:lambda (e)
    (case (length (second e))
      (0 (if (third e)
	     `(:tuple ,(first e))
	     (first e)))
      (t (let ((elements (mapcar #'fourth (second e))))
	   `(:tuple ,(first e) ,@elements)))))) 

(defrule expression-list* (and expression (* (and ws* "," ws* expression)) (? (and ws* ",")))
  (:lambda (e)
    `(,(first e) ,@(mapcar #'fourth (second e)))))

(defrule list-display (and "[" ws* (? expression-list*) ws* "]")
  (:lambda (l)
    `(:list ,@(third l))))

(defrule set-display (and "{" ws* expression-list* ws* "}")
  (:lambda (s)
    `(:set ,@(third s))))

(defrule dict-display (and "{" ws* (? key-datum-list) ws* "}")
  (:lambda (d)
    `(:dict ,@(third d))))

(defrule key-datum-list (and key-datum (* (and ws* "," ws* key-datum)) (? (and ws* ",")))
  (:lambda (k)
    `(,(first k) ,@(mapcar #'fourth (second k)))))

(defrule key-datum (and expression ws* ":" ws* expression)
  (:lambda (k)
    (list (first k) (fifth k))))


;;; primary

(defrule primary (or attributeref subscription slicing call atom))

(defrule attributeref (and primary "." identifier)
  (:lambda (a)
    (list :get-attr (first a) (third a))))

(defrule subscription (and primary "[" ws* expression-list* ws* "]")
  (:lambda (s)
    (list :get-item (first s) (fourth s))))

(defrule call (and primary "(" ws* (? mixed-argument-list) ws* ")") ; FIXME lispy lambda list
  (:lambda (c)
    (list :call (first c) (fourth c))))

(defrule mixed-argument-list (and mixed-argument (* (and (and ws* "," ws*) mixed-argument)) (? (and ws* ",")))
  (:lambda (m)
    `(,(first m) ,@(mapcar #'second (second m)))))

(defrule mixed-argument (or keyword-item positional-item))

#|
(defrule argument-list (or keywords-arguments ; FIXME this doesn't seem to work
			   (and positional-arguments (? (and ws* "," keywords-arguments))))
  )

(defrule argument-list (and (? positional-arguments) (? (and ws* "," keywords-arguments))))

(defrule positional-arguments (and positional-item (* (and ws* "," ws* positional-item)))
  (:lambda (p)
    `(,(first p) ,@(mapcar #'fourth (second p)))))
    |#
    
(defrule positional-item expression)

#|
(defrule keywords-arguments (and keyword-item (* (and ws* "," ws* keyword-item)))
  (:lambda (k)
    `(,(first k) ,@(mapcar #'fourth (second k)))))

|#

(defun read-keyword (string)
  (serapeum:with-standard-input-syntax
    (let ((*package* (find-package :keyword))
	  (*read-eval* nil))
      (read-from-string string))))

(defrule keyword-item (and name  ws* "=" ws* expression)
  (:lambda (k)
    (list (read-keyword (first k)) (fifth k))))

(defrule slicing (and primary "[" ws* slice-list ws* "]")
  (:lambda (s)
    (list :slicing (first s) (fourth s))))

(defrule slice-list (and slice-item (* (and ws* "," ws* slice-item)) (? (and ws* ",")))
  (:lambda (e)
    (case (length (second e))
      (0 (if (third e)
	     `(:tuple ,(first e))
	     (first e)))
      (t (let ((elements (mapcar #'fourth (second e))))
	   `(:tuple ,(first e) ,@elements))))))

(defrule slice-item (or proper-slice expression))

(defrule proper-slice (and (? expression) ws* ":" (? (and ws* expression)) (? (and ws* ":" (? (and ws* expression)))))
  (:lambda (p)
    (list :proper-slice (first p) (second (fourth p)) (second (third (fifth p))))))

;;; template

(defrule t-statement-start "{%")

(defrule t-statement-end "%}")

(defrule t-expression-start "{{")

(defrule t-expression-end "}}")

(defrule t-comment-start "{#")

(defrule t-comment-end "#}")

(defrule matter (+ (not (or t-statement-start t-expression-start t-comment-start)))
  (:lambda (m)
    (list :matter (text m))))

(defrule suite (* (or t-statement t-expression t-comment matter))
  (:lambda (s)
    `(:suite ,@s)))

;;; t-comment

(defrule t-comment (and t-comment-start comment-content t-comment-end)
  (:constant :comment))

(defrule comment-content (* (not t-comment-end)))

;;; t-statement

(defrule t-statement (or t-raw t-if t-for t-set-block t-set t-autoescape t-with))

(defrule t-raw (and t-raw-start raw-content t-raw-end)
  (:lambda (r)
    `(:raw ,(second r))))

(defrule raw-content (* (not t-raw-end))
  (:text t))

(defrule t-raw-start (and t-statement-start ws* "raw" ws* t-statement-end)
  (:constant nil))

(defrule t-raw-end (and t-statement-start ws* "endraw" ws* t-statement-end)
  (:constant nil))

;; for
#|
for_stmt ::= "for" target_list "in" expression_list ":" suite
             ["else" ":" suite]

target_list ::= target ("," target)* [","]
target      ::= identifier
                | "(" [target_list] ")"
                | "[" [target_list] "]"
                | attributeref
                | subscription
                | slicing
                | "*" target
|#
(defrule t-for (and t-for-start suite (? (and t-for-else suite)) t-for-end)
  (:destructure ((target-list expression-list) suite &optional else-part &rest end)
    (declare (ignore end))
    `(:for ,target-list ,expression-list ,suite ,(second else-part))))

(defrule t-for-start (and (and t-statement-start ws* "for" ws) target-list (and ws "in" ws) expression-list (and ws* t-statement-end))
  (:destructure (for-keyword target-list in-keyword expression-list &rest end)
    (declare (ignore for-keyword in-keyword end))
    (list target-list expression-list)))

(defrule t-for-else (and t-statement-start ws* "else" ws* t-statement-end)
  (:constant nil))

(defrule t-for-end (and t-statement-start ws* "endfor" ws* t-statement-end)
  (:constant nil))

(defrule target-list (and target (* (and ws* "," ws* target)) (? (and ws* ",")))
  (:lambda (l)
    `(,(first l) ,@(mapcar #'fourth (second l)))))

(defrule target (or tuple-target
		    list-target
		    #+(or)attributeref
		    #+(or)subscription
		    #+(or)slicing
		    #+(or)identifier name))

(defrule tuple-target (and "(" ws* (? target-list) ws* ")")
  (:lambda (e)
    (third e)))

(defrule list-target (and "[" ws* (? target-list) ws* "]")
  (:lambda (l)
    (third l)))

;; if
#|
if_stmt ::=  "if" assignment_expression ":" suite
             ("elif" assignment_expression ":" suite)*
             ["else" ":" suite]
|#

(defrule t-if (and t-if-start suite (* (and t-if-elif suite)) (? (and t-if-else suite)) t-if-end)
  (:destructure (test then &optional elifs else &rest end)
    (declare (ignore end))
    `(:if ,test ,then ,(second else) ,(mapcar (lambda (elif) (list (first elif) (second elif))) elifs))))

(defrule t-if-start (and (and t-statement-start ws* "if" ws) expression (and ws* t-statement-end))
  (:function second))

(defrule t-if-elif (and (and t-statement-start ws* "elif" ws) expression (and ws* t-statement-end))
  (:function second))

(defrule t-if-else (and t-statement-start ws* "else" ws* t-statement-end)
  (:constant nil))

(defrule t-if-end (and t-statement-start ws* "endif" ws* t-statement-end)
  (:constant nil))

;; block/endblock identifier
;; extends (or string identifier)
;; macro / call
;; filter identifier
;; include
;; import
;; from

(defrule t-with (and t-with-start suite t-with-end)
  (:lambda (w)
    `(:with ,(first w) ,(second w))))

(defrule t-with-start (and (and t-statement-start ws* "with" ws) assign-list ws* t-statement-end)
  (:function second))

(defrule t-with-end (and t-statement-start ws* "endwith" ws* t-statement-end)
  (:constant nil))

(defrule assign-list (and assign-element (* (and (and ws* "," ws*) assign-element)))
  (:lambda (a)
    `(,(first a) ,@(mapcar #'second (second a)))))

(defrule assign-element (and target-list (and ws* "=" ws*) expression)
  (:lambda (a)
    (list (first a) (third a))))
			  
;; autoescape

(defrule t-autoescape (and t-autoescape-start suite t-autoescape-end)
  (:destructure (start suite end)
    (declare (ignore end))
    `(:autoescape ,start ,suite)))
		 
(defrule t-autoescape-start (and t-statement-start ws* "autoescape" ws identifier ws* t-statement-end)
  (:function fifth))

(defrule t-autoescape-end (and t-statement-start ws* "endautoescape" ws* t-statement-end)
  (:constant nil))

;; set / set target_list = expression_list

(defrule t-set (and t-statement-start ws* "set" ws target-list ws* "=" ws* expression ws* t-statement-end)
  (:lambda (s)
    `(:assign ,(fifth s) ,(ninth s))))

;; set block / set target suite endset

(defrule t-set-block (and t-set-block-start suite t-set-block-end)
  (:lambda (s)
    `(:block-set ,(first s) ,(second s))))

(defrule t-set-block-start (and t-statement-start ws* "set" ws* target ws* t-statement-end)
  (:function fifth))

(defrule t-set-block-end (and t-statement-start ws* "endset" ws* t-statement-end)
  (:constant nil))

;;; t-expression

(defrule t-expression (and t-expression-start ws* expression ws* t-expression-end)
  (:lambda (e)
    `(:expression ,(third e))))

