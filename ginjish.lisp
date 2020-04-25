;;;; ginjish.lisp

;(cl:require #:esrap)

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

(defrule stringliteral (or doublequote-string singlequote-string))

(defrule doublequote-string (and #\" (* doublequote-string-char) #\")
  (:destructure (q1 string q2)
		(declare (ignore q1 q2))
		(list :string (text string))))

(defrule singlequote-string (and #\' (* singlequote-string-char) #\')
  (:destructure (q1 string q2)
		(declare (ignore q1 q2))
		(list :string (text string))))


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

#|
(mapcar (lambda (num) (parse 'integer num)) '("7" "2147483647" "0o177" "0b100110111" "3" "79228162514264337593543950336" "0o377" "0xdeadbeef" "100_000_000_000" "0b_1110_0101"))
|#

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
      (cons (float (* (car first) exp)))
      (number (float (* first exp))))))

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

(defrule identifier (and id-start (* id-cont))
  (:text t))

;;; literals

(defrule literal (or stringliteral floatnumber integer))

;;; expression

(defrule expression conditional-expression)

(defrule conditional-expression (and or-test
				     (? (and ws "if" ws or-test ws "else" ws expression)))
  (:lambda (c)
	   (if (second c)
	       (list :if-else (fourth (second c)) (first c) (eighth (second c)))
	       (first c))))

(defrule or-test (or (and or-test ws "or" ws and-test)
		     and-test)
  (:lambda (o)
	   (if (and (consp o) (equal "or" (third o)))
	       (list :or (first o) (fifth o))
	       o)))

(defrule and-test (or (and and-test ws "and" ws not-test)
		      not-test)
  (:lambda (a)
	   (if (and (consp a) (equal "and" (third a)))
	       (list :and (first a) (fifth a))
	       a)))

(defrule not-test (or (and "not" ws not-test)
		      comparison)
  (:lambda (n)
    (if (and (consp n) (equal "not" (first n)))
	(list :not (third n))
	n)))

(defun chain-comparisons (c)
  `(:and
    ,@(loop for left = (first c) then right
	 for (nil op nil right) in (second c)
	 collecting (list op left right))))

(defrule comparison (and or-expr
			 (* (and ws* comp-operator ws* or-expr))) ;; FIXME
  (:lambda (c)
	   (if (null (second (second c)))
	       (first c)
	       (chain-comparisons c))))

(defrule comp-operator (or ">=" "<=" "!=" "<" ">" "="
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

(defrule or-expr (or (and or-expr ws* "|" ws* xor-expr)
		     xor-expr)
  (:lambda (o)
    (if (and (consp o) (null (second o)))
	(list (alexandria:make-keyword (third o)) (first o) (fifth o))
	o)))

(defrule xor-expr (or (and xor-expr ws* "^" ws* and-expr)
		      and-expr)
  (:lambda (x)
    (if (and (consp x) (null (second x)))
	(list (alexandria:make-keyword (third x)) (first x) (fifth x))
	x)))

(defrule and-expr (or (and and-expr ws* "&" ws* shift-expr)
		      shift-expr)
  (:lambda (a)
    (if (and (consp a) (null (second a)))
	(list (alexandria:make-keyword (third a)) (first a) (fifth a))
	a)))

(defrule shift-expr (or (and shift-expr ws* (or "<<" ">>") ws* a-expr)
			a-expr)
  (:lambda (s)
    (if (and (consp s) (null (second s)))
	(list (alexandria:make-keyword (third s)) (first s) (fifth s))
	s)))

(defrule a-expr (or (and a-expr ws* "+" ws* m-expr)
		    (and a-expr ws* "-" ws* m-expr)
		    m-expr)
  (:lambda (a)
    (if (and (consp a) (null (second a)))
	(list (alexandria:make-keyword (third a)) (first a) (fifth a))
	a)))

(defrule m-expr (or (and m-expr ws* "*" ws* u-expr)
		    (and m-expr ws* "@" ws* u-expr)
		    (and m-expr ws* "//" ws* u-expr)
		    (and m-expr ws* "/" ws* u-expr)
		    (and m-expr ws* "%" ws* u-expr)
		    u-expr)
  (:lambda (m)
    (if (and (consp m) (null (second m)))
	(list (alexandria:make-keyword (third m)) (first m) (fifth m))
	m)))

(defrule u-expr (or (and "-" ws* u-expr)
		    (and "+" ws* u-expr)
		    (and "~" ws* u-expr)
		    power)
  (:lambda (u)
    (if (and (consp u) (member (first u) '(#\- #\+ #\~) :test #'string=))
	(list (alexandria:make-keyword (first u)) (third u))
	u)))

(defrule power (and primary (? (and ws* "**" ws* u-expr)))
  (:lambda (p)
	   (if (null (second p))
	       (first p)
	       (list :** (first p) (fourth (second p))))))

;;; primary

(defrule primary atom) ; attributeref subscription slicing call

;;; atom

(defrule atom (or identifier literal)) ; enclosure
