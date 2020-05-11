;;;; ginjish-package.lisp

(cl:defpackage #:ginjish-grammar
  (:use #:cl #:esrap))

(cl:defpackage #:ginjish-compiler
  (:use #:cl))

(cl:defpackage #:ginjish-builtins
  (:use #:cl))

(cl:defpackage #:ginjish.filters) ; FIXME parameterize

(cl:defpackage #:ginjish.tests) ; FIXME parameterize

