;;;; ginjish-package.lisp

(cl:defpackage #:ginjish-specials
  (:use #:cl))

(cl:defpackage #:ginjish-conditions
  (:use #:cl)
  (:export
   #:template-error
   #:template-syntax-error
   #:signal-template-syntax-error
   #:template-compilation-error
   #:signal-template-compilation-error
   #:template-runtime-error
   #:template-filter-error
   #:signal-template-filter-error
   #:template-test-error
   #:signal-template-test-error
   #:template-not-found-error
   #:signal-template-not-found-error
   #:template-block-not-found-error
   #:signal-template-block-not-found-error))

(cl:defpackage #:ginjish-utils
  (:use #:cl)
  (:export
   #:symbolize
   #:read-keyword
   #:name-equal
   #:load-value
   #:context
   #:context-map
   #:context-parent
   #:make-context
   #:truthy
   #:print-expression
   #:print-expression-to-string
   #:copy-map
   #:getf-name
   #:to-list))

(cl:defpackage #:ginjish-grammar
  (:use #:cl #:esrap)
  (:export
   #:parse-template
   #:parse-number))

(cl:defpackage #:ginjish-compiler
  (:use #:cl #:ginjish-utils)
  (:export
   #:gt
   #:gte
   #:lt
   #:lte))

(cl:defpackage #:ginjish-builtins
  (:use #:cl #:ginjish-utils))

(cl:defpackage #:ginjish.filters) ; FIXME parameterize

(cl:defpackage #:ginjish.tests) ; FIXME parameterize

