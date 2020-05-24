;;;; ginjish-conditions.lisp

(cl:in-package #:ginjish-conditions)

(define-condition template-error (simple-error)
  ()
  (:documentation "All errors signaled by GINJISH are of this type."))

(define-condition template-syntax-error (template-error)
  ()
  (:documentation "Signalled when a syntax error occurs while parsing a template."))

(defmacro signal-template-syntax-error (format-control &rest format-arguments)
  `(error 'template-syntax-error
	  :format-control ,format-control
	  :format-arguments (list ,@format-arguments)))

(define-condition template-compilation-error (template-error)
  ()
  (:documentation "Signalled when an error is encountered compiling a template."))

(defmacro signal-template-compilation-error (format-control &rest format-arguments)
  `(error 'template-compilation-error
	  :format-control ,format-control
	  :format-arguments (list ,@format-arguments)))

(define-condition template-runtime-error (template-error)
  ()
  (:documentation "Parent for errors that could occur when rendering a template."))

(define-condition template-filter-error (template-runtime-error)
  ()
  (:documentation "Signalled when an error is encountered applying a filter."))

(defmacro signal-template-filter-error (format-control &rest format-arguments)
  `(error 'template-filter-error
	  :format-control ,format-control
	  :format-arguments (list ,@format-arguments)))

(define-condition template-test-error (template-runtime-error)
  ()
  (:documentation "Signalled when an error is encountered applying a test."))

(defmacro signal-template-test-error (format-control &rest format-arguments)
  `(error 'template-test-error
	  :format-control ,format-control
	  :format-arguments (list ,@format-arguments)))

(define-condition template-not-found-error (template-runtime-error)
  ()
  (:documentation "Signalled when a specified template cannot be found."))

(defmacro signal-template-not-found-error (format-control &rest format-arguments)
  `(error 'template-not-found-error
	  :format-control ,format-control
	  :format-arguments (list ,@format-arguments)))

(define-condition template-block-not-found-error (template-runtime-error)
  ()
  (:documentation "Signalled when a specified blcok cannot be found."))

(defmacro signal-template-block-not-found-error (format-control &rest format-arguments)
  `(error 'template-block-not-found-error
	  :format-control ,format-control
	  :format-arguments (list ,@format-arguments)))
