;;;; docxdjula.asd

(asdf:defsystem #:docxdjula
  :description "DOCX templating using Djula"
  :author "John Q. Splittist <splittist@splittist.com>"
  :license "MIT"
  :version "0.0.1"
  :serial t
  :depends-on (#:djula #:docxplora
		       #:cl-ppcre #:cl-interpol
		       #:uiop #:alexandria #:split-sequence
		       #:lquery
		       #:esrap #:lorem-ipsum) ; for gingish
  :components ((:file "package")
	       (:file "regex")
	       (:file "ginjish-package")
	       (:file "ginjish-builtins")
	       (:file "ginjish-grammar")
	       (:file "ginjish-compiler")
	       (:file "docxdjula")
	       (:file "questionnaire")))

(asdf:defsystem #:ginjish-tests
  :depends-on (#:docxdjula #:parachute)
  :components ((:file "ginjish-tests")))
