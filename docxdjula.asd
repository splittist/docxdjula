;;;; docxdjula.asd

(asdf:defsystem #:docxdjula
  :description "DOCX templating using Djula"
  :author "John Q. Splittist <splittist@splittist.com>"
  :license "MIT"
  :version "0.0.1"
  :serial t
  :depends-on (#:djula #:docxplora #:cl-ppcre #:cl-interpol)
  :components ((:file "package")
	       (:file "docxdjula")))
