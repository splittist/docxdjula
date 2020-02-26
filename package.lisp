;;;; package.lisp

(in-package #:cl)

(defpackage #:docxdjula
  (:nicknames #:com.splittist.docxdjula)
  (:use #:cl #:docxplora)
  (:export
   #:docx-compiler
   #:render-docx-template
   #:extract-template-arguments))
