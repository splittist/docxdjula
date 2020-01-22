;;;; docxdjula.lisp

(cl:in-package #:docxdjula)

(defclass compiled-template ()
  ((%pathname :initarg :pathname :accessor template-pathname)
   (%mdp-function :initarg :mdp-function :accessor template-mdp-function)
   (%part-functions :initarg :part-functions :accessor template-part-functions))
  (:default-initargs :part-functions (make-hash-table :test 'equalp)))

(defmethod print-object ((object compiled-template) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~A" (template-pathname object))))

(defun compile-template (pathname)
  (let ((document (open-document pathname))
	(template (make-instance 'compiled-template :pathname pathname)))
    (compile-parts document template)))

(defgeneric compile-parts (document template)
  (:method ((document document) (template compiled-template))
    (let ((parts (alexandria:flatten
		  (list (main-document document)
			(endnotes document)
			(footnotes document)
			(headers document)
			(footers document))))
	  (func-table (template-part-functions template)))
      (dolist (part parts)
	(setf (gethash (opc:part-name part) func-table)
	      (compile-part part)))
      template)))

(defgeneric compile-part (part)
  (:method ((part opc:opc-xml-part))
    (let* ((root (opc:xml-root part))
	   (str (plump:serialize root nil))
	   (tidy-str (tidy-xml str)))
      (djula::compile-string tidy-str))))

(defgeneric render-template (template outpath &rest vars)
  (:method ((template compiled-template) outpath &rest vars)
    (let ((djula::*template-arguments* vars))
      (let* ((document (open-document (template-pathname template)))
	     (package (opc-package document)))
	(maphash
	 #'(lambda (name function)
	     (let ((result
		    (with-output-to-string (s)
		      (funcall function s)))
		   (part (opc:get-part package name)))
	       (docxplora::ensure-xml part) ;; FIXME (a) ensure-xml should return part (b) should be part of get protocol
	       (setf (opc:xml-root part)
		     (plump:parse result))))
	 (template-part-functions template))
	(opc:save-package package outpath)))))

;; Djula meta stuff

(defclass compiled-docx-template ()
  ((%compiled-template :accessor djula::compiled-template)
   (%linked-templates :initform '() :accessor djula::linked-templates)
   (%template-file :initarg :template-file :initform (error "Provide the template file") :accessor djula::template-file)
   (%template-file-write-date :accessor djula::template-file-write-date)
   (%part-functions :initarg :part-functions :accessor template-part-functions))
  (:default-initargs :part-functions (make-hash-table :test 'equalp))
  (:metaclass closer-mop:funcallable-standard-class))

(defmethod print-object ((object compiled-docx-template) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~A" (djula::template-file object))))

(defmethod djula::compile-template-file ((template compiled-docx-template))

  (setf (djula::template-file-write-date template)
	(file-write-date (djula::template-file template)))

  (let ((djula::*block-alist* nil)
	(djula::*linked-templates* nil))
    (let* ((document (open-document (djula::template-file template)))
	   (parts (alexandria:flatten
		   (list (main-document document)
			 (endnotes document)
			 (footnotes document)
			 (headers document)
			 (footers document))))
	   (func-table (template-part-functions template)))
      (dolist (part parts)
	(setf (gethash (opc:part-name part) func-table)
	      (compile-part part)))
      (setf (djula::compiled-template template)
	    (with-accessors ((tpf template-part-functions) (tf djula::template-file)) template
	      #'(lambda (outpath)
		  (let* ((document (open-document tf))
			 (package (opc-package document)))
		    (maphash
		     #'(lambda (name function)
			 (let ((result (with-output-to-string (s) (funcall function s)))
			       (part (opc:get-part package name)))
			   (docxplora::ensure-xml part) ;; FIXME (a) ensure-xml should return part (b) should be part of get protocol
			   (setf (opc:xml-root part) (plump:parse result))))
		     tpf)
		    (opc:save-package package outpath))))
	    (djula::linked-templates template)
	    djula::*linked-templates*))))

(defmethod djula::template-changed ((template compiled-docx-template))
  (or (not (cl-fad:file-exists-p (djula::template-file template)))
      (not (= (djula::template-file-write-date template)
	      (file-write-date (djula::template-file template))))))

(defmethod initialize-instance :after ((template compiled-docx-template) &rest initargs)
  (declare (ignore initargs))
  (djula::compile-template-file template)
  (closer-mop:set-funcallable-instance-function
   template
   (if (not (uiop:featurep :djula-prod))
       ;; recompile templates that have changed
       (lambda (outpath)
	 (let ((template-file-write-date (djula::template-file-write-date template)))
	   (when (or (not (equalp (file-write-date (djula::template-file template))
				  template-file-write-date))
		     (loop for linked-template in (djula::linked-templates template)
			thereis (djula::template-changed linked-template)))
	     (djula::compile-template-file template)))
	 (funcall (djula::compiled-template template) outpath))
       ;; don't check
       (djula::compiled-template template))))

(defclass docx-compiler (djula:compiler)
  ())

(defmethod djula:compile-template ((compiler docx-compiler) name &optional (error-p t))
  (alexandria:when-let ((template-file (djula:find-template* name error-p)))
    (let ((djula::*linked-templates* nil)
	  (djula::*block-alist* nil))
      (make-instance 'compiled-docx-template :template-file template-file))))

#|
(setf djula::*current-compiler* (make-instance 'docx-compiler))
(djula:add-template-directory "/users/cabox/workspace/")

|#

(defun render-docx-template (template outfile &rest vars)
  (let ((djula::*template-arguments* vars))
    (funcall template outfile)))


;;; xml string hacking

(cl-interpol:enable-interpol-syntax)

;; djula uses:
;; {{ variable
;; {% tag
;; {# comment (can enclose other tags)
;; {$ verbatim (ditto)
;; {_ translation

(defun strip-tags-between-braces (str)
  ;; remove all tags between opening { and {,%,#,$ or _, and closing },%,#,$ or _ and }
  (cl-ppcre:regex-replace-all
   #?rx"
     (?s)          # . matches #\Newline
     (?<={)        # just seen a #\{
     (<[^>]*>)+    # followed by one or more tag-like things
     (?=[\{%\#$_]) #  and a #\{, #\%, #\#, #\$ or #\_
     |             # or
     (?<=[_$\#%}]) # just seen a #\}, #\%, #\#, #\$ or #\_
     (<[^>]*>)+    # followed by one or more tag-like things
     (?=\})        #  and a #\}
   "
   str 
   ""))

(defun strip-tags-within-braces (str)
  (flet ((stags (match)
	   (cl-ppcre:regex-replace-all
	    #?rx"
	      (?s)                 # . matches #\Newline
	      </w:t>               # closing text tag
	      .*?                  # non-greedy anything up to
	      (<w:t>|<w:t [^>]*>)  # and including, next opening text tag
	      # That is, extends text tag between braces, deleting non-text elements
              "
	    match
	    "")))
    (cl-ppcre:regex-replace-all
     #?rx"
        (?s)               # . matches #\Newline
        {{                 # opening pair
        (?:(?!}}).)*       # any amount of thing until looking at closing pair (not included)
        |
        {%(?:(?!%}).)*|    # same for {% tags
        {\#(?:(?!\#}).)*|  # and for  {# comments
        {$(?:(?!$}).)*|    # and for  {$ verbatim
        {_(?:(?!_}).)*     # and      {_ translation
        "
     str
     #'stags
     :simple-calls t)))

(defun strip-enclosing-wml-tag (str p)
  (cl-ppcre:regex-replace-all
   #?rx"
      (?s)                     # . matches #\Newline
      <w:${p}[\ >]             # opening tag
      (?:(?!<w:${p}[ >]).)*    # stuff that isn't an opening tag
      ({{|{%|{\#|{$|{_)${p}\   # opening braces (group 0 below) followed by tagname and a #\Space
      ([^}%\#$_]*              # stuff that isn't a tag char FIXME too amy tag chars?
       (?:}}|%}|\#}|$}|_}))    #  until and including closing braces (group 1 below)
      .*?</w:${p}>             # stuff until the next closing tag (not captured)
      "
   str
   '(0 " " 1))) ; replace with groups 0 and 1 (zero-based in list format)

(defun clean-tags (str)
  (let ((sublist '((#\" . #\“) (#\" . #\”) (#\' . #\‘) (#\' . #\’))))
    (flet ((subs (match)
	     (dolist (pair sublist match)
	       (setf match (substitute (car pair) (cdr pair) match)))))
      (cl-ppcre:regex-replace-all
       "(?<=\{[\{%#$_])(?:.*?)(?=[\}%#$_]})"
       str
       #'subs
       :simple-calls t))))

(defun tidy-xml (str)
  (setf str (strip-tags-between-braces str))
  (setf str (strip-tags-within-braces str))
  (setf str (strip-enclosing-wml-tag str "p"))
  (setf str (strip-enclosing-wml-tag str "r"))
  (setf str (strip-enclosing-wml-tag str "tr"))
  (setf str (strip-enclosing-wml-tag str "tc"))
  (setf str (clean-tags str))
  str)