;;;; docxdjula.lisp

(cl:in-package #:docxdjula)

(defgeneric compile-part (part)
  (:method ((part opc:opc-xml-part))
    (let* ((root (opc:xml-root part))
	   (str (plump:serialize root nil))
	   (tidy-str (tidy-xml str)))
      (djula::compile-string tidy-str))))

(defgeneric compile-main-document-part-for-insertion (part)
  (:method ((part opc:opc-xml-part))
    (let* ((root (opc:xml-root part))
	   (body (first (plump:get-elements-by-tag-name root "w:body")))
	   (new-root (plump:make-root (plump:clone-children body))))
      (mapcar #'plump:remove-child (plump:get-elements-by-tag-name new-root "w:sectPr"))
      (let* ((str (plump:serialize new-root nil))
	     (tidy-str (tidy-xml str)))
	(djula::compile-string tidy-str)))))

(defclass compiled-docx-template ()
  ((%compiled-template :accessor djula::compiled-template)
   (%linked-templates :initform '() :accessor djula::linked-templates)
   (%template-file :initarg :template-file :initform (error "Provide the template file") :accessor djula::template-file)
   (%template-file-write-date :accessor djula::template-file-write-date)
   (%part-functions :initarg :part-functions :accessor template-part-functions)
   (%mdp-function :accessor template-main-document-function))
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
      (setf (template-main-document-function template)
	    (compile-main-document-part-for-insertion (main-document document))
	    (djula::compiled-template template)
	    (with-accessors ((tpf template-part-functions) (tf djula::template-file)) template
	      #'(lambda (destination)
		  (typecase destination
		    ((or string pathname)
		     (let* ((document (open-document tf))
			    (package (opc-package document)))
		       (maphash
			#'(lambda (name function)
			    (let ((result (with-output-to-string (s) (funcall function s)))
				  (part (get-part-by-name document name t)))
			      (setf (opc:xml-root part) (plump:parse result))))
			tpf)
		       (opc:save-package package destination)))
		    (stream
		     (funcall (template-main-document-function template) destination))
		    (t (error "Don't know how to operate on ~A" destination)))))
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
(setf djula:*current-compiler* (make-instance 'docx-compiler))
(djula:add-template-directory "/users/cabox/workspace/")
(djula:compile-template* tmpname)
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
        {\$(?:(?!\$}).)*|    # and for  {$ verbatim
        {_(?:(?!_}).)*     # and      {_ translation
        "
     str
     #'stags
     :simple-calls t)))

(defparameter +all-the-tags+
   #?rx"
     (?s)
     {{                 # opening pair
     (?:(?!}}).)*       # any amount of thing until looking at closing pair
     }}                 # the closing pair
     |
     {%(?:(?!%}).)*%}|    # same for {% tags
     {\#(?:(?!\#}).)*\#}|  # and for  {# comments
     {\$(?:(?!\$}).)*$}|    # and for  {$ verbatim
     {_(?:(?!_}).)*_}     # and      {_ translation
     ") 

(defun strip-enclosing-wml-tag (str p)
  (cl-ppcre:regex-replace-all
   #?rx"
      (?s)                     # . matches #\Newline
      <w:${p}[\ >]             # opening tag
      (?:(?!<w:${p}[ >]).)*    # stuff that isn't an opening tag
      ({{|{%|{\#|{\$|{_)${p}\   # opening braces (group 0 below) followed by tagname and a #\Space
      (.*?                     # stuff that isn't a tag char FIXME nested tags
       (?:}}|%}|\#}|\$}|_}))    #  until and including closing braces (group 1 below)
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
       +all-the-tags+
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

;;; utilities

(defun list-tags-str (cleanstr) ;; FIXME - include closing or exclude opening delims.
  (cl-ppcre:all-matches-as-strings
   +all-the-tags+
   cleanstr))

(defun list-tags-document (path)
  (let* ((document (open-document path))
	 (parts (alexandria:flatten
		 (list (main-document document)
		       (endnotes document)
		       (footnotes document)
		       (headers document)
		       (footers document)))))
    (alexandria:mappend
     (lambda (part)
       (let* ((root (opc:xml-root part))
	      (str (plump:serialize root nil))
	      (tidystr (tidy-xml str)))
	 (list-tags-str tidystr)))
     parts)))
