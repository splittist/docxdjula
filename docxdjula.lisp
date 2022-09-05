;;;; docxdjula.lisp

(cl:in-package #:docxdjula)

(defgeneric compile-part (part)
  (:method ((part opc:opc-xml-part))
    (let* ((root (opc:xml-root part))
	   (str (plump:serialize root nil))
	   (tidy-str (tidy-xml str)))
      (djula::compile-string tidy-str))))

(defun tidy-mdp-string (str)
  (tidy-xml (with-output-to-string (s)
	      (lquery:$1 (initialize str)
			 "w::body"
			 (contents)
			 (not "w::sectPr")
			 (serialize s)))))

(defgeneric get-xml-part-string (part)
  (:method ((part opc:opc-xml-part))
    (plump:serialize (opc:xml-root part) nil)))

(defgeneric compile-main-document-part-for-insertion (part)
  (:method ((part opc:opc-xml-part))
    (let* ((str (tidy-mdp-string (get-xml-part-string part))))
      (djula::compile-string str))))

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
	    (with-accessors ((tpf template-part-functions)
			     (tf djula::template-file)
			     (mdf template-main-document-function)) template
	      #'(lambda (destination)
		  (typecase destination
		    ((or string pathname)
		     (let* ((document (open-document tf))
			    (package (opc-package document)))
		       (maphash
			#'(lambda (name function)
			    (let ((result (with-output-to-string (s) (funcall function s)))
				  (part (get-part-by-name document name :xml t)))
			      (setf (opc:xml-root part) (plump:parse result))))
			tpf)
		       (opc:save-package package destination)))
		    (stream
		     (funcall mdf destination))
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

(defclass docx-file-store (djula:filesystem-template-store)
  ())

(defmethod djula:fetch-template ((store docx-file-store) name)
  (with-slots (djula::current-path)
      store
    (setf djula::current-path name)
    (and name
	 (let* ((template (djula:find-template store name))
		(document (open-document template))
		(mdp (main-document document)))
	   (tidy-mdp-string (get-xml-part-string mdp))))))

(defparameter +NEWLINE-XML+ "</w:t><w:br /><w:t xml:space=\"preserve\">")

(djula::def-filter :xlinebreaksbr (it)
  (cl-ppcre:regex-replace-all "\\n" (princ-to-string it) +NEWLINE-XML+))

#|
(setf djula:*current-compiler* (make-instance 'docx-compiler))
(setf djula:*current-store* (make-instance 'docx-file-store))
(djula:add-template-directory "/users/cabox/workspace/")
(djula:compile-template* tmpname)
|#

(defun render-docx-template (template outfile &rest vars)
  (let ((djula::*template-arguments* vars))
    (funcall template outfile)))

;;; utilities

(defun list-tags-str (cleanstr) 
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

(defun split/ws (string)
  (split-sequence:split-sequence-if (alexandria:rcurry #'member '(#\Space #\Tab) :test #'equal)
				    string
				    :remove-empty-subseqs t))

(defun extract-useful-items (tags-list)
  (let ((parsed-tags (mapcar #'djula::parse-template-string tags-list))
	(variables '())
	(ifs '())
	(fors '())
	(others '()))
    (dolist (tag-list parsed-tags)
      (alexandria:destructuring-case (car tag-list)
	((:unparsed-variable v-string)
	 (pushnew (string-trim " " v-string) variables :test (function string-equal)))
	((:unparsed-tag t-string)
	 (destructuring-bind (tag-name &rest rest)
	     (split/ws t-string)
	   (cond
	     ((string-equal "if" tag-name)
	      (cond
		((= 1 (length rest))
		 (pushnew (first rest) variables :test (function string-equal)))
		((= 2 (length rest))
		 (pushnew (second rest) variables :test (function string-equal)))
		(t (push rest ifs))))
	     ((string-equal "for" tag-name)
	      (push rest fors))
	     (t
	      (unless (member tag-name `("endif" "else") :test (function string-equal))
		(push (cons tag-name rest) others))))))
	((t &rest rest)
	 (push rest others))))
    (values (sort variables (function string-lessp)) ifs fors others)))
