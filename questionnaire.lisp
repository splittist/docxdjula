;;;; questionnaire.lisp

(cl:in-package #:docxdjula)

;; A questionnaire is a table with a first row including cells with
;; the text "variable" and "answer". The entries in the corresponding
;; column are extracted into an alist, with the variables interned
;; as keywords.

(defun find-header (row)
  (let ((cells (lquery:with-master-document (row)
		 (lquery:$ ">w::tc")))
	variable
	answer)
    (dotimes (count (length cells))
      (let ((cell (aref cells count)))
	(when (string-equal "variable" (plump:render-text cell))
	  (setf variable count))
	(when (string-equal "answer" (plump:render-text cell))
	  (setf answer count))
	(when (and variable answer)
	  (return (values variable answer)))))))

(defun find-questionnaire (document)
  (let* ((md (main-document document))
	 (root (opc:xml-root md))
	 (tbls (lquery:with-master-document (root)
		 (lquery:$ "w::tbl"))))
    (dotimes (i (length tbls))
      (let* ((tbl (aref tbls i))
	     (first-row (lquery:with-master-document (tbl)
			   (lquery:$1 "w::tr"))))
	(multiple-value-bind (variable-column answer-column)
	    (find-header first-row)
	  (when (and variable-column answer-column)
	    (return (values tbl variable-column answer-column))))))))

(defgeneric extract-template-arguments (document)
  (:method ((document document))
    (multiple-value-bind (tbl variable-column answer-column)
	(find-questionnaire document)
      (when tbl
	(let (result
	      (rows (lquery:with-master-document (tbl)
		      (lquery:$ ">w::tr"))))
	  (dotimes (i (length rows))
	    (unless (zerop i) ;; skip first row
	      (let ((cells (lquery:with-master-document ((aref rows i))
			     (lquery:$ ">w::tc"))))
		(push (cons (alexandria:make-keyword
			     (string-upcase (plump:render-text (aref cells variable-column))))
			    (plump:render-text (aref cells answer-column)))
		    result))))
	  (reverse result))))))

(defmethod extract-template-arguments ((pathname pathname))
  (let ((document (open-document pathname)))
    (extract-template-arguments document)))

(defmethod extract-template-arguments ((namestring string))
  (extract-template-arguments (pathname namestring)))
