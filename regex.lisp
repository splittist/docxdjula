;;;; regex.lisp

(cl:in-package #:docxdjula)

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

(defun ensure-space-preservation (str)
    (cl-ppcre:regex-replace-all
     #?rx"
          (?s)              # . matches #\Newline
          <w:t>             # text tag without xml:space = preserve
          ((?:(?!<w:t>).)*) # any amount of non-text tag
          ({{.*?}}|
           {%.*?%}|
           {\#.*?\#}|
           {\$.*?\$}|
           {_.*?_})         # the subsequent template tag
          " 
     str
     #?rx"<w:t\ xml:space=\"preserve\">\1\2"))

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
  (setf str (ensure-space-preservation str))
  (setf str (strip-enclosing-wml-tag str "p"))
  (setf str (strip-enclosing-wml-tag str "r"))
  (setf str (strip-enclosing-wml-tag str "tr"))
  (setf str (strip-enclosing-wml-tag str "tc"))
  (setf str (clean-tags str))
  str)
