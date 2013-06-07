(in-package #:scrape)

(defvar *the-form-set* (make-hash-table :test #'equal)
  "Used to enable eq v.s. equal.")

(defun intern-into-the-form-set (form)
  (let ((x (gethash form *the-form-set* '#1=#:nope)))
    (when (eq '#1# x)
      (setf x
            (setf (gethash form *the-form-set*)
                  form)))
    x))

(defun external-symbols-of-package (package)
  (let (result)
    (do-external-symbols (s (or (find-package package)
                                (error "Package ~S not found." package)))
      (push (string-downcase s) result)) 
    result))

(memoize 'external-symbols-of-package)

(defvar *last-fetch* nil "Useful for debugging.")

(defun fetch-into-h-sexpr (url)
  (setf *last-fetch* (fetch-into-h-sexpr-1 (intern-into-the-form-set url))))

(defun fetch-into-h-sexpr-1 (url)
  (parse (http-request url) (make-lhtml-builder)))

(memoize 'fetch-into-h-sexpr-1)

(defun collect-matches-in-sexpr (sexpr match-func)
  "Apply match-fun to nodes in sexpr, collect non-nil results, don't recure into those."
  (let (result)
    (labels ((recure (node)
               (let ((x? (funcall match-func node)))
                 (when x?
                   (push x? result)))
               (when (listp node)
                 (when (car node) (recure (car node)))
                 (when (cdr node) (recure (cdr node))))))
      (recure sexpr)
      result)))

(defun tidy-and-ok-name-list (name-list)
  (setf name-list (sort name-list #'string<))
  ; (assert (equal name-list (remove-duplicates name-list :test #'string-equal)))
  name-list)

(defgeneric collect-names (t))

(defmethod collect-names :around ((x symbol))
  (sort 
   (remove-duplicates (call-next-method x) :test #'string=)
   #'string<))


(defun summarize-status (lib)
  (let ((names (collect-names lib))
        (externals (external-symbols-of-package lib)))
    (format t "~2&Library: ~S" lib)
    (format t "~& exports: ~D symbols" (length externals))
    (format t "~& doc: ~D symbols" (length names))
    (format t "~& Doc'd but not exported: ~S" (set-difference names externals :test #'string=))
    (format t "~& Exported but not doc'd: ~S" (set-difference externals names :test #'string=))))

;; Note the followind status report is frustrating...
;; (ql:quickload *libraries-we-want*)
;; (loop for i in *libraries-we-want* do (summarize-status i))


(defparameter *libraries-we-want*
  '(cl-gd cl-who cl-webdav cl-unicode flexi-streams documentation-template cl-fad cl-ppcre drakma))

(defun emit-files ()
  (loop
     for lib in *libraries-we-want*
       as redirect-dir = (or (probe-file
                              (merge-pathnames "../l1sp-org/redirects/"
                                               (asdf:component-pathname
                                                (asdf:find-system "l1sp-scrape"))))
                             (error "Didn't fine l1sp-org/redirect"))
     as redirect-file = (make-pathname
                         :name (string-downcase lib)
                         :type "txt"
                         :defaults redirect-dir)
     do (with-open-file (*standard-output* redirect-file
                                           :direction :output
                                           :if-exists :rename-and-delete)
          (emit-redirects-txt lib))))


