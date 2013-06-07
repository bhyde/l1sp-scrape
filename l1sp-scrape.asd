(defsystem #:l1sp-scrape
  :serial t
  :depends-on (#:fare-memoization #:parse-number
                                  #:optima #:optima.ppcre #:fare-quasiquote-optima
                                  #:drakma #:closure-html)
  :components ((system #:closure-html :around-compile #'(lambda (thunk)
                                                          (let ((** :upcase))
                                                            (funcall thunk))))
               (:file "packages")
               (:file "utilities")
               (:file "weitz")
               #+nil(:file "foo")
               ))
