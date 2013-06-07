(in-package #:cl-user)

(defpackage #:scrape
  (:use #:common-lisp #:fare-memoization #:parse-number
        #:optima #:optima.ppcre
        #:drakma #:closure-html))
