(in-package #:scrape)

;;;; Alexandria

(defmethod base-url-of-library ((lib (eql 'alexandria)))
  "http://common-lisp.net/project/alexandria/draft/alexandria.html")

(defun scrape-alexandria-doc ()
  (let* ((url (base-url-of-library 'alexandria))
         (sexpr (fetch-into-h-sexpr url))
         (names-and-anchors
          (collect-matches-in-sexpr
           *last-fetch*
           #'(lambda (x) (match x
                           (`((:p nil (:a ((:name ,anchor))) ,@_)
                              (:div ((:class ,_)) ,_ (:b nil ,name) ,@_)
                              ,@_)
                             (list name anchor)))))))
    names-and-anchors))
  
(defmethod collect-names ((lib (eql 'alexandria)))
  (mapcar #'first (scrape-alexandria-doc)))

(defmethod  emit-redirects-txt ((lib (eql 'alexandria)))
  (loop
     with base-url = (base-url-of-library 'alexandria)
     for (name anchor) in (scrape-alexandria-doc)
     unless (find #\space name) ;; dont include the cool setf methods
     do (format t "~&~(~A ~a t ~a#~a~)" lib name base-url (drakma:url-encode anchor :ascii))))

#| FYI 
> (summarize-status 'alexandria)

Library: alexandria
 exports: 206 symbols
 doc: 118 symbols
 Doc'd but not exported: ("(setf lastcar)" "(setf last-elt)" "(setf first-elt)")
 Exported but not doc'd: ("non-negative-short-float" "negative-rational" "non-positive-short-float-p" "unwind-protect-case" "extremum" "negative-short-float" "non-negative-fixnum" "non-negative-integer-p" "non-positive-rational" "positive-single-float" "negative-double-float-p" "negative-real-p" "non-negative-long-float-p" "copy-stream" "non-positive-fixnum" "non-negative-integer" "non-positive-float" "positive-rational" "negative-float-p" "non-negative-double-float-p" "negative-long-float-p" "simple-reader-error" "non-negative-real-p" "copy-file" "non-positive-integer" "non-negative-real" "negative-real" "non-positive-single-float" "non-negative-short-float-p" "non-positive-integer-p" "with-input-from-file" "positive-fixnum-p" "positive-real" "positive-single-float-p" "non-positive-double-float-p" "positive-long-float-p" "non-negative-fixnum-p" "non-positive-long-float-p" "negative-integer" "simple-program-error" "positive-short-float-p" "non-negative-long-float" "positive-integer-p" "simple-style-warning" "non-positive-real" "non-positive-double-float" "non-positive-long-float" "positive-integer" "non-negative-float" "non-negative-double-float" "non-positive-short-float" "positive-real-p" "negative-fixnum" "negative-short-float-p" "positive-fixnum" "positive-rational-p" "positive-long-float" "non-negative-float-p" "non-positive-rational-p" "negative-rational-p" "positive-double-float" "simple-parse-error" "non-positive-single-float-p" "non-positive-real-p" "positive-short-float" "negative-float" "negative-single-float" "with-output-to-file" "non-negative-single-float-p" "negative-long-float" "non-negative-rational" "rassoc-value" "assoc-value" "negative-integer-p" "write-byte-vector-into-file" "non-positive-fixnum-p" "ordinary-lambda-list-keywords" "non-negative-rational-p" "positive-double-float-p" "positive-float-p" "required-argument" "destructuring-ecase" "write-string-into-file" "negative-double-float" "ignore-some-conditions" "negative-single-float-p" "non-negative-single-float" "non-positive-float-p" "destructuring-ccase" "positive-float" "negative-fixnum-p")
|#
