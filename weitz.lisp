(in-package #:scrape)

;; :symbol-kind is use to match strings like:
;;  "  [Function] " that appear near a symbol's documentation.
(cl-ppcre:define-parse-tree-synonym :symbol-kind
    (:sequence :start-anchor
               (:greedy-repetition 0 nil :whitespace-char-class)
               #\[ 
               (:register (:alternation
                           "Function"
                           "Special variable"
                           "Macro"
                           "Condition type"
                           "Condition"
                           "Accessor"
                           "Standard class"
                           "Method"
                           "Generic Function"
                           "Generic function"
                           "Generic accessors"
                           "Generic accessor"
                           "Generic readers"))
               #\] 
               (:greedy-repetition 0 nil :whitespace-char-class) 
               :end-anchor))

(defun weitz-match-2 (node)
  (match node
    ;; usually the anchor comes before the symbol-kind
    (`(:p nil (:a ((:class "none") (:name ,name)))
          ,(ppcre :symbol-kind *)
          ,@(type list))
      name)
    ;; but sometimes it comes after and includes the symbol and other notes.
    (`(:p nil (:br nil) 
          ,(ppcre :symbol-kind _)
          (:br nil)
          (:a ((:class "none") (:name ,name)) ,@(type list))
          ,@(type list))
      name)
    ;; Then we have lists of generic accessors/readers
    (`((:br nil) 
       (:a ((:class "none") (:name ,name)))
       (:b nil ,(guard n2 (string= n2 name)))
       ,@_)
      name)
    ;; Then we have lists of generic accessors/readers
    (`((:br nil)
       (:a ((:class "none") (:name ,name))
           (:b nil ,(guard n2 (string= n2 name))) ,@_)
       ,@_)
      name)
    

    ))



(defun collect-weitz-names (lib)
  (collect-matches-in-sexpr
   (fetch-into-h-sexpr (base-url-of-library lib))
   #'weitz-match-2))

(macrolet ((defscrape (library-name)
             `(progn
                (defmethod collect-names ((lib (eql ',library-name))) (collect-weitz-names lib))
                (defmethod base-url-of-library ((lib (eql ',library-name)))
                  (format nil "http://weitz.de/~(~A~)/" lib)))))
  (defscrape cl-gd)
  (defscrape cl-who)
  (defscrape cl-webdav)
  (defscrape cl-unicode)
  (defscrape flexi-streams)
  (defscrape documentation-template)
  (defscrape cl-fad)

  (defscrape cl-ppcre)

  (defscrape drakma))

#+nil ;; give up on this for now.
(defmethod collect-names ((lib (eql 'hunchentoot)))
  ;; easier to snarf these by hand.
  (let ((constants '("+http-continue+"
                     "+http-switching-protocols+"
                     "+http-ok+"
                     "+http-created+"
                     "+http-accepted+"
                     "+http-non-authoritative-information+"
                     "+http-no-content+"
                     "+http-reset-content+"
                     "+http-partial-content+"
                     "+http-multi-status+"
                     "+http-multiple-choices+"
                     "+http-moved-permanently+"
                     "+http-moved-temporarily+"
                     "+http-see-other+"
                     "+http-not-modified+"
                     "+http-use-proxy+"
                     "+http-temporary-redirect+"
                     "+http-bad-request+"
                     "+http-authorization-required+"
                     "+http-payment-required+"
                     "+http-forbidden+"
                     "+http-not-found+"
                     "+http-method-not-allowed+"
                     "+http-not-acceptable+"
                     "+http-proxy-authentication-required+"
                     "+http-request-time-out+"
                     "+http-conflict+"
                     "+http-gone+"
                     "+http-length-required+"
                     "+http-precondition-failed+"
                     "+http-request-entity-too-large+"
                     "+http-request-uri-too-large+"
                     "+http-unsupported-media-type+"
                     "+http-requested-range-not-satisfiable+"
                     "+http-expectation-failed+"
                     "+http-failed-dependency+"
                     "+http-internal-server-error+"
                     "+http-not-implemented+"
                     "+http-bad-gateway+"
                     "+http-service-unavailable+"
                     "+http-gateway-time-out+"
                     "+http-version-not-supported+")))
  (nconc constants (collect-weitz-names lib))))


;; other Weitz libraries...
;;   hunchentoot
;;   capi-overview
;;   chunga
;;   cl-dongle
;;   cl-interpol
;;   cl-wbxml
;;   fm-plugin-tools
;;   html-extract
;;   html-template
;;   lw-add-ons
;;   lw-doc
;;   lw-win
;;   midgets
;;   odd-streams
;;   rdnzl
;;   regex-plugin
;;   starter-pack
;;   tbnl
;;   url-rewrite
