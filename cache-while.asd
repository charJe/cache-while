(in-package #:asdf-user)

(defsystem cache-while
  :description "A Common Lisp macro for defining temporary caches that
invalidate based on expressions evaluating to different values."
  :version "0.0.0"
  :author "Charles Jackson <charles.b.jackson@protonmail.com>"
  :licence "GPL3"
  :serial t
  :components
  ((:file "package")
   (:file "cache-while")))
