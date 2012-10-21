;;;; package.lisp

(defpackage #:wimelib-utilities
  (:use #:cl)
  (:export #:with-collectors
	   #:collecting
	   #:collect
	   #:split-at
	   #:split
	   #:with-unique-names
	   #:once-only))

