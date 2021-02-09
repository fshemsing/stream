(defpackage #:stream-asd
  (:use :cl :asdf))

(defsystem stream
  :name "stream"
  :version "0.0.0"
  :maintainer "Frank Hemsing"
  :description "streams"
  :long-description "lazy streams representing potentially infinite sequences"
  :components ((:file "streams")
	       (:file "stream-functions"
		      :depends-on ("streams"))))
