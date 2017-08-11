;;;; hm-test.asd  Tests for the hm application

(asdf:defsystem #:hm-test
  :author "Thomas S. Dye <tsd@tsdye.com>"
  :license "GPL V3"
  :depends-on (#:graph
               #:hm
               #:stefil)
  :components ((:file "test/package")
               (:file "test/hm")))
