;;;; hm.asd

(asdf:defsystem #:hm
  :serial t
  :description "Write dot files for archaeological sequence diagrams"
  :author "Thomas S. Dye <tsd@tsdye.com>"
  :version "0.1"
  :license "GPL V3"
  :depends-on (#:graph
               #:graph-matrix
               #:graph-dot
               #:cl-csv)
  :components ((:static-file "COPYING")
               (:file "package")
               (:file "hm")))

