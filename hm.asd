;;;; hm.asd

(asdf:defsystem #:hm
  :serial t
  :description "Write dot files for archaeological sequence diagrams
  and chronological models"
  :author "Thomas S. Dye <tsd@tsdye.com>"
  :version "0.1"
  :license "GPL V3"
  :depends-on (#:graph
               #:graph-matrix
               #:graph-dot
               #:cl-csv
               #:do-urlencode
               #:fset)
  :components ((:static-file "COPYING")
               (:file "package")
               (:file "hm")))

