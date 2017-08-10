;;;; hm.asd

(asdf:defsystem #:hm
  :name "hm"
  :serial t
  :description "Write dot files for archaeological sequence diagrams
  and chronological models"
  :author "Thomas S. Dye <tsd@tsdye.com>"
  :maintainer "Thomas S. Dye <tsd@tsdye.com>"
  :homepage "http://tsdye.github.io/harris-matrix/"
  :version "0.2"
  :license "GPL V3"
  :depends-on (#:graph
               #:graph-matrix
               #:graph-dot
               #:cl-csv
               #:fset
               #:py-configparser
               #:cl-colors
               #:let-over-lambda
               #:named-readtables)
  :components ((:static-file "COPYING")
               (:file "src/package")
               (:file "src/config/hm-cfg")
               (:file "src/chronology/hm-chronology")
               (:file "src/hm")))
