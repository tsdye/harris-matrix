;;;; hm.asd

(asdf:defsystem #:hm
  :name "hm"
  :serial t
  :description "Write dot files for archaeological sequence diagrams
  and chronological models"
  :author "Thomas S. Dye <tsd@tsdye.com>"
  :maintainer "Thomas S. Dye <tsd@tsdye.com>"
  :homepage "http://tsdye.github.io/harris-matrix/"
  :version "0.3"
  :license "GPL V3"
  :depends-on (#:graph
               #:graph/matrix
               #:graph/dot
               #:cl-csv
               #:fset
               #:py-configparser
               #:cl-colors
               #:fare-memoization
               #:inferior-shell)
  :components ((:static-file "COPYING")
               (:file "src/package")
               (:file "src/macros/hm-macros")
               (:file "src/config/hm-cfg")
               (:file "src/chronology/hm-chronology")
               (:file "src/color/hm-color")
               (:file "src/files/hm-file")
               (:file "src/elements/hm-elements")
               (:file "src/data-structures/hm-data-structures")
               (:file "src/memo/hm-memo")
               (:file "src/fas/hm-fas")
               (:file "src/hm")))
