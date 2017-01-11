;;;; package.lisp

(defpackage #:hm
  (:use :common-lisp
        :cl-colors
        :py-configparser
        :alexandria)
  (:export :configure-archaeological-sequence
           :graphviz-hsv-string
           :make-default-configuration
           :write-default-configuration
           :write-configuration
           :write-Graphviz-style-configuration
           :write-general-configuration
           :convert-csv-config-to-ini
           :configuration-errors?
           :reset-option
           :set-input-file
           :set-dot-file
           :read-configuration-from-files
           :show-configuration-options
           :show-configuration-sections
           :create-chronology-graph))
