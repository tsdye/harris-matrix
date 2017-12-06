;;; hm-macros.lisp

;; Copyright (C) Thomas Dye 2017

;; Licensed under the Gnu Public License Version 3 or later

(in-package #:hm)
(named-readtables:in-readtable lol:lol-syntax)

;; Macros

(defmacro <-dot (seq element dot-attr graph-type verbose)
  (list 'to-dot-macro seq element dot-attr graph-type verbose))

;; From On Lisp, p. 92, a macro for testing macroexpansion
(defmacro mac (expr)
  `(pprint (macroexpand-1 ',expr)))

;; threading macro
;; http://www.teknoids.net/content/immutable-persistent-data-structures-common-lisp

(defmacro -> (x &optional (form nil form-supplied-p) &rest more)
  (if form-supplied-p
      (if more
          `(-> (-> ,x ,form)
               ,@more)
          (if (listp form)
              `(,(car form)
                ,x
                ,@
                (cdr form))
              (list form x)))
      x))
