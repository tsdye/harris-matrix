(in-package :cl-user)

;;; Ensure ASDF and UIOP are loaded when running
;;; from CLI
(require 'asdf)

;;; Ensure Quicklisp is loaded
#-quicklisp
(let ((quicklisp-init
       (merge-pathnames "quicklisp/setup.lisp"
                        (user-homedir-pathname))))
  (when (probe-file quicklisp-init)
    (load quicklisp-init)))

;;; *SYSTEM-LOCATION* of the project's system
;;; definition. It can not be used as the
;;; *SYSTEM-SEARCH-ROOT* because it will interfere
;;; with the Qlot managed systems which are also
;;; located on this level.
(defparameter *SYSTEM-LOCATION*
  (uiop:subpathname
   (uiop:ensure-absolute-pathname
    (uiop:pathname-directory-pathname
     *load-truename*))
   "../"))

;;; Systems below *SYSTEM-SEARCH-ROOT* are findable
;;; by ASDF. This file is stored in the src/
;;; sub-directory.
(defparameter *SYSTEM-SEARCH-ROOT*
  (uiop:ensure-absolute-pathname
   (uiop:pathname-directory-pathname
    *load-truename*)))

;;; Forget previous customised ASDF settings
(asdf:clear-source-registry)

;;; Tell ASDF where it must search for this branch's
;;; versions of systems.
;;; Note: the :here directive does not work in CCL
;;; because it refers to
;;; *default-directory-defaults* which is not set by
;;; CCL.
(asdf:initialize-source-registry 
 `(:source-registry
   :inherit-configuration  
   (:directory ,*SYSTEM-LOCATION*)
   (:tree ,*SYSTEM-SEARCH-ROOT*)))

