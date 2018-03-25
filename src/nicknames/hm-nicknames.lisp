(defun add-nickname (package nickname)
  "Add a nickname to a package. From Rainer Joswig, see
https://stackoverflow.com/questions/28906563/alias-package-names-in-common-lisp."
  (when (stringp package)
    (setf package (find-package package)))
  (check-type package package)
  (check-type nickname string)
  (rename-package package (package-name package)
                  (adjoin nickname (package-nicknames package)
                          :test #'string=)))

(defun hm-nicknames ()
  (add-nickname "fset" "f")
  (add-nickname "cl-csv" "c"))
