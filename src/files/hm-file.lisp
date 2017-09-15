;;; hm-file.lisp

;; Copyright (C) Thomas Dye 2017

;; Licensed under the Gnu Public License Version 3 or later

(in-package #:hm)

(defun read-table (name header &optional (verbose t))
  "Checks that NAME is a file, then attempts to read it as
comma-separated values.  HEADER indicates whether or not the first
line of NAME contains column heads, rather than values.  If VERBOSE,
give notice."
  (if-let (in-file (probe-file (etypecase name
                                 (string (truename name))
                                 (pathname name))))
    (progn
      (when verbose
        (format t "Reading table ~a.~%" in-file))
      (cl-csv:read-csv in-file :skip-first-p header))
    (error "Unable to read ~a.~&" name)))

;; API
(defun write-levels (graph out-file)
  (let ((levels (graph:levels graph)))
    (with-open-file (stream out-file :direction :output
                                     :if-exists :overwrite
                                     :if-does-not-exist :create)
      (maphash (lambda (key value)
                 (cl-csv:write-csv-row (list key value)
                                       :stream stream))
               levels))))

;; API
(defun write-default-configuration (path-name)
  "Write the default configuration to path-name."
  (let ((config (make-default-or-empty-configuration (master-table))))
    (with-open-file (stream path-name :direction :output :if-exists :supersede)
      (write-stream config stream))))

;; API
(defun write-configuration (cfg path-name)
  "Write configuration, CFG, to the file, PATH-NAME."
  (with-open-file (stream path-name :direction :output :if-exists :supersede)
    (write-stream cfg stream)))

;; API
(defun write-Graphviz-style-configuration (config path-name)
  "Write the Graphviz style portion of CONFIG to PATH-NAME."
  (let ((cfg (copy-structure config))
        (file-sections (sections config)))
    (dolist (section file-sections)
      (when (not (Graphviz-section-p section))
        (remove-section cfg section)))
    (write-configuration cfg path-name)))

;; API
(defun write-general-configuration (config path-name)
  "Write the non-Graphviz portion of CONFIG to PATH-NAME."
  (let ((cfg (copy-structure config))
        (file-sections (sections config)))
    (dolist (section file-sections)
      (when (Graphviz-section-p section)
        (remove-section cfg section)))
    (write-configuration cfg path-name)))
