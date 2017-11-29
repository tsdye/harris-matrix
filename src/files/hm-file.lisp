;;; hm-file.lisp

;; Copyright (C) Thomas Dye 2017

;; Licensed under the Gnu Public License Version 3 or later

(in-package #:hm)

;; color paths

(defun cet-pathname (name)
  "Returns a path to the CET .csv file NAME."
  (let ((source (asdf:system-source-directory :hm))
        (full-name (uiop:merge-pathnames* "resources/cet/" name)))
    (uiop:merge-pathnames* full-name source)))

(defun svg-pathname (name)
  "Returns a path to the SVG .csv file NAME."
  (let ((source (asdf:system-source-directory :hm))
        (full-name (uiop:merge-pathnames* "resources/svg/" name)))
    (uiop:merge-pathnames* full-name source)))

(defun brewer-pathname (name)
  "Returns a path to the brewer file NAME."
  (let ((source (asdf:system-source-directory :hm))
        (full-name (uiop:merge-pathnames* "resources/brewer/" name)))
    (uiop:merge-pathnames* full-name source)))

;; input file paths

(defun configuration-pathname (name)
  "Returns a path to the system configuration .ini file NAME."
  (let ((source (asdf:system-source-directory :hm))
        (full-name (uiop:merge-pathnames* "resources/configurations/" name)))
    (uiop:merge-pathnames* full-name source)))

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
(defun write-default-configuration (path-name)
  "Write the default configuration to path-name."
  (let ((config (make-default-or-empty-configuration (master-table))))
    (with-open-file (stream path-name :direction :output :if-exists :supersede)
      (write-stream config stream))))

;; API
(defun write-configuration (cfg file-name)
  "Write configuration, CFG, to the file, FILE-NAME."
  (with-open-file (stream file-name :direction :output :if-exists :supersede)
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

(defun read-configuration-from-files (verbose &rest file-names)
  "Reads the initialization files FILE-NAMES and returns a configuration. Errors
out if one or more initialization files were not read. Prints a status message."
  (let ((config (make-default-or-empty-configuration (master-table))))
    (dolist (file file-names)
      (when (null (probe-file file))
        (error "Error: unable to read file: ~s.~&" file)))
    (when verbose
      (format t "Read ~r initialization file~:p: ~{~a~^, ~}.~&"
              (length file-names) file-names))
    (dolist (file file-names)
      (read-files config (list file)))
    config))

;; output files
;; API

(defun write-levels (graph out-file cfg)
  "Write levels of GRAPH to OUT-FILE in the project directory specified in CFG."
  (let ((levels (graph:levels graph))
        (out-file-path (uiop:merge-pathnames* out-file (get-project-directory cfg))))
    (with-open-file (stream out-file-path :direction :output
                                          :if-exists :overwrite
                                          :if-does-not-exist :create)
      (maphash (lambda (key value)
                 (cl-csv:write-csv-row (list key value)
                                       :stream stream))
               levels))))

(defun get-project-directory (cfg &optional verbose)
  "Check if the user's project directory exists, if so, return a path to it. If
not, return a path to the default project directory."
  (let ((user (prob-file (project-directory cfg))))
    (or user
        (uiop:merge-pathnames* "resources/default-project/"
                               (asdf:system-source-directory :hm)))))

(defun input-file-name (cfg content)
  "Return the file path for CONTENT from the user's configuration, CFG, or nil
  if the file does not exist. CONTENT is a string, one of `contexts',
  `observations', `inferences', `periods', `phases', `events', or
  `event-order'."
  (probe-file (uiop:merge-pathnames*
               (get-option cfg "Input files" content)
               (get-option cfg "General configuration" "project-directory"))))


(defun output-file-name (cfg content)
  "Return the file path for CONTENT from the user's configuration, CFG. CONTENT
  is a string, one of `sequence-dot' or `chronology-dot'. CONTENT is a string,
  one of `contexts', `observations', `inferences', `periods', `phases',
  `events', or `event-order'."
  (uiop:merge-pathnames*
   (get-option cfg "Output files" content)
   (get-option cfg "General configuration" "project-directory")))
