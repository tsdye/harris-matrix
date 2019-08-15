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
        (format t "Reading table from ~a.~a.~&" (pathname-name name)
                (pathname-type name)))
      (cl-csv:read-csv in-file :skip-first-p header))
    (error "Error: Unable to read ~a.~&" in-file)))

(defun read-context-table (cfg &optional (verbose t))
  "Reads the context table specified in the configuration CFG and returns an fset map where the keys are symbols for the context labels and the values are context structures.  Errors out if the input file cannot be read or a context label contains the `=' character."
  (let* ((in-file (input-file-name cfg :contexts))
         (header? (file-header-p cfg :contexts))
         (in-file-path (probe-file in-file)))
    (unless in-file-path (error "Error: Unable to locate ~a.~&" in-file))
    (when verbose (format t "Reading context table from ~a.~&" in-file))
    (let ((context-list
            (cl-csv:read-csv in-file-path
                             :skip-first-p header?
                             :map-fn #'(lambda (row)
                                         (make-context :label (nth 0 row)
                                                       :unit-type (nth 1 row)
                                                       :position (nth 2 row)
                                                       :period (nth 3 row)
                                                       :phase (nth 4 row)
                                                       :url (nth 5 row)))))
          (ret (fset:empty-map))
          (count 0))
      (dolist (rec context-list)
        (incf count)
        (if (search "=" (context-label rec))
            (error "Error: Invalid context label, ~a, in file ~a.~&" (context-label rec) in-file)
            (setf ret (fset:with ret (ensure-symbol (context-label rec)) rec))))
      (when verbose (format t "Read ~W contexts from ~a.~&" count in-file))
      ret)))

(defun write-default-configuration (file-name)
  "Given a string or path name to a file, FILE-NAME, write the default
  configuration to it. Returns an error if the directory part of FILE-NAME
  cannot be found."
  (let* ((cfg (make-default-or-empty-configuration (master-table)))
         (out-dir (directory-namestring file-name))
         (out-file (file-namestring file-name)))
    (unless (directory out-dir) (error "The directory ~s cannot be found.~&"
            out-dir))
    (with-open-file
      (stream (uiop:merge-pathnames* out-file out-dir)
        :direction :output :if-exists :supersede)
      (write-stream cfg stream))))

(defun write-configuration (seq file-name)
  "Given an archaeological sequence, SEQ, and a string naming a file,
FILE-NAME, write the configuration associated with the archaeological sequence
to the file, FILE-NAME, in the project directory specified by the user's
configuration.  FILE-NAME is silently overwritten if it exists."
  (let* ((cfg (archaeological-sequence-configuration seq))
         (out-file (uiop:merge-pathnames* (project-directory cfg) file-name)))
    (with-open-file (stream out-file :direction :output :if-exists :supersede)
      (write-stream cfg stream))))

(defun write-Graphviz-style-configuration (seq file-name)
  "Write the Graphviz style portion of the configuration associated with the
archaeological sequence, SEQ, to the file, FILE-NAME, in the project directory
associated with SEQ."
  (let* ((cfg (copy-structure (archaeological-sequence-configuration seq)))
         (out-file (uiop:merge-pathnames* (project-directory cfg) file-name)))
    (dolist (section (sections cfg))
      (unless (Graphviz-section-p section) (remove-section cfg section)))
    (with-open-file (stream out-file :direction :output :if-exists :supersede)
      (write-stream cfg stream))))

(defun write-general-configuration (seq file-name)
  "Write the non-Graphviz portion of the configuration associated with the
archaeological sequence, SEQ, to the file, FILE-NAME, in the project directory
associated with SEQ."
  (let* ((cfg (copy-structure (archaeological-sequence-configuration seq)))
         (out-file (uiop:merge-pathnames* (project-directory cfg) file-name)))
    (dolist (section (sections cfg))
      (when (Graphviz-section-p section) (remove-section cfg section)))
    (with-open-file (stream out-file :direction :output :if-exists :supersede)
      (write-stream cfg stream))))

(defun read-configuration-from-files (verbose cfg-file cfg-other)
  "Given a boolean value, VERBOSE, and a list of strings or path names,
FILE-NAMES, to user .ini files, read and parse FILE-NAMES and return a
configuration. Errors out if one or more initialization files were not read. If
VERBOSE is non-nil, prints a status message."
  (let ((config (make-default-or-empty-configuration (master-table)))
        (files (push cfg-file cfg-other)))
    (dolist (file files)
      (when (null (probe-file file))
        (error "Error: Unable to find file ~s.~&" (enough-namestring file))))
    (read-files config files)
    (when verbose
      (format t "Read ~r initialization file~:p: ~{~a~^, ~}.~&"
              (length files) files))
    config))

;; output files

(defun write-classifier (classifier-type seq &optional (verbose t))
  "Given a keyword, CLASSIFIER-TYPE, that indicates one of the classifiers
recognized by hm, write the classifier to a file specified in the user's
configuration. If the boolean argument, VERBOSE, is non-nil then write to
standard output an indication that a file was written."
  (let ((classifier (make-classifier classifier-type seq verbose))
        (cfg (archaeological-sequence-configuration seq))
        (out-file (classifier-out-file classifier-type seq verbose)))
    (with-open-file (stream out-file :direction :output
                                     :if-exists :overwrite
                                     :if-does-not-exist :create)
      (when (out-file-header-p classifier-type cfg)
        (cl-csv:write-csv-row (list "node" (string-downcase classifier-type))
                              :stream stream))
      (fset:do-map (key val classifier)
        (if (search "=" (string key))
            (progn
              (let ((keys (ppcre:split "=" (string key))))
                (dolist (once-whole-key keys)
                  (cl-csv:write-csv-row
                   (list once-whole-key (if (numberp val) val (string-downcase val)))
                   :stream stream))))
            (cl-csv:write-csv-row
             (list key (if (numberp val) val (string-downcase val)))
             :stream stream))))
    (when verbose (format t "Wrote classifier ~a to ~a.~&"
       classifier-type (enough-namestring out-file)))))

(defun get-project-directory (cfg)
  "Check if the user's project directory exists, if so, return a path to it. If
not, return a path to the default project directory."
  (let ((user (probe-file (project-directory cfg))))
    (or user
        (uiop:merge-pathnames* "resources/default-project/"
                               (asdf:system-source-directory :hm)))))

(defun input-file-name (cfg content)
  "Return the file path for CONTENT from the user's configuration, CFG, or nil
  if the file does not exist. CONTENT is a keyword, one of :contexts,
  :observations, :inferences, :periods, :phases, :events, or
  :event-order."
  (probe-file (uiop:merge-pathnames*
               (get-option cfg "Input files" (string-downcase (string content)))
               (project-directory cfg))))


(defun output-file-name (cfg content)
  "Return the file path for CONTENT from the user's configuration, CFG. CONTENT
  is a keyword, one of :sequence-dot or :chronology-dot."
  (uiop:merge-pathnames* (get-option cfg "Output files" (string-downcase (string content)))
                         (project-directory cfg)))

(defun unable-to-find-input-files? (cfg)
  "Returns non-nil if input files specified in the configuration CFG
can't be found, nil otherwise."
  (let ((option-list (options cfg "Input files"))
        (missing))
    (dolist (option option-list)
      (let ((file-name (get-option cfg "Input files" option))
            (dir (project-directory cfg)))
        (unless (or (emptyp file-name) (not file-name))
          (let ((in-file (uiop:merge-pathnames* file-name dir)))
            (unless (probe-file in-file)
              (push file-name missing)
              (format t "Warning: The file ~s is missimg from ~s.~&" in-file dir))))))
    missing))

(defun dot-output-format-map ()
  "Return an fset map where the keys are strings specifying valid dot output formats and the values are strings indicating the associated file name extensions."
  (let ((map (-> (fset:empty-map)
                 (fset:with "bmp" "bmp")
                 (fset:with "canon" "dot")
                 (fset:with "gv" "dot")
                 (fset:with "xdot" "dot")
                 (fset:with "xdot1.2" "dot")
                 (fset:with "xdot1.4" "dot")
                 (fset:with "cgimage" "cgi")
                 (fset:with "eps" "eps")
                 (fset:with "exr" "exr")
                 (fset:with "fig" "fig")
                 (fset:with "gd" "gd")
                 (fset:with "gd2" "gd2")
                 (fset:with "gif" "gif")
                 (fset:with "gtk" "gtk")
                 (fset:with "ico" "ico")
                 (fset:with "imap" "map")
                 (fset:with "cmapx" "map")
                 (fset:with "imap_np" "map")
                 (fset:with "cmapx_np" "map")
                 (fset:with "jp2" "jp2")
                 (fset:with "jpg" "jpg")
                 (fset:with "jpeg" "jpeg")
                 (fset:with "jpe" "jpe")
                 (fset:with "json" "json")
                 (fset:with "json0" "json")
                 (fset:with "dot_json" "json")
                 (fset:with "xdot_json" "json")
                 (fset:with "pict" "pct")
                 (fset:with "pct" "pct")
                 (fset:with "pdf" "pdf")
                 (fset:with "pic" "pic")
                 (fset:with "plain" "txt")
                 (fset:with "plain-ext" "txt")
                 (fset:with "png" "png")
                 (fset:with "pov" "pov")
                 (fset:with "ps" "ps")
                 (fset:with "ps2" "ps2")
                 (fset:with "psd" "psd")
                 (fset:with "sgi" "sgi")
                 (fset:with "svg" "svg")
                 (fset:with "svgz" "svg")
                 (fset:with "tga" "tga")
                 (fset:with "tif" "tif")
                 (fset:with "tiff" "tiff")
                 (fset:with "tk" "tk")
                 (fset:with "vml" "vml")
                 (fset:with "vmlz" "vml")
                 (fset:with "vrml" "wrl")
                 (fset:with "wbmp" "wbmp")
                 (fset:with "webp" "webp")
                 (fset:with "xlib" "")
                 (fset:with "x11" ""))))
    map))

(defun image-file-format-p (format)
  "A predicate for a valid dot image file FORMAT."
  (let ((map (dot-output-format-map)))
    (fset:domain-contains? map format)))

(defun image-file-extension (format)
  "Return a string with a file extension for FORMAT."
  (if (image-file-format-p format)
      (let ((map (dot-output-format-map)))
        (fset:lookup map format))
      (error "Error: ~s is not a valid Graphviz dot image file format.~&" format)))

(defun delete-graphics-file (cfg graph format)
  "Delete a graphics file in the project specified by the user's configuration,
CFG. GRAPH is one of :chronology :sequence, and FORMAT is a string that
specifies an output graphics file format recognized by Graphviz dot."
  (let ((ext (image-file-extension format))
        (dot-file (namestring
                   (truename
                    (output-file-name
                     cfg (case graph (:sequence :sequence-dot)
                               (:chronology :chronology-dot)))))))
    (uiop:delete-file-if-exists (ppcre:regex-replace "[.]dot" dot-file
                                                     (format nil ".~a" ext)))))

(defun make-graphics-file (cfg graph format &key open (verbose t))
  "Run the dot program to make a graphics file of type, FORMAT, based on
information in the user's configuration, CFG, for the specified GRAPH type.
GRAPH is one of :sequence, :chronology. FORMAT is any output format recognized
by the dot program."
  (unless (image-file-format-p format)
    (error "Error: ~s is not a valic Graphviz dot image file format.~&" format))
  (unless (fset:contains? (fset:set :sequence :chronology) graph)
    (error "Error: ~a is not a recognized graph type.~&" graph))
  (unless (typep cfg 'config)
    (error "Error: ~a is not a user configuration file.~&" cfg))
  (let* ((ext (image-file-extension format))
         (dot-file (namestring
                    (truename
                     (output-file-name
                      cfg (case graph (:sequence :sequence-dot)
                                (:chronology :chronology-dot))))))
         (can-open
           (fset:set "jpg" "jpe" "jp2" "jpeg" "png" "pdf" "tif" "tiff" "gif" "svg"))
         (two-outputs (fset:set "imap" "cmapx" "imap_np" "cmapx_np"))
         (output-file (ppcre:regex-replace "[.]dot" (copy-seq dot-file)
                                           (format nil ".~a" ext))))
    (when verbose (format t "Creating ~a.~&" output-file))
    (if (fset:contains? two-outputs format)
        (let ((gif-file (ppcre:regex-replace "[.]dot" (copy-seq dot-file) ".gif")))
          (run (format nil "dot -T~a -o~a -Tgif -o~a ~a"
                       format output
                       -file gif-file dot-file)))
        (run (format nil "dot -T~a ~a -o~a" format dot-file output-file)))
    (when (and open (fset:contains? can-open format))
      (run (format nil "~a ~a" open output-file)))))

(defun write-cycles-to-file (cfg cycles &key (verbose t))
  "Given the information in a configuration, CFG, and a list of nodes, CYCLES,
write a comma separated value file with the feedback vertex set. When VERBOSE,
write a confirmation that the file was written."
  (let* ((file-name (get-option cfg "Output files" "feedback-vertex-set"))
         (out-file (uiop:merge-pathnames* (project-directory cfg) file-name)))
    (with-open-file (stream out-file :direction :output
                                     :if-exists :overwrite
                                     :if-does-not-exist :create)
      (cl-csv:write-csv cycles :stream stream))
    (when verbose (format t "Wrote feedback vertex set to ~a.~&" (enough-namestring out-file)))))
