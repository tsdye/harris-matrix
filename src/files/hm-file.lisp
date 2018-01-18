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
    (error "Unable to read ~a.~&" in-file)))

(defun write-default-configuration (path-name)
  "Write the default configuration to path-name."
  (let ((config (make-default-or-empty-configuration (master-table))))
    (with-open-file (stream path-name :direction :output :if-exists :supersede)
      (write-stream config stream))))

(defun write-configuration (cfg file-name)
  "Write configuration, CFG, to the file, FILE-NAME."
  (with-open-file (stream file-name :direction :output :if-exists :supersede)
    (write-stream cfg stream)))

(defun write-Graphviz-style-configuration (config path-name)
  "Write the Graphviz style portion of CONFIG to PATH-NAME."
  (let ((cfg (copy-structure config))
        (file-sections (sections config)))
    (dolist (section file-sections)
      (when (not (Graphviz-section-p section))
        (remove-section cfg section)))
    (write-configuration cfg path-name)))

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
        (error "Error: Unable to find file ~s.~&" file)))
    (when verbose
      (format t "Read ~r initialization file~:p: ~{~a~^, ~}.~&"
              (length file-names) file-names))
    (dolist (file file-names)
      (read-files config (list file)))
    config))

;; output files

(defun write-levels (graph out-file cfg)
  "Write levels of GRAPH to OUT-FILE in the project directory specified in CFG."
  (let ((levels (graph:levels graph))
        (out-file-path (uiop:merge-pathnames* out-file
                                              (get-project-directory cfg))))
    (with-open-file (stream out-file-path :direction :output
                                          :if-exists :overwrite
                                          :if-does-not-exist :create)
      (maphash (lambda (key value)
                 (cl-csv:write-csv-row (list key value)
                                       :stream stream))
               levels))))

(defun write-classifier (classifier-type seq &optional (verbose t))
  "Write the classifier, CLASSIFIER-TYPE, to a file specified in the user's
configuration stored in the archaeological sequence, SEQ. If verbose, indicate
that a file was written."
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
        (cl-csv:write-csv-row
         (list key (if (numberp val) val (string val)))
         :stream stream)))
    (when verbose (format t "Wrote ~a.~&" out-file))))

(defun get-project-directory (cfg)
  "Check if the user's project directory exists, if so, return a path to it. If
not, return a path to the default project directory."
  (let ((user (probe-file (project-directory cfg))))
    (or user
        (uiop:merge-pathnames* "resources/default-project/"
                               (asdf:system-source-directory :hm)))))

(defun input-file-name (cfg content)
  "Return the file path for CONTENT from the user's configuration, CFG, or nil
  if the file does not exist. CONTENT is a string, one of `contexts',
  `observations', `inferences', `periods', `phases', `events', or
  `event-order'."
  (probe-file (uiop:merge-pathnames* (get-option cfg "Input files" content)
                                     (project-directory cfg))))


(defun output-file-name (cfg content)
  "Return the file path for CONTENT from the user's configuration, CFG. CONTENT
  is a string, one of `sequence-dot' or `chronology-dot'."
  (uiop:merge-pathnames* (get-option cfg "Output files" content)
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
                       format output-file gif-file dot-file)))
        (run (format nil "dot -T~a ~a -o~a" format dot-file output-file)))
    (when (and open (fset:contains? can-open format))
      (run (format nil "open ~a" output-file)))))
