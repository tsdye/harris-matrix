;;; hm.lisp

;; Copyright (C) Thomas Dye 2015

;; Licensed under the Gnu Public License Version 3 or later

(in-package #:hm)

(require "graph-dot")
(require "graph-matrix")
(require "cl-csv")
(require "do-urlencode")
(require "fset")
;; The inferior-shell package should enable hm.lisp to call a script
;; that compiles and displays the dot file output.
;; (require "inferior-shell")

;; macro
;; http://www.teknoids.net/content/immutable-persistent-data-structures-common-lisp

(defmacro -> (x &optional (form nil form-supplied-p) &rest more)
  (if form-supplied-p
      (if more
          `(-> (-> ,x ,form) ,@more)
          (if (listp form)
              `(,(car form) ,x ,@(cdr form))
              (list form x)))
      x))

;; constants

(defconstant infinity graph-matrix:infinity)

;; declare global variables that will be set by the configuration file

(defparameter *output-file-sequence* nil
  "Output file path for the sequence diagram dot file.")
(defparameter *output-file-chronology* nil
  "Output file path for the chronology diagram dot file.")
(defparameter *context-table-name* nil
  "Input file path for the context table.")
(defparameter *observation-table-name* nil
  "Input file path for the observation table.")
(defparameter *inference-table-name* nil
  "Optional input file path for the inference table.")
(defparameter *period-table-name* nil
  "Optional input file path for the period table.")
(defparameter *phase-table-name* nil
  "Optional input file path for the phase table.")
(defparameter *radiocarbon-table-name* nil
  "Optional input file path for the radiocarbon table.")
(defparameter *date-order-table-name* nil
  "Optional input file path for the date order table.")
(defparameter *context-table-header* nil
  "Switch for a header line in the context table, nil for no header
  line and non-nil for a header line.")
(defparameter *observation-table-header* nil
  "Switch for a header line in the observation table, nil for no header
  line and non-nil for a header line.")
(defparameter *inference-table-header* nil
  "Switch for a header line in the inference table, nil for no header
  line and non-nil for a header line.")
(defparameter *period-table-header* nil
  "Switch for a header line in the period table, nil for no header
  line and non-nil for a header line.")
(defparameter *phase-table-header* nil
  "Switch for a header line in the phase table, nil for no header
  line and non-nil for a header line.")
(defparameter *radiocarbon-table-header* nil
  "Switch for a header line in the radiocarbon table, nil for no header
  line and non-nil for a header line.")
(defparameter *date-order-table-header* nil
  "Switch for a header line in the date-order table, nil for no header
  line and non-nil for a header line.")
(defparameter *symbolize-unit-type* nil
  "Switch to distinguish interfaces and deposits, nil for no
  distinction and non-nil to distinguish.")
(defparameter *create-chronology-graph* nil
  "Switch for a chronology graph, nil for no graph and non-nil to create a graph.")
(defparameter *node-fill-by* nil
  "Fill nodes of a sequence graph according to some procedure; nil for
  no procedure, or one of 'levels', 'reachable', 'periods', 'phases',
  'connected', or 'distance'.")
(defparameter *node-shape-by* nil
  "Shape nodes of a sequence graph according to some procedure; nil
  for no procedure, or one of 'levels' 'reachable', 'periods',
  'phases', 'connected', or 'distance'. ")
(defparameter *reachable-from* nil
  "The label of the start node for calculating reachability.")
(defparameter *reachable-limit* nil
  "A number that determines the scope of the reachability calculation;
  < 0 for all reachable nodes, 0 for the start node only, 1 for
  adjacent nodes, etc.")
(defparameter *url-include* nil
  "Switch for URL information for nodes and edges; 'nil' for no URL
  information, non-nil otherwise.")
(defparameter *url-default* nil
  "A default URL to use for nodes and edges if specific URL
  information is absent.")
(defparameter *legend* nil
  "A switch for creating a rudimentary legend; non-nil to include a
  legend, nil otherwise.")
(defparameter *assume-correlations-true* nil
  "A switch to toggle between a directed graph based solely on
  observations and one based on observations and inferences; nil for a
  directed graph based on observations and non-nil for a directed
  graph based on observations and inferences.")
(defparameter *use-fast-matrix* t
  "Toggle fast matrix routines.")

;; The following variables should be set from a separate style file

;; Special variables for style
(defparameter *style-edge-sequence* nil
  "A valid GraphViz dot style attribute for edges.")
(defparameter *style-node-sequence* nil
  "A valid GraphViz dot style attribute for nodes.")
(defparameter *style-node-chronology* nil
  "A valid GraphViz dot style attribute for nodes.")


;; Graphviz colors

(defparameter *color-scheme-sequence* nil
  "Either 'nil' for no color scheme, or the name of a Brewer color
  scheme used by GraphViz dot.")
(defparameter *color-scheme-chronology* nil
  "Either 'nil' for no color scheme, or the name of a Brewer color
  scheme used by GraphViz dot.")
(defparameter *color-space* nil
  "Choose the color space for color names; 'nil' for the GraphViz dot
  default, or one of 'x11' or 'svg'.  Used by sequence and chronology graphs.")
(defparameter *color-label-dark* nil
  "The dark color for labels used with nodes having a light fill
  color; either a color name from the active color space, or an integer
  index into a Brewer color scheme.  Used by sequence and chronology graphs.")
(defparameter *color-label-light* nil
  "The light color for labels used with nodes having a dark fill
  color; either a color name from the active color space, or an integer
  index into a Brewer color scheme.  Used by sequence and chronology graphs.")
(defparameter *color-reachable* nil
  "The fill color for reachable nodes; either a color name from the
  active color space, or an integer index into a Brewer color scheme.")
(defparameter *color-not-reachable* nil
  "The fill color for unreachable nodes; either a color name from the
  active color space, or an integer index into a Brewer color scheme.")
(defparameter *color-origin* nil
  "The fill color for the origin node; either a color name from the
  active color space, or an integer index into a Brewer color scheme.")
(defparameter *color-adjacent* nil
  "The fill color for nodes adjacent to the origin node; either a
  color name from the active color space, or an integer index into a
  Brewer color scheme.")
(defparameter *color-edge-sequence* nil
  "The color of edges. Either a color name from the active color space
  or an integer index into a Brewer color scheme.")
(defparameter *color-fill-graph-sequence* nil
  "A color to fill the background of a sequence graph.  One of
  'transparent', a color name from the active color space, or an
  integer index into a Brewer color scheme.")
(defparameter *color-node-sequence* nil
  "The color of the node outline in a sequence graph.  Either a color
  name from the active color space or an integer index into a Brewer
  color scheme.")
(defparameter *color-fill-node-sequence* nil
  "The color used to fill nodes of a sequence graph.  Either a color
  name from the active color space or an integer index into a Brewer
  color scheme.")
(defparameter *color-edge-chronology* nil
  "The color of edges in a chronology graph. Either a color name from
  the active color space or an integer index into a Brewer color
  scheme.")
(defparameter *color-node-chronology* nil
  "The color of the node outline in a chronology graph.  Either a
  color name from the active color space or an integer index into a
  Brewer color scheme.")
(defparameter *color-fill-node-chronology* nil
  "The color used to fill nodes in a chronology graph.  Either a color
  name from the active color space or an integer index into a Brewer
  color scheme.")
(defparameter *color-fill-node-deposit* nil
  "A color used to fill nodes of depositional contexts in a sequence
  graph.  Either a color name from the active color space or an
  integer index into a Brewer color scheme.")
(defparameter *color-fill-node-interface* nil
  "A color used to fill nodes of interfacial contexts in a sequence
  graph.  Either a color name from the active color space or an
  integer index into a Brewer color scheme.")
(defparameter *color-fill-node-legend* nil
  "A color used to fill nodes of a legend.  Either a color name from
  the active color space or an integer index into a Brewer color
  scheme.")
(defparameter *color-fill-graph-chronology* nil
  "A color to fill the background of a chronology graph.  One of
  'transparent', a color name from the active color space, or an
  integer index into a Brewer color scheme.")

;; Graphviz shapes

(defparameter *shape-reachable* nil
  "The shape for reachable nodes; either a Graphviz dot node shape
  name, or an integer index into hm-node-shapes.")
(defparameter *shape-not-reachable* nil
  "The shape for unreachable nodes; either a Graphviz dot node shape
  name, or an integer index into hm-node-shapes.")
(defparameter *shape-origin* nil
  "The shape for the origin node; either a Graphviz dot node shape
  name, or an integer index into hm-node-shapes.")
(defparameter *shape-adjacent* nil
  "The shape for adjacent nodes; either a Graphviz dot node shape
  name, or an integer index into hm-node-shapes.")
(defparameter *shape-phase* nil
  "A valid GraphViz dot node shape for chronological phase nodes.")
(defparameter *shape-date* nil
  "A valid GraphViz dot node shape for dates in a chronological graph.")

;; Graphviz graph attributes

(defparameter *graph-title-sequence* nil
  "An optional title for the sequence graph.")
(defparameter *graph-labelloc-sequence* nil
  "The location of the sequence graph title, either non-nil for
  centered or nil for not centered.")
(defparameter *graph-style-sequence* nil
  "A valid argument for the GraphViz dot style attribute for a
  sequence graph.")
(defparameter *graph-size-sequence* nil
  "The maximum size of the sequence graph in inches; either one value
  that serves as both width and height, or two values separated by a
  comma.  Leave this nil for the GraphViz dot default.")
(defparameter *graph-ratio-sequence* nil
  "A valid argument for the GraphViz dot ratio attribute for a
  sequence graph; typically a numeric value indicating the aspect
  ratio, but also various strings that trigger complex responses.")
(defparameter *graph-page-sequence* nil
  "The maximum sequence graph page size in inches; either one
  value that serves as both width and height, or two values separated
  by a comma.  Leave this nil for the GraphViz dot default.")
(defparameter *graph-dpi-sequence* nil
  "Specify the expected number of pixels per inch on the sequence
  graph display device.  Leave this nil for the GraphViz dot
  default.")
(defparameter *graph-margin-sequence* nil
  "The x and y margins of the sequence graph in inches; either one
  value that serves as both x and y, or two values separated by a
  comma.  Leave this nil for the GraphViz dot default.")
(defparameter *graph-title-chronology* nil
  "An optional title for the chronology graph.")
(defparameter *graph-labelloc-chronology* nil
  "The location of the chronology graph title, either non-nil for
  centered or nil for not centered.")
(defparameter *graph-style-chronology* nil
  "A valid argument for the GraphViz dot style attribute for a
  chronology graph.")
(defparameter *graph-size-chronology* nil
  "The maximum size of the chronology graph in inches; either one
  value that serves as both width and height, or two values separated
  by a comma.  Leave this nil for the GraphViz dot default.")
(defparameter *graph-ratio-chronology* nil
  "A valid argument for the GraphViz dot ratio attribute for a
  chronology graph; typically a numeric value indicating the aspect
  ratio, but also various strings that trigger complex responses.")
(defparameter *graph-page-chronology* nil
  "The maximum chronology graph page size in inches; either one value that
  serves as both width and height, or two values separated by a
  comma.  Leave this nil for the GraphViz dot default.")
(defparameter *graph-dpi-chronology* nil
  "Specify the expected number of pixels per inch on the chronology
  graph display device.  Leave this nil for the GraphViz dot
  default.")
(defparameter *graph-margin-chronology* nil
  "The x and y margins of the chronology graph in inches; either one
  value that serves as both x and y, or two values separated by a
  comma.  Leave this nil for the GraphViz dot default.")

;; Graphviz font attributes

(defparameter *font-color-label-break* nil
  "Switch from dark to light label color; 'nil' for no switch, or a
  number corresponding to the highest level node that is labeled with
  a dark color--higher level nodes will be light-colored.")
(defparameter *font-name-graph-sequence* nil
  "The font used to render text associated with the graph (rather than
  the nodes and edges). The possibilities here are device dependent.
  The values 'times' and 'helvetica' select Roman and sans-serif
  fonts, respectively, on most platforms.")
(defparameter *font-size-graph-sequence* nil
  "Font size in points.")
(defparameter *font-color-graph-sequence* nil
  "The color of text associated with the graph (rather than nodes and
  edges). Either a color name from the active color space or an
  integer index into a Brewer color scheme.")
(defparameter *font-name-edge-sequence* nil
  "The font used to render edge labels. The possibilities here are
  device dependent.  The values 'times' and 'helvetica' select Roman
  and sans-serif fonts, respectively, on most platforms.")
(defparameter *font-size-edge-sequence* nil
  "Font size in points of edge labels.")
(defparameter *font-color-edge-sequence* nil
  "The color of text for edge labels. Either a color name from the
  active color space or an integer index into a Brewer color scheme.")
(defparameter *font-name-node-sequence* nil
  "The font used to render node labels. The possibilities here are
  device dependent.  The values 'times' and 'helvetica' select Roman
  and sans-serif fonts, respectively, on most platforms.")
(defparameter *font-size-node-sequence* nil
  "Font size in points of node labels.")
(defparameter *font-color-node-sequence* nil
  "The color used to render node labels.  Either a color name from the
  active color space or an integer index into a Brewer color scheme.")
(defparameter *font-name-graph-chronology* nil
  "The font used to render text associated with the graph (rather than
  the nodes and edges). The possibilities here are device dependent.
  The values 'times' and 'helvetica' select Roman and sans-serif
  fonts, respectively, on most platforms.")
(defparameter *font-size-graph-chronology* nil
  "Font size in points.")
(defparameter *font-color-graph-chronology* nil
  "The color of text associated with the graph (rather than nodes and
  edges). Either a color name from the active color space or an
  integer index into a Brewer color scheme.")
(defparameter *font-name-edge-chronology* nil
  "The font used to render edge labels. The possibilities here are
  device dependent.  The values 'times' and 'helvetica' select Roman
  and sans-serif fonts, respectively, on most platforms.")
(defparameter *font-size-edge-chronology* nil
  "Font size in points of edge labels.")
(defparameter *font-color-edge-chronology* nil
  "The color of text for edge labels. Either a color name from the
  active color space or an integer index into a Brewer color scheme.")
(defparameter *font-name-node-chronology* nil
  "The font used to render node labels. The possibilities here are
  device dependent.  The values 'times' and 'helvetica' select Roman
  and sans-serif fonts, respectively, on most platforms.")
(defparameter *font-size-node-chronology* nil
  "Font size in points of node labels.")
(defparameter *font-color-node-chronology* nil
  "The color used to render node labels.  Either a color name from the
  active color space or an integer index into a Brewer color scheme.")

;; Graphviz node shape attributes
(defparameter *shape-node-legend* nil
  "A valid GraphViz dot node shape for node entries in the legend.")
(defparameter *shape-node-interface* nil
  "A valid GraphViz dot node shape for interfacial contexts.")
(defparameter *shape-node-deposit* nil
  "A valid GraphViz dot node shape for depositional contexts.")

;; Graphviz edge attributes
(defparameter *graph-splines-sequence* nil
  "Controls how, and if, edges are drawn on the sequence graph; one of
  'none' for no edges, 'line' or 'false' for straight line edges,
  'polyline' for straight line edges that avoid nodes, 'curved' for
  curved edges, 'ortho' for orthogonal edges, and 'spline' or 'true'
  for edges that avoid nodes.")
(defparameter *graph-splines-chronology* nil
  "Controls how, and if, edges are drawn on the chronology graph; one
  of 'none' for no edges, 'line' or 'false' for straight line edges,
  'polyline' for straight line edges that avoid nodes, 'curved' for
  curved edges, 'ortho' for orthogonal edges, and 'spline' or 'true'
  for edges that avoid nodes.")
(defparameter *edge-arrowhead-sequence* nil
  "A valid GraphViz dot arrowType attribute.")
(defparameter *edge-arrowhead-chronology* nil
  "A valid GraphViz dot arrowType attribute.")
(defparameter *edge-date-chronology* nil
  "A valid GraphViz dot style attribute for an edge that connects to a date.")
(defparameter *edge-abutting-chronology* nil
  "A valid GraphViz dot style attribute for an edge that connects abutting phases.")
(defparameter *edge-separated-chronology* nil
  "A valid GraphViz dot style attribute for an edge that connects
  separated phases.")

;; general function definitions

(defun initialize-special-variables ()
  (setf *output-file-sequence* nil
        *output-file-chronology* nil
        *context-table-name* nil
        *observation-table-name* nil
        *inference-table-name* nil
        *period-table-name* nil
        *phase-table-name* nil
        *radiocarbon-table-name* nil
        *date-order-table-name* nil
        *context-table-header* nil
        *observation-table-header* nil
        *inference-table-header* nil
        *period-table-header* nil
        *phase-table-header* nil
        *radiocarbon-table-header* nil
        *date-order-table-header* nil
        *symbolize-unit-type* nil
        *create-chronology-graph* nil
        *node-fill-by* nil
        *node-shape-by* nil
        *reachable-from* nil
        *reachable-limit* nil
        *url-include* nil
        *url-default* nil
        *legend* nil
        *assume-correlations-true* nil
        *use-fast-matrix* t
        *style-edge-sequence* nil
        *style-node-sequence* nil
        *style-node-chronology* nil
        *color-scheme-sequence* nil
        *color-scheme-chronology* nil
        *color-space* nil
        *color-label-dark* nil
        *color-label-light* nil
        *color-reachable* nil
        *color-not-reachable* nil
        *color-origin* nil
        *color-adjacent* nil
        *color-edge-sequence* nil
        *color-fill-graph-sequence* nil
        *color-node-sequence* nil
        *color-fill-node-sequence* nil
        *color-edge-chronology* nil
        *color-node-chronology* nil
        *color-fill-node-chronology* nil
        *color-fill-node-deposit* nil
        *color-fill-node-interface* nil
        *color-fill-node-legend* nil
        *color-fill-graph-chronology* nil
        *shape-reachable* nil
        *shape-not-reachable* nil
        *shape-origin* nil
        *shape-adjacent* nil
        *shape-phase* nil
        *shape-date* nil
        *graph-title-sequence* nil
        *graph-labelloc-sequence* nil
        *graph-style-sequence* nil
        *graph-size-sequence* nil
        *graph-ratio-sequence* nil
        *graph-page-sequence* nil
        *graph-dpi-sequence* nil
        *graph-margin-sequence* nil
        *graph-title-chronology* nil
        *graph-labelloc-chronology* nil
        *graph-style-chronology* nil
        *graph-size-chronology* nil
        *graph-ratio-chronology* nil
        *graph-page-chronology* nil
        *graph-dpi-chronology* nil
        *graph-margin-chronology* nil
        *font-color-label-break* nil
        *font-name-graph-sequence* nil
        *font-size-graph-sequence* nil
        *font-color-graph-sequence* nil
        *font-name-edge-sequence* nil
        *font-size-edge-sequence* nil
        *font-color-edge-sequence* nil
        *font-name-node-sequence* nil
        *font-size-node-sequence* nil
        *font-color-node-sequence* nil
        *font-name-graph-chronology* nil
        *font-size-graph-chronology* nil
        *font-color-graph-chronology* nil
        *font-name-edge-chronology* nil
        *font-size-edge-chronology* nil
        *font-color-edge-chronology* nil
        *font-name-node-chronology* nil
        *font-size-node-chronology* nil
        *font-color-node-chronology* nil
        *shape-node-legend* nil
        *shape-node-interface* nil
        *shape-node-deposit* nil
        *graph-splines-sequence* nil
        *graph-splines-chronology* nil
        *edge-arrowhead-sequence* nil
        *edge-arrowhead-chronology* nil
        *edge-date-chronology* nil
        *edge-abutting-chronology* nil
        *edge-separated-chronology* nil))

(defun pair-with (elem list)
  (mapcar (lambda (a) (list elem a)) list))

(defun unique-pairs (list)
  (mapcon (lambda (rest) (pair-with (first rest) (rest rest)))
          (remove-duplicates list)))

(defun new-matrix (fast)
  "Makes a matrix instance.  If FAST is t, then uses fast matrix
routines.  If FAST is nil, then uses CL matrix routines."
  (if fast (make-instance 'graph-matrix:fast-matrix)
      (make-instance 'graph-matrix:matrix)))

(defun new-symbol (string &optional format)
  "Makes a symbol out of STRING, optionally passing the string through
a FORMAT specification."
  (when (or (stringp string) (and format (numberp string)))
    (read-from-string (if format (format nil format string) string)
                      nil nil :preserve-whitespace t)))

(defun url-decode (url)
  "Decodes symbol URL and returns a valid url as a string."
  (do-urlencode:urldecode (string url)))

(defun graph-element-control (default &optional (func #'identity) switch map)
  "A template that connects a MAP with a graph element, either
a node or an arc.  SWITCH determines whether to use a value from
MAP or DEFAULT.  FUNC is an optional function with one
argument that is used to process either the MAP result or the
DEFAULT value.  The default FUNC function, IDENTITY, simply
passes through the value passed to it."
  (if (and switch map)
      (lambda (x) (format-attribute (funcall func (fset:@ map x))))
      (constantly-format (funcall func default))))

;; Label function definitions.  Not all these are used by hm.lisp;
;; they are for future improvements.

(defun make-label (id &optional type)
  (case type
    (html (let* ((s (string-downcase (string id)))
                 (hyphen-pos (position #\- s :test #'equal))
                 (greek (subseq s 0 hyphen-pos))
                 (post (subseq s (+ 1 hyphen-pos))))
            (format nil "<&~a;<sub>~a</sub>>" greek post)))
    (uppercase (substitute #\SPACE #\- (string-upcase (write-to-string id))))
    (lowercase (substitute #\SPACE #\- (string-downcase (write-to-string id))))
    (titlecase (substitute #\SPACE #\- (string-capitalize (write-to-string id))))
    (otherwise "\N")))

(defun chronology-graph-html-label (id)
  "Convert the symbol ID into an html label for Graphviz dot.  This
function is designed specifically for use with chronology graphs."
  (let* ((s (string-downcase (string id)))
         (hyphen-pos (position #\- s :test #'equal))
         (greek (subseq s 0 hyphen-pos))
         (post (subseq s (+ 1 hyphen-pos))))
    (format nil "<&~a;<sub>~a</sub>>" greek post)))

(defun uppercase-label (id)
  "Convert the symbol ID into an uppercase label string for Graphviz
  dot, changing hyphens in ID to spaces."
  (substitute #\SPACE #\- (write-to-string id :case :upcase)))

(defun lowercase-label (id)
  "Convert the symbol ID into an ercase label string for Graphviz
  dot, changing hyphens in ID to spaces."
  (substitute #\SPACE #\- (write-to-string id :case :downcase)))

(defun titlecase-label (id)
  "Convert the symbol ID into an titlecase label string for Graphviz
  dot, changing hyphens in ID to spaces."
  (substitute #\SPACE #\- (write-to-string id :case :capitalize)))

;; io function definitions

(defun hm-read-cnf-file (name)
  "Read csv file specified by NAME and set most global variables to
symbols, except strings that appear in the graph and must preserve
case.  URL's are encoded so they can be represented as symbols.
Return t if successful, nil otherwise."
  (alexandria:when-let (in-file (probe-file name))
    (format t "Reading configuration file ~a~%" in-file)
    (cl-csv:do-csv (row in-file)
      (let ((special-var (read-from-string (first row))))
        (setf (symbol-value special-var)
              (case special-var
                (*graph-title-sequence* (second row))
                (*graph-title-chronology* (second row))
                ;; should probably make this a symbol
                (*url-default* (do-urlencode:urlencode (second row)))
                (otherwise (new-symbol (second row)))))
        (setf (get special-var 'dot-label) (string-capitalize (second row)))
        (setf (get special-var 'html-label) (second row))))
    t))

(defun hm-read-table (name header)
  "Checks that NAME is a file, then attempts to read it as
comma-separated values.  HEADER indicates whether or not the first
line of NAME contains column heads, rather than values."
  (alexandria:when-let (in-file (probe-file (string name)))
    (format t "Reading table ~a~%" in-file)
    (cl-csv:read-csv in-file :skip-first-p header)))

;; filter function definitions


(defun color-filter (col)
  "Returns a valid Graphviz dot color designator. The result is either
an integer or a color name appended to a color space.  COL can either
be an integer, in which case Brewer colors are assumed, or a symbol
whose string value is a color name.  Returns 0 if COL is nil or a string."
  (cond
    ((integerp col) col)
    ((eq col 'transparent) "transparent")
    ((and (symbolp col) col) (format nil "~(/~a/~a~)" *color-space* col))
    (t 0)))

(defun format-attribute (att)
  "A convenience function to transform a Lisp symbol into a GraphViz
dot attribute."
  (format nil "~(~s~)" (if att att "")))

(defun constantly-format (x)
  "A convenience function for situations where Lisp requires a
function to express a constant."
  (constantly (format-attribute x)))

(defun label-color (color)
  (if (and *font-color-label-break* (integerp color))
      (if (<= *font-color-label-break* color)
          (color-filter *color-label-light*) (color-filter *color-label-dark*))
      (color-filter *color-label-dark*)))

;; graph matrix creation functions

(defun create-adjacency-matrix (graph matrix)
  "Given a GRAPH and a possibly empty MATRIX, returns the adjacency
matrix of GRAPH."
  (if matrix matrix
      (graph-matrix:to-adjacency-matrix graph (new-matrix *use-fast-matrix*))))

(defun create-reachability-matrix (graph matrix &optional limit)
  "Given a GRAPH and a possibly empty MATRIX, returns the reachability
matrix of GRAPH."
  (if matrix matrix
      (if (and limit (< limit 2))
          (graph-matrix:to-reachability-matrix
           graph (new-matrix *use-fast-matrix*))
          (graph-matrix:to-reachability-matrix
           graph (new-matrix *use-fast-matrix*) :limit limit))))

(defun create-distance-matrix (graph matrix)
  "Given a GRAPH and a possibly empty MATRIX, returns the distance
matrix of GRAPH."
  (if matrix matrix
      (graph-matrix:to-distance-matrix graph (new-matrix *use-fast-matrix*))))


(defun get-node-urls-from (table inferences)
  "Reads URL's from TABLE.  Returns a map where the keys are
node labels and the values are symbols for URL's."
  (let ((ret (fset:empty-map)))
    (dolist (node table)
      (setf ret (fset:with ret (new-symbol (first node))
                           (if (string= (sixth node) "")
                               (if *url-default* *url-default* "")
                               (sixth node)))))
    (when *assume-correlations-true*
      (dolist (part inferences)
        (setf ret (fset:with ret (read-from-string
                                  (format nil "~a=~a" (first part) (second part)))
                             (if (string= (third part) "")
                                 (if *url-default*  *url-default* "")
                                 (third part))))))
    ret))

(defun get-arc-urls-from (table)
  "Reads URL's from TABLE.  Returns a map where the keys are
arcs and the values are URL's."
  (let ((ret (fset:empty-map)))
    (dolist (arc table)
      (fset:with ret (list (new-symbol (first arc))
                           (new-symbol (second arc)))
                 (if (string= (third arc) "")
                     (if *url-default* *url-default* "")
                     (third arc))))
    ret))

;; graph structure functions

(defun set-same-ranks (table)
  "Use the values in TABLE to return a list of graph:rank structures
where the indicated nodes are to appear on the same rank of the graph
picture."
  (let ((ranks))
    (mapcar #'(lambda (x)
                (push (graph-dot::make-rank
                       :value "same"
                       :node-list (list (format nil "~s" (new-symbol (first x)))
                                        (format nil "~s" (new-symbol (second x)))))
                      ranks))
            table)
    ranks))

(defun set-other-ranks (table)
  "Use the values in TABLE to return a list of graph:rank structures
where the indicated nodes either appear at the top or the bottom of
the graph picture."
  (let ((ranks))
    (mapcar #'(lambda (x)
                (when (or (eq (new-symbol (third x)) 'basal)
                          (eq (new-symbol (third x)) 'surface))
                  (push (graph-dot::make-rank
                         :value
                         (cond
                           ((eq (new-symbol (third x)) 'basal) "sink")
                           ((eq (new-symbol (third x)) 'surface) "source"))
                         :node-list
                         (list (format nil "~s" (new-symbol (first x)))))
                        ranks)))
            table)
    ranks))


;; Classifiers

(defclass classifier ()
  ((map :accessor classification
        :initform (fset:empty-map))
   (lookup :reader node-shapes
           :initform (-> (fset:empty-map)
                         (fset:with 0  "box")
                         (fset:with 1  "polygon")
                         (fset:with 2  "ellipse")
                         (fset:with 3  "oval")
                         (fset:with 4  "circle")
                         (fset:with 5  "point")
                         (fset:with 6  "egg")
                         (fset:with 7  "triangle")
                         (fset:with 8  "diamond")
                         (fset:with 9  "trapezium")
                         (fset:with 10  "parallelogram")
                         (fset:with 11  "house")
                         (fset:with 12  "pentagon")
                         (fset:with 13  "hexagon")
                         (fset:with 14  "septagon")
                         (fset:with 15  "octagon")
                         (fset:with 16  "doublecircle")
                         (fset:with 17  "doubleoctagon")
                         (fset:with 18  "tripleoctagon")
                         (fset:with 19  "invtriangle")
                         (fset:with 20  "invtrapezium")
                         (fset:with 21  "invhouse")
                         (fset:with 22  "Mdiamond")
                         (fset:with 23  "Msquare")
                         (fset:with 24  "Mcircle")
                         (fset:with 25  "square")
                         (fset:with 26  "star")
                         (fset:with 27  "underline")
                         (fset:with 28  "note")
                         (fset:with 29  "tab")
                         (fset:with 30  "folder")
                         (fset:with 31  "box3d")
                         (fset:with 32  "component")
                         (fset:with 33  "cds")
                         (fset:with 34  "signature")
                         (fset:with 35  "rpromoter")
                         (fset:with 36  "rarrow")
                         (fset:with 37  "larrow")
                         (fset:with 38  "lpromoter")))))

(defmethod shape-by ((obj classifier) graph)
  (let ((ret (fset:empty-map)))
    (fset:do-map (key value (classification obj))
      (setf ret (fset:with ret key (fset:@ (node-shapes obj) value))))
    ret))

(defmethod fill-by ((obj classifier) graph)
  (classification obj))

(defclass levels (classifier)
  ())

(defmethod classify ((obj levels) graph)
  "Set map according to the levels of GRAPH."
  (let ((lev (graph:levels graph)))
    (alexandria:maphash-keys
     (lambda (key) (setf (classification obj)
                    (fset:with (classification obj) key
                               (gethash key lev))))
     lev)))

;; Tabular class and methods for periods and phases

(defclass tabular (classifier)
  ((table :accessor table
          :initform nil)
   (contexts :accessor contexts
             :initform nil)))

(defmethod classify ((obj tabular) graph)
  (let ((ht (fset:empty-map)))
    (mapcar
     #'(lambda (x)
         (setf ht (fset:with ht (new-symbol (first x)) (new-symbol (third x)))))
     (table obj))
    (mapcar
     #'(lambda (x)
         (setf (classification obj)
               (fset:with (classification obj) (new-symbol (first x))
                          (fset:@ ht (new-symbol (fourth x))))))
     (contexts obj))))

(defmethod legend-shape ((obj tabular) &key default)
  (let ((ret (fset:empty-map)))
    (mapcar #'(lambda (x)
                (fset:with ret (new-symbol (second x))
                           (if default *shape-node-legend*
                               (fset:@ (node-shapes obj) (new-symbol (third x))))))
            (table obj))
    ret))

(defmethod legend-fill ((obj tabular) &key default)
  (let ((ret (fset:empty-map)))
    (mapcar #'(lambda (x)
                (fset:with ret (new-symbol (second x))
                           (if default *color-fill-node-legend*
                               (color-filter (new-symbol (third x))))))
            (table obj))
    ret))

(defmethod legend-urls ((obj tabular))
  (let ((ret (fset:empty-map)))
    (mapcar #'(lambda (x)
                (fset:with ret (new-symbol (second x)) *url-default*))
            (table obj))
    ret))

(defmethod add-legend-nodes ((obj tabular) graph)
  (mapcar #'(lambda (x)
              (graph:add-node graph (new-symbol (second x))))
          (table obj)))

(defclass periods (tabular) ())
(defclass phases (tabular) ())

(defclass graph-matrix (classifier)
  ((matrix :accessor matrix
           :initform nil)))

(defclass reachable (graph-matrix)
  ())

(defmethod classify ((obj reachable) graph)
  "Sets unreachable nodes to 'not-reachable, the origin node
to 'origin, and reachable nodes to 'reachable"
  (let ((reachable-list
         (cond
           ((< *reachable-limit* 0)
            (graph-matrix:reachable-from
             graph (matrix obj) *reachable-from*))
           ((eq *reachable-limit* 0) ())
           (t (graph-matrix:reachable-from
               graph (matrix obj) *reachable-from*)))))
    (dolist (node reachable-list)
      (setf (classification obj) (fset:with (classification obj) node 'reachable)))
    (dolist (node (set-difference (graph:nodes graph) reachable-list))
      (setf (classification obj)
            (fset:with (classification obj) node 'not-reachable)))
    (setf (classification obj)
          (fset:with (classification obj) *reachable-from* 'origin))))

(defmethod shape-by ((obj reachable) graph)
  (when (fset:empty? (classification obj)) (classify obj graph))
  (let ((ret (fset:empty-map)))
    (fset:do-map (key value (classification obj))
      (setf ret
            (fset:with ret key (case value
                                 (not-reachable *shape-not-reachable*)
                                 (origin *shape-origin*)
                                 (reachable *shape-reachable*)))))
    ret))

(defmethod fill-by ((obj reachable) graph)
  (when (fset:empty? (classification obj)) (classify obj graph))
  (let ((ret (fset:empty-map)))
    (fset:do-map (key value (classification obj))
      (setf ret
            (fset:with ret key (case value
                                 (not-reachable *color-not-reachable*)
                                 (origin *color-origin*)
                                 (reachable *color-reachable*)))))
    ret))

(defmethod legend-shape ((obj reachable) &key default)
  (if default
      (-> (fset:empty-map)
          (fset:with 'reachable *shape-node-legend*)
          (fset:with 'not-reachable *shape-node-legend*)
          (fset:with 'origin *shape-node-legend*))
      (-> (fset:empty-map)
          (fset:with 'reachable *shape-reachable*)
          (fset:with 'not-reachable *shape-not-reachable*)
          (fset:with 'origin *shape-origin*))))

(defmethod legend-fill ((obj reachable) &key default)
  (if default
      (-> (fset:empty-map)
          (fset:with 'reachable *color-fill-node-legend*)
          (fset:with 'not-reachable *color-fill-node-legend*)
          (fset:with 'origin *color-fill-node-legend*))
      (-> (fset:empty-map)
          (fset:with 'reachable *color-reachable*)
          (fset:with 'not-reachable *color-not-reachable*)
          (fset:with 'origin *color-origin*))))

(defmethod legend-urls ((obj reachable))
  (-> (fset:empty-map)
      (fset:with 'reachable *url-default*)
      (fset:with 'not-reachable *url-default*)
      (fset:with 'origin *url-default*)))

(defmethod add-legend-nodes ((obj reachable) graph)
  (graph:add-node graph 'reachable)
  (graph:add-node graph 'not-reachable)
  (graph:add-node graph 'origin))


;; connected is the same as reachable.  Can this be right?
;; (defclass connected (graph-matrix)
;;   ())

;; (let ((classification (fset:empty-map)))
;;   (setf (get 'connected 'classify)
;;         #'(lambda ()
;;             "Sets unreachable nodes to 'not-reachable, the origin node
;; to 'origin, and reachable nodes to 'reachable"
;;             (reachable-list
;;              (cond
;;                ((< *reachable-limit* 0)
;;                 (graph-matrix:reachable-from
;;                  graph (create-reachability-matrix
;;                         graph *use-fast-matrix*) *reachable-from*))
;;                ((eq *reachable-limit* 0) (list *reachable-from*))
;;                (t (graph-matrix:reachable-from
;;                    graph (create-reachability-matrix
;;                           graph *use-fast-matrix* *reachable-limit*)
;;                    *reachable-from*))))
;;             (dolist (node reachable-list)
;;               (setf classification (fset:with classification node 'reachable)))
;;             (dolist (node (set-difference (graph:nodes graph)
;;                                           reachable-list))
;;               (setf classification
;;                     (fset:with classification node 'not-reachable)))
;;             (setf classification
;;                   (fset:with classification *reachable-from* 'origin))))

;;   (setf (get 'connected 'shape-by)
;;         #'(lambda ()
;;             (when (fset:empty? classification) (funcall (get 'connected 'classify)))
;;             (let ((ret (fset:empty-map)))
;;               (fset:do-map (key value classification)
;;                 (fset:with ret key (case value
;;                                      ('not-reachable *shape-not-reachable*)
;;                                      ('origin *shape-origin*)
;;                                      ('reachable *shape-reachable*))))
;;               ret)))

;;   (setf (get 'connected 'fill-by)
;;         #'(lambda ()
;;             (when (fset:empty? classification) (funcall (get 'connected 'classify)))
;;             (let ((ret (fset:empty-map)))
;;               (fset:do-map (key value classification)
;;                 (fset:with ret key (case value
;;                                      ('not-reachable *color-not-reachable*)
;;                                      ('origin *color-origin*)
;;                                      ('reachable *color-reachable*))))
;;               ret))))

;; (setf (get 'connected 'legend-shape)
;;       #'(lambda (&key default)
;;           (if default
;;               (-> (fset:empty-map)
;;                   (fset:with 'reachable *shape-node-legend*)
;;                   (fset:with 'not-reachable *shape-node-legend*)
;;                   (fset:with 'origin *shape-node-legend*))
;;               (-> (fset:empty-map)
;;                   (fset:with 'reachable *shape-reachable*)
;;                   (fset:with 'not-reachable *shape-not-reachable*)
;;                   (fset:with 'origin *shape-origin*)))))

;; (setf (get 'connected 'legend-fill)
;;       #'(lambda (&key default)
;;           (if default
;;               (-> (fset:empty-map)
;;                   (fset:with 'reachable *color-fill-node-legend*)
;;                   (fset:with 'not-reachable *color-fill-node-legend*)
;;                   (fset:with 'origin *color-fill-node-legend*))
;;               (-> (fset:empty-map)
;;                   (fset:with 'reachable *color-reachable*)
;;                   (fset:with 'not-reachable *color-not-reachable*)
;;                   (fset:with 'origin *color-origin*)))))

;; (setf (get 'connected 'add-legend-nodes)
;;       #'(lambda ()
;;           (graph:add-node graph 'reachable)
;;           (graph:add-node graph 'not-reachable)
;;           (graph:add-node graph 'origin)))


(defclass distance (graph-matrix)
  ())

(defmethod classify ((obj distance) graph)
  "Sets unreachable nodes to 'not-reachable, the origin node to 'origin,
adjacent nodes to 'adjacent, and reachable nodes to 'reachable"
  (let ((val))
    (mapc #'(lambda (node)
              (progn
                (setf val (min (graph-matrix:distance-from-to
                                graph (matrix obj)
                                *reachable-from* node)
                               (graph-matrix:distance-from-to
                                graph (matrix obj) node
                                *reachable-from*)))
                (setf (classification obj)
                      (fset:with (classification obj) node
                                 (case val
                                   (4611686018427387903 'unreachable)
                                   (0 'origin)
                                   (1 'abutting)
                                   (otherwise 'separated))))))
          (graph:nodes graph))))

(defmethod shape-by ((obj distance) graph)
  (when (fset:empty? (classification obj)) (classify obj graph))
  (let ((ret (fset:empty-map)))
    (fset:do-map (key value (classification obj))
      (setf ret
            (fset:with ret key
                       (case value
                         (not-reachable *shape-not-reachable*)
                         (origin *shape-origin*)
                         (abutting *shape-adjacent*)
                         (separated *shape-reachable*)))))
    ret))

(defmethod fill-by ((obj distance) graph)
  (when (fset:empty? (classification obj)) (classify obj graph))
  (let ((ret (fset:empty-map)))
    (fset:do-map (key value (classification obj))
      (setf ret
            (fset:with ret key
                       (case value
                         (unreachable *color-not-reachable*)
                         (origin *color-origin*)
                         (abutting *color-adjacent*)
                         (separated *color-reachable*)))))
    ret))

(defmethod legend-shape ((obj distance) &key default)
  (if default
      (-> (fset:empty-map)
          (fset:with 'separated *shape-node-legend*)
          (fset:with 'not-reachable *shape-node-legend*)
          (fset:with 'origin *shape-node-legend*)
          (fset:with 'abutting *shape-node-legend*))
      (-> (fset:empty-map)
          (fset:with 'separated *shape-reachable*)
          (fset:with 'not-reachable *shape-not-reachable*)
          (fset:with 'origin *shape-origin*)
          (fset:with 'abutting *shape-adjacent*))))

(defmethod legend-fill ((obj distance) &key default)
  (if default
      (-> (fset:empty-map)
          (fset:with 'separated *color-fill-node-legend*)
          (fset:with 'not-reachable *color-fill-node-legend*)
          (fset:with 'origin *color-fill-node-legend*)
          (fset:with 'abutting *color-fill-node-legend*))
      (-> (fset:empty-map)
          (fset:with 'separated *color-reachable*)
          (fset:with 'not-reachable *color-not-reachable*)
          (fset:with 'origin *color-origin*)
          (fset:with 'abutting *color-adjacent*))))

(defmethod legend-urls ((obj distance))
  (-> (fset:empty-map)
      (fset:with 'separated *url-default*)
      (fset:with 'not-reachable *url-default*)
      (fset:with 'origin *url-default*)
      (fset:with 'abutting *url-default*)))

(defmethod add-legend-nodes ((obj distance) graph)
  (graph:add-node graph 'separated)
  (graph:add-node graph 'not-reachable)
  (graph:add-node graph 'origin)
  (graph:add-node graph 'abutting))

;; pass context-table and inference-table
(defclass units (tabular)
  ())

(defmethod classify ((obj units) graph)
  (mapcar #'(lambda (x)
              (setf (classification obj)
                    (fset:with (classification obj) (new-symbol (first x))
                               (new-symbol (second x)))))
          (contexts obj))
  (when *assume-correlations-true*
    (dolist (part (table obj))
      (setf (classification obj)
            (fset:with (classification obj)
                       (read-from-string
                        (format nil "~a=~a" (first part) (second part)))
                       (fset:@ (classification obj) (new-symbol (first part))))))))

(defmethod shape-by ((obj units) graph)
  (when (fset:empty? (classification obj)) (classify obj graph))
  (let ((ret (fset:empty-map)))
    (fset:do-map (key value (classification obj))
      (setf ret
            (fset:with ret key
                       (case value
                         (deposit *shape-node-deposit*)
                         (interface *shape-node-interface*)))))
    ret))

(defmethod fill-by ((obj units) graph)
  (when (fset:empty? (classification obj)) (classify obj graph))
  (let ((ret (fset:empty-map)))
    (fset:do-map (key value (classification obj))
      (setf ret
            (fset:with ret key
                       (case value
                         (deposit *color-fill-node-deposit*)
                         (interface *color-fill-node-interface*)))))
    ret))

(defmethod legend-shape ((obj units) &key default)
  (if default
      (-> (fset:empty-map)
          (fset:with 'deposit *shape-node-legend*)
          (fset:with 'interface *shape-node-legend*))
      (-> (fset:empty-map)
          (fset:with 'deposit *shape-node-deposit*)
          (fset:with 'interface *shape-node-interface*))))

(defmethod legend-fill ((obj units) &key default)
  (if default
      (-> (fset:empty-map)
          (fset:with 'deposit *color-fill-node-legend*)
          (fset:with 'interface *color-fill-node-legend*))
      (-> (fset:empty-map)
          (fset:with 'deposit *color-fill-node-deposit*)
          (fset:with 'interface *color-fill-node-interface*))))

(defmethod legend-urls ((obj units))
  (-> (fset:empty-map)
      (fset:with 'deposit *url-default*)
      (fset:with 'interface *url-default*)))

(defmethod add-legend-nodes ((obj units) graph)
  (graph:add-node graph 'deposit)
  (graph:add-node graph 'interface))


(defun hm-draw (cnf-file-path &optional style-file-path)
  "Read a configuration file and various data files, create a
stratigraphy graph and optionally a chronology graph, and write one or
more dot files according to the variables contained in the
configuration file."

  (initialize-special-variables)

  (let ((rejected)
        (node-fills (fset:empty-map))
        (node-shapes (fset:empty-map))
        ;; (node-styles (fset:empty-map))
        (arc-urls (fset:empty-map))
        (node-urls (fset:empty-map))
        (chronology-graph)
        (context-list)
        (ranks)
        (node-index-hash (make-hash-table))
        (counter -1)
        (context-table)
        (observation-table)
        (inference-table)
        (period-table)
        (phase-table)
        (radiocarbon-table)
        (date-order-table)
        (graph (graph:populate (make-instance 'graph:digraph)))
        (adjacency-matrix)
        (reachability-matrix)
        (distance-matrix)
        (node-filler)
        (node-shaper))


    ;; conditionally read style file
    (when style-file-path
      (unless (hm-read-cnf-file style-file-path)
        (return-from hm-draw
          (format nil "Unable to read style file ~a" style-file-path))))

    ;; read configuration file

    (unless (hm-read-cnf-file cnf-file-path)
      (return-from hm-draw
        (format nil "Unable to read configuration file ~a" cnf-file-path)))

    (when (and *create-chronology-graph* *assume-correlations-true*)
      (return-from hm-draw
        "Cannot create chronology graph when *assume-correlations-true*"))

    (when (and (not *reachable-from*) (eq *node-fill-by* 'reachable))
      (return-from hm-draw
        "When *node-fill-by* is 'reachable' *reachable-from* must be
          specified."))

    (when (and (not *reachable-from*) (eq *node-fill-by* 'distance))
      (return-from hm-draw
        "When *node-fill-by* is 'distance' *reachable-from* must be
          specified."))

    (when *symbolize-unit-type*
      (setf *node-shape-by* 'units))

    ;; read required tables

    (unless
        (setf context-table
              (hm-read-table *context-table-name* *context-table-header*))
      (return-from hm-draw (format nil "Unable to read ~a"
                                   *context-table-name*)))

    (unless
        (setf observation-table
              (hm-read-table *observation-table-name*
                             *observation-table-header*))
      (return-from hm-draw (format nil "Unable to read ~a"
                                   *observation-table-name*)))

    ;; read optional tables, if necessary

    (when *inference-table-name*
      (unless
          (setf inference-table
                (hm-read-table *inference-table-name*
                               *inference-table-header*))
        (return-from hm-draw (format nil "Unable to read ~a"
                                     *inference-table-name*))))

    (when *period-table-name*
      (unless
          (setf period-table
                (hm-read-table *period-table-name* *period-table-header*))
        (return-from hm-draw (format nil "Unable to read ~a"
                                     *period-table-name*))))

    (when *phase-table-name*
      (unless
          (setf phase-table
                (hm-read-table *phase-table-name* *phase-table-header*))
        (return-from hm-draw (format nil "Unable to read ~a"
                                     *phase-table-name*))))

    (when (and *radiocarbon-table-name* *create-chronology-graph*)
      (unless
          (setf radiocarbon-table
                (hm-read-table *radiocarbon-table-name*
                               *radiocarbon-table-header*))
        (return-from hm-draw (format nil "Unable to read ~a"
                                     *radiocarbon-table-name*))))

    (when (and *date-order-table-name* *create-chronology-graph*)
      (unless
          (setf date-order-table
                (hm-read-table *date-order-table-name*
                               *date-order-table-header*))
        (return-from hm-draw (format nil "Unable to read ~a"
                                     *date-order-table-name*))))

    ;; create sequence diagram graph
;;; add nodes

    (dolist (node context-table)
      (graph:add-node graph (new-symbol (first node))))

;;; add arcs

    (dolist (arc observation-table rejected)
      (graph:add-edge graph
                      (list (new-symbol (first arc))
                            (new-symbol (second arc))))
      (unless rejected
        (and (graph:cycles graph) (push arc rejected))))

    ;; if there is a cycle in the graph, shut down

    (when rejected
      (return-from hm-draw
        (format t "A cycle that includes node ~a is present."
                (pop rejected))))

    ;; set ranks of nodes
    ;; possibly assume correlated contexts once-whole
    ;; check for cycles

    (if *assume-correlations-true*
        (progn
          (dolist (part inference-table)
            (graph:merge-nodes
             graph (new-symbol (second part))
             (new-symbol (first part))
             :new (alexandria:symbolicate (new-symbol (first part)) "="
                                          (new-symbol (second part)))))
          (when (graph:cycles graph)
            (return-from hm-draw
              (format t "Correlated contexts introduced a cycle."))))
        (alexandria:appendf ranks (set-same-ranks inference-table)))

    (alexandria:appendf ranks (set-other-ranks context-table))

    (setf distance-matrix (create-distance-matrix graph distance-matrix))
    (setf reachability-matrix
          (create-reachability-matrix graph reachability-matrix
                                      *reachable-limit*))
    (setf adjacency-matrix (create-adjacency-matrix graph adjacency-matrix))

    ;; optionally, fill nodes

    (when *node-fill-by*
      (setf node-filler (make-instance *node-fill-by*))
      (case *node-fill-by*
        (periods (setf (table node-filler) period-table
                       (contexts node-filler) context-table))
        (phases (setf (table node-filler) phase-table
                      (contexts node-filler) context-table))
        (reachable (setf (matrix node-filler) reachability-matrix))
        (distance (setf (matrix node-filler) distance-matrix))
        (units (setf (contexts node-filler) context-table)))
      (setf node-fills
            (fset:map
             (fset:$ node-fills)
             (fset:$ (fill-by node-filler graph))))
      (when *legend* (add-legend-nodes node-filler graph)
            (setf node-fills
                  (fset:map
                   (fset:$ node-fills)
                   (fset:$ (legend-fill node-filler))))
            (when *shape-node-legend*
              (setf node-shapes
                    (fset:map
                     (fset:$ node-shapes)
                     (fset:$ (legend-shape node-filler :default t)))))
            (when *url-include*
              (setf node-urls
                    (fset:map (fset:$ node-urls)
                              (fset:$ (legend-urls node-filler)))))))

    ;; optionally, set node shapes

    (when *node-shape-by*
      (setf node-shaper (make-instance *node-shape-by*))
      (case *node-shape-by*
        (periods (setf (table node-shaper) period-table
                       (contexts node-shaper) context-table))
        (phases (setf (table node-shaper) phase-table
                      (contexts node-shaper) context-table))
        (reachable (setf (matrix node-shaper) reachability-matrix))
        (distance (setf (matrix node-shaper) distance-matrix))
        (units (setf (contexts node-shaper) context-table)))

      (setf node-shapes
            (fset:map
             (fset:$ node-shapes)
             (fset:$ (shape-by node-shaper graph))))
      (when *legend* (add-legend-nodes node-shaper graph)
            (setf node-shapes
                  (fset:map
                   (fset:$ node-shapes)
                   (fset:$ (legend-shape node-shaper))))
            (when *color-fill-node-legend*
              (setf node-fills
                    (fset:map
                     (fset:$ node-fills)
                     (fset:$ (legend-fill node-shaper :default t)))))
            (when *url-include*
              (setf node-urls
                    (fset:map (fset:$ node-urls)
                              (fset:$ (legend-urls node-shaper)))))))

    ;; optionally, add url information

    (when *url-include*
      (setf node-urls
            (fset:map (fset:$ node-urls)
                      (fset:$ (get-node-urls-from context-table inference-table)))
            arc-urls
            (fset:map (fset:$ arc-urls)
                      (fset:$ (get-arc-urls-from observation-table)))))


    ;; optionally, construct the chronology graph

    (when *create-chronology-graph*
      (setf chronology-graph
            (graph:populate (make-instance 'graph:digraph))
            adjacency-matrix
            (create-adjacency-matrix graph adjacency-matrix)
            reachability-matrix
            (create-reachability-matrix graph reachability-matrix))
      (mapc (lambda (node) (setf (gethash node node-index-hash) (incf counter)))
            (graph:nodes graph))
      (dolist (col radiocarbon-table)
        (graph:add-node chronology-graph
                        (new-symbol (second col) "alpha-~a"))
        (graph:add-node chronology-graph
                        (new-symbol (second col) "beta-~a"))
        (graph:add-node chronology-graph
                        (new-symbol (first col) "theta-~a")))
      (when date-order-table
        (dolist (pair date-order-table)
          (graph:add-edge chronology-graph
                          (list (new-symbol (second pair) "theta-~a")
                                (new-symbol (first pair) "theta-~a"))
                          0)))
      (dolist (node radiocarbon-table)
        (when (eq 0 (graph:indegree
                     chronology-graph
                     (new-symbol (first node) "theta-~a")))
          (graph:add-edge chronology-graph
                          (list (new-symbol (second node) "beta-~a")
                                (new-symbol (first node) "theta-~a"))
                          0))
        (when (eq 0 (graph:outdegree
                     chronology-graph
                     (new-symbol (first node) "theta-~a")))
          (graph:add-edge chronology-graph
                          (list (new-symbol (first node) "theta-~a")
                                (new-symbol (second node) "alpha-~a"))
                          0)))
      (dolist (arc radiocarbon-table)
        (push (new-symbol (second arc)) context-list))

      (setf context-list (append (unique-pairs context-list)
                                 (unique-pairs (reverse context-list))))
      (dolist (pair context-list)
        (when (graph-matrix:reachablep
               graph reachability-matrix (first pair) (first (rest pair)))
          (graph:add-edge
           chronology-graph
           (list (new-symbol (first pair) "alpha-~a")
                 (new-symbol (first (rest pair)) "beta-~a"))
           (if (eq 1 (graph-matrix:matrix-ref
                      adjacency-matrix
                      (gethash (first pair) node-index-hash)
                      (gethash (first (rest pair)) node-index-hash))) 1 2))))

      ;; write the dot file for the chronology graph

      (graph-dot:to-dot-file
       chronology-graph (string-downcase (string *output-file-chronology*))
       :attributes
       (list
        (cons :style (format-attribute *graph-style-chronology*))
        (cons :colorscheme (format-attribute *color-scheme-chronology*))
        (cons :dpi (format-attribute *graph-dpi-chronology*))
        (cons :margin (format-attribute *graph-margin-chronology*))
        (cons :bgcolor (format-attribute
                        (color-filter *color-fill-graph-chronology*)))
        (cons :fontname (format-attribute *font-name-graph-chronology*))
        (cons :fontsize (format-attribute *font-size-graph-chronology*))
        (cons :fontcolor (format-attribute
                          (color-filter *font-color-graph-chronology*)))
        (cons :splines (format-attribute *graph-splines-chronology*))
        (cons :page (format-attribute *graph-page-chronology*))
        (cons :size (format-attribute *graph-size-chronology*))
        (cons :ratio (format-attribute *graph-ratio-chronology*))
        (cons :label (format-attribute *graph-title-chronology*))
        (cons :labelloc (format-attribute *graph-labelloc-chronology*)))
       :edge-attrs
       (list
        (cons :style
              (lambda (e)
                (case (graph:edge-value chronology-graph e)
                  (0 (format-attribute *edge-date-chronology*))
                  (1 (format-attribute *edge-abutting-chronology*))
                  (2 (format-attribute *edge-separated-chronology*)))))
        (cons :label (constantly-format ""))
        (cons :arrowhead (constantly-format *edge-arrowhead-chronology*))
        (cons :colorscheme (constantly-format *color-scheme-chronology*))
        (cons :color (constantly-format (color-filter *color-edge-chronology*)))
        (cons :fontname (constantly-format *font-name-edge-chronology*))
        (cons :fontsize (constantly-format *font-size-edge-chronology*))
        (cons :fontcolor (constantly-format
                          (color-filter *font-color-edge-chronology*))))
       :node-attrs
       (list
        (cons :shape
              (lambda (n)
                (if (equal (char (symbol-name n) 0) #\T)
                    (format-attribute *shape-date*)
                    (format-attribute *shape-phase*))))
        (cons :style (constantly-format *style-node-chronology*))
        (cons :fontname (constantly-format *font-name-node-chronology*))
        (cons :fontsize (constantly-format *font-size-node-chronology*))
        (cons :colorscheme (constantly-format *color-scheme-chronology*))
        (cons :color (constantly-format (color-filter *color-node-chronology*)))
        (cons :fillcolor (constantly-format (color-filter
                                             *color-fill-node-chronology*)))
        (cons :fontcolor (constantly-format
                          (color-filter *font-color-node-chronology*)))))
      (format t "Wrote ~a~%"
              (probe-file (string-downcase (string *output-file-chronology*)))))

    ;; write the dot file for the sequence diagram

    (graph-dot:to-dot-file
     graph (string-downcase (string *output-file-sequence*))
     :ranks ranks
     :attributes
     (list
      (cons :style (format-attribute *graph-style-sequence*))
      (cons :colorscheme (format-attribute *color-scheme-sequence*))
      (cons :dpi (format-attribute *graph-dpi-sequence*))
      (cons :URL (format-attribute (url-decode *url-default*)))
      (cons :margin (format-attribute *graph-margin-sequence*))
      (cons :bgcolor (format-attribute (color-filter
                                        *color-fill-graph-sequence*)))
      (cons :fontname (format-attribute *font-name-graph-sequence*))
      (cons :fontsize (format-attribute *font-size-graph-sequence*))
      (cons :fontcolor (format-attribute (color-filter
                                          *font-color-graph-sequence*)))
      (cons :splines (format-attribute *graph-splines-sequence*))
      (cons :page (format-attribute *graph-page-sequence*))
      (cons :size (format-attribute *graph-size-sequence*))
      (cons :ratio (format-attribute *graph-ratio-sequence*))
      (cons :label (format nil "~s" *graph-title-sequence*))
      (cons :labelloc (format-attribute *graph-labelloc-sequence*)))
     :edge-attrs
     (list
      (cons :style (graph-element-control *style-edge-sequence*))
      (cons :arrowhead (constantly-format *edge-arrowhead-sequence*))
      (cons :colorscheme (constantly-format *color-scheme-sequence*))
      (cons :color (constantly-format (color-filter *color-edge-sequence*)))
      (cons :fontname (constantly-format *font-name-edge-sequence*))
      (cons :fontsize (constantly-format *font-size-edge-sequence*))
      (cons :fontcolor (constantly-format (color-filter
                                           *font-color-edge-sequence*)))
      (cons :URL (graph-element-control "" #'url-decode *url-include* arc-urls)))
     :node-attrs
     (list
      (cons :shape (graph-element-control *shape-node-deposit* #'identity
                                          *node-shape-by* node-shapes))
      (cons :label #'(lambda (x) (format nil "\"~a\"" (titlecase-label x))))
      (cons :style (constantly-format *style-node-sequence*))
      (cons :fontname (constantly-format *font-name-node-sequence*))
      (cons :fontsize (constantly-format *font-size-node-sequence*))
      (cons :colorscheme (constantly-format *color-scheme-sequence*))
      (cons :color (constantly-format (color-filter *color-node-sequence*)))
      (cons :fillcolor (graph-element-control *color-fill-node-sequence*
                                              #'color-filter
                                              *node-fill-by* node-fills))
      (cons :fontcolor (graph-element-control *font-color-node-sequence*
                                              #'label-color
                                              *node-fill-by* node-fills))
      (cons :URL (graph-element-control "" #'url-decode *url-include*
                                        node-urls))))
    (return-from hm-draw
      (format nil "Wrote ~a"
              (probe-file (string-downcase (string *output-file-sequence*)))))))
