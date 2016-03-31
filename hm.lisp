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

(defconstant +shapes+
  (-> (fset:empty-map)
      (fset:with 0  'box)
      (fset:with 1  'polygon)
      (fset:with 2  'ellipse)
      (fset:with 3  'egg)
      (fset:with 4  'triangle)
      (fset:with 5  'diamond)
      (fset:with 6  'oval)
      (fset:with 7  'circle)
      (fset:with 8  'point)
      (fset:with 9  'trapezium)
      (fset:with 10  'parallelogram)
      (fset:with 11  'house)
      (fset:with 12  'pentagon)
      (fset:with 13  'hexagon)
      (fset:with 14  'septagon)
      (fset:with 15  'octagon)
      (fset:with 16  'doublecircle)
      (fset:with 17  'doubleoctagon)
      (fset:with 18  'tripleoctagon)
      (fset:with 19  'invtriangle)
      (fset:with 20  'invtrapezium)
      (fset:with 21  'invhouse)
      (fset:with 22  'Mdiamond)
      (fset:with 23  'Msquare)
      (fset:with 24  'Mcircle)
      (fset:with 25  'square)
      (fset:with 26  'star)
      (fset:with 27  'underline)
      (fset:with 28  'note)
      (fset:with 29  'tab)
      (fset:with 30  'folder)
      (fset:with 31  'box3d)
      (fset:with 32  'component)
      (fset:with 33  'cds)
      (fset:with 34  'signature)
      (fset:with 35  'rpromoter)
      (fset:with 36  'rarrow)
      (fset:with 37  'larrow)
      (fset:with 38  'lpromoter)))

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
(defparameter *event-table-name* nil
  "Optional input file path for the radiocarbon table.")
(defparameter *event-order-table-name* nil
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
(defparameter *event-table-header* nil
  "Switch for a header line in the radiocarbon table, nil for no header
  line and non-nil for a header line.")
(defparameter *event-order-table-header* nil
  "Switch for a header line in the event-order table, nil for no header
  line and non-nil for a header line.")
(defparameter *symbolize-unit-type* nil
  "Switch to distinguish interfaces and deposits, nil for no
  distinction and non-nil to distinguish.")
(defparameter *create-chronology-graph* nil
  "Switch for a chronology graph, nil for no graph and non-nil to
  create a graph.")
(defparameter *node-fill-by* nil
  "Fill nodes of a sequence graph according to some procedure; nil for
  no procedure, or one of 'levels', 'units, 'reachable', 'periods',
  'phases', 'connected', or 'distance'.")
(defparameter *node-shape-by* nil
  "Shape nodes of a sequence graph according to some procedure; nil
  for no procedure, or one of 'levels', 'units, 'reachable',
  'periods', 'phases', 'connected', or 'distance'. ")
(defparameter *node-color-by* nil
  "Color node outliness of a sequence graph according to some
  procedure; nil for no procedure, or one of 'levels', 'units',
  'reachable', 'periods', 'phases', 'connected', or 'distance'. ")
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
(defparameter *color-node-legend* nil
  "The color of the node outline in a legend.  Either a color name
  from the active color space or an integer index into a Brewer color
  scheme.")
(defparameter *color-fill-node-chronology* nil
  "The color used to fill nodes in a chronology graph.  Either a color
  name from the active color space or an integer index into a Brewer
  color scheme.")
;; Add the next two to example configuration files
(defparameter *color-node-deposit* nil
  "A color used for node outlines of depositional contexts in a
  sequence graph.  Either a color name from the active color space or
  an integer index into a Brewer color scheme.")
(defparameter *color-node-interface* nil
  "A color used for node outlines of interfacial contexts in a
  sequence graph.  Either a color name from the active color space or
  an integer index into a Brewer color scheme.")
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
(defparameter *font-size-subscript* nil
  "The font size of subscripts in html-like labels.")

;; Graphviz node shape attributes
(defparameter *shape-node-legend* nil
  "A valid GraphViz dot node shape for node entries in the legend.")
(defparameter *shape-node-interface* nil
  "A valid GraphViz dot node shape for interfacial contexts.")
(defparameter *shape-node-deposit* nil
  "A valid GraphViz dot node shape for depositional contexts.")
(defparameter *node-outline-width* 1.0
  "The pen width used to draw node outlines.  A value of 1.0 is the
  Graphviz dot default.  A larger value will make the outline color
  easier to see.  The minimum value for this parameter is 0.0.")

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

(defparameter *x* nil
  "Debugging help")

;; general function definitions

(defun initialize-special-variables ()
  (setf *output-file-sequence* nil
        *output-file-chronology* nil
        *context-table-name* nil
        *observation-table-name* nil
        *inference-table-name* nil
        *period-table-name* nil
        *phase-table-name* nil
        *event-table-name* nil
        *event-order-table-name* nil
        *context-table-header* nil
        *observation-table-header* nil
        *inference-table-header* nil
        *period-table-header* nil
        *phase-table-header* nil
        *event-table-header* nil
        *event-order-table-header* nil
        *symbolize-unit-type* nil
        *create-chronology-graph* nil
        *node-fill-by* nil
        *node-shape-by* nil
        *node-color-by* nil
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
        *font-size-subscript* 12
        *shape-node-legend* nil
        *shape-node-interface* nil
        *shape-node-deposit* nil
        *node-outline-width* 1.0
        *graph-splines-sequence* nil
        *graph-splines-chronology* nil
        *edge-arrowhead-sequence* nil
        *edge-arrowhead-chronology* nil
        *edge-date-chronology* nil
        *edge-abutting-chronology* nil
        *edge-separated-chronology* nil
        *x* nil))

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

(defun new-symbol (string &optional (format "~a"))
  (unless (alexandria:emptyp string)
    (if (and format (not (string= string "nil")))
        (alexandria:format-symbol t format string)
        (read-from-string string))))

(defun url-decode (url)
  "Decodes symbol URL and returns a valid url as a string."
  (do-urlencode:urldecode (string url)))

(defun add-missing-interfaces (graph contexts)
  "Check for edges in GRAPH that connect two depositional nodes and, if found,
  insert an interfacial node between them."
  (let ((hiatus)
        (context-lookup (fset:empty-map)))
    (dolist (context contexts)
      (setf context-lookup
            (fset:with context-lookup (new-symbol (first context))
                       (new-symbol (second context)))))
    (dolist (edge (graph:edges graph))
      (and (eq (fset:@ context-lookup (first edge)) 'deposit)
           (eq (fset:@ context-lookup (second edge)) 'deposit)
           (push edge hiatus)))
    (dolist (edge hiatus)
      (let ((new-node (alexandria:symbolicate (second edge) "-*surface*")))
        (graph:add-node graph new-node)
        (graph:add-edge graph (list (first edge) new-node))
        (graph:add-edge graph (list new-node (second edge)))
        (graph:delete-edge graph edge)))))

;; Label function definition.

(defun chronology-graph-html-label (id size)
  "Convert the symbol ID into an html label for Graphviz dot.  This
function is designed specifically for use with chronology graphs."
  (let* ((s (string-downcase (string id)))
         (hyphen-pos (position #\- s :test #'equal))
         (greek (subseq s 0 hyphen-pos))
         (post (subseq s (+ 1 hyphen-pos))))
    (format nil "<&~a;<FONT POINT-SIZE=\"~a\"><SUB>~a</SUB></FONT>>"
            greek size post)))

;; io function definitions

(defun read-configuration-file (name)
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
                (*graph-title-sequence* (new-symbol (second row)))
                (*graph-title-chronology* (new-symbol (second row)))
                (*reachable-from* (new-symbol (second row)))
                (*graph-size-sequence* (new-symbol (second row)))
                (*graph-page-sequence* (new-symbol (second row)))
                (*graph-margin-sequence* (new-symbol (second row)))
                (*graph-size-chronology* (new-symbol (second row)))
                (*graph-page-chronology* (new-symbol (second row)))
                (*graph-margin-chronology* (new-symbol (second row)))
                (*url-default* (new-symbol (do-urlencode:urlencode (second row))))
                (otherwise (new-symbol (second row) nil))))))
    t))

(defun read-table (name header)
  "Checks that NAME is a file, then attempts to read it as
comma-separated values.  HEADER indicates whether or not the first
line of NAME contains column heads, rather than values."
  (alexandria:when-let (in-file (probe-file (string name)))
    (format t "Reading table ~a~%" in-file)
    (cl-csv:read-csv in-file :skip-first-p header)))

;; filter function definitions

(defun color-filter (color)
  "Returns a valid Graphviz dot color designator. The result is either
an integer or a symbol with a color name appended to a color space.
COLOR can either be an integer, in which case Brewer colors are assumed,
or a symbol whose string value is a color name.  Returns 0 if COLOR is
nil or a string."
  (cond
    ((not color) 0)
    ((integerp color) color)
    ((eq color 'transparent) color)
    ((symbolp color) (alexandria:symbolicate "/" *color-space* "/" color))
    (t 0)))

(defun shape-filter (shape)
  "Returns a valid Graphviz dot node shape as a symbol."
  (cond
    ((integerp shape) (fset:@ +shapes+ shape))
    ((stringp shape) (if (alexandria:emptyp shape) nil
                         (alexandria:symbolicate shape)))
    ((symbolp shape) shape)
    (t nil)))

;; formatting functions

(defun format-attribute (att &key quote preserve-case)
  "A convenience function to transform a Lisp symbol into a GraphViz
dot attribute."
  (if (not att) "\"\""
      (if quote
          (if preserve-case
              (format nil "\"~a\"" att)
              (format nil "\"~(~a~)\"" att))
          (if preserve-case
              (format nil "~a" att)
              (format nil "~(~a~)" att)))))

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
                               (do-urlencode:urlencode (sixth node))))))
    (when *assume-correlations-true*
      (dolist (part inferences)
        (setf ret (fset:with ret (read-from-string
                                  (format nil "~a=~a" (first part) (second part)))
                             (if (string= (third part) "")
                                 (if *url-default*  *url-default* "")
                                 (do-urlencode:urlencode  (third part)))))))
    ret))

(defun get-arc-urls-from (table)
  "Reads URL's from TABLE.  Returns a map where the keys are
arcs and the values are URL's."
  (let ((ret (fset:empty-map)))
    (dolist (arc table)
      (setf ret
            (fset:with ret (list (new-symbol (first arc))
                                 (new-symbol (second arc)))
                       (if (string= (third arc) "")
                           (if *url-default* *url-default* "")
                           (do-urlencode:urlencode (third arc))))))
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
                       :node-list
                       (list (format nil "~s" (new-symbol (first x) nil))
                             (format nil "~s" (new-symbol (second x) nil))))
                      ranks))
            table)
    ranks))

(defun set-other-ranks (table)
  "Use the values in TABLE to return a list of graph:rank structures
where the indicated nodes either appear at the top or the bottom of
the graph picture."
  (let ((ranks))
    (mapcar #'(lambda (x)
                (let ((rank (new-symbol (third x) nil)))
                  (when (or (eq rank 'basal)
                            (eq rank 'surface))
                    (push (graph-dot::make-rank
                           :value
                           (cond
                             ((eq rank 'basal) "sink")
                             ((eq rank 'surface) "source"))
                           :node-list
                           (list (string (new-symbol (first x)))))
                          ranks))))
            table)
    ranks))


;; Classifiers

(defclass classifier ()
  ((map :accessor classification
        :initform (fset:empty-map)
        :documentation "An fset map to hold the classification for the
        nodes.  The key is the node label and the value is set
        according to the CLASSIFY method.")
   (table :accessor table
          :initform nil
          :initarg :table
          :documentation "A table that holds user-specified
          classification information, either for periods or for
          phases.")
   (contexts :accessor contexts
             :initform nil
             :initarg :contexts
             :allocation :class
             :documentation "Contextual information for each of the
             nodes, supplied by the user.")
   (matrix :accessor matrix
           :initform nil
           :initarg :matrix
           :documentation "A matrix representation of the
           stratigraphic graph, typically the distance matrix.")
   (legend-node-shape :accessor legend-node-shape
                      :initform 'box
                      :initarg :legend-node-shape
                      :documentation "A default shape for nodes in the
                      legend.  Typically supplied by the user as
                      *shape-node-legend*.")
   (legend-node-fill :accessor legend-node-fill
                     :initform 'white
                     :initarg :legend-node-fill
                     :documentation "A default fill color for nodes in
                     the legend.  Typically supplied by the user as
                     *color-fill-node-legend*.")
   (legend-node-color :accessor legend-node-color
                      :initform 'black
                      :initarg :legend-node-color
                      :documentation "A default outline color for
                      nodes in the legend.  Typically supplied by the
                      user as *color-node-legend*.")
   (origin-node :accessor origin-node
                :initform nil
                :initarg :origin-node
                :documentation "The label of the node used as the
                origin in computations of adjacency, reachability, and
                distance.")
   (origin-attribute :accessor origin
                     :initform nil
                     :initarg :origin
                     :documentation "An attribute of the origin node.
                     Currently, this might be a shape, a fill color,
                     or a node color.")
   (adjacent-attribute :accessor adjacent
                       :initform nil
                       :initarg :adjacent
                       :documentation "An attribute of nodes adjacent
                       to the origin node.  Currently, this might be a
                       shape, a fill color, or a node color.")
   (reachable-attribute :accessor reachable
                        :initform nil
                        :initarg :reachable
                        :documentation "An attribute of nodes
                        reachable from the origin node.  Currently,
                        this might be a shape, a fill color, or a node
                        color.")
   (unreachable-attribute :accessor unreachable
                          :initform nil
                          :initarg :unreachable
                          :documentation "An attribute of nodes
                          unreachable from the origin node.
                          Currently, this might be a shape, a fill
                          color, or a node color.")
   (deposit-attribute :accessor deposit
                      :initform nil
                      :initarg :deposit
                      :documentation "An attribute of nodes that
                      represent deposits.  Currently, this might be a
                      shape, a fill color, or a node color.")
   (interface-attribute :accessor interface
                        :initform nil
                        :initarg :interface
                        :documentation "An attribute of nodes that
                        represent interfaces.  Currently, this might
                        be a shape, a fill color, or a node color.")))

(defgeneric classify (obj graph)
  (:documentation "Make a classification and store it in the MAP slot."))

(defgeneric classify-by (obj graph)
  (:documentation "Classify objects according to the classification in
  OBJ.  Returns an fset map."))

(defgeneric legend-shape (obj &key default)
  (:documentation "A shape for the legend node.  If DEFAULT, use a
  value supplied by the user."))

(defgeneric legend-fill (obj &key default)
  (:documentation "A fill color for the legend node.  If DEFAULT, use
  a value supplied by the user."))

(defgeneric legend-color (obj &key default)
  (:documentation "An outline color for the legend node.  If DEFAULT,
  use a value supplied by the user."))

(defgeneric add-legend-nodes (obj graph)
  (:documentation "Adds nodes for the legend to GRAPH.  Note that this
  method should be called just before drawing the graph and after
  all the classifications have been completed."))

(defgeneric add-legend-edges (obj graph)
  (:documentation "Adds edges for the legend to GRAPH. Note that this
  method should be called just before drawing the graph and after all
  the classifications have been completed."))

(defmethod classify-by ((obj classifier) graph)
  (when (fset:empty? (classification obj)) (classify obj graph))
  (classification obj))

(defclass levels (classifier)
  () (:documentation "A simple classifier that assigns a positive
integer to each node in GRAPH, called its level, where, for each
directed edge (a b) the corresponding integers satisfy a < b."))

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
  ()
  (:documentation "A classifier based on information contained in a
  user-specified table, which might hold information on periods or
  phases."))

(defmethod legend-shape ((obj tabular) &key default)
  (let ((ret (fset:empty-map)))
    (mapcar #'(lambda (x)
                (setf ret
                      (fset:with ret (new-symbol (second x))
                                 (if default (legend-node-shape obj)
                                     (new-symbol (third x))))))
            (table obj))
    ret))

(defmethod legend-fill ((obj tabular) &key default)
  (let ((ret (fset:empty-map)))
    (mapcar #'(lambda (x)
                (setf ret
                      (fset:with ret (new-symbol (second x))
                                 (if default (legend-node-fill obj)
                                     (new-symbol (third x) nil)))))
            (table obj))
    ret))

(defmethod legend-color ((obj tabular) &key default)
  (let ((ret (fset:empty-map)))
    (mapcar #'(lambda (x)
                (setf ret
                      (fset:with ret (new-symbol (second x))
                                 (if default (legend-node-color obj)
                                     (new-symbol (third x) nil)))))
            (table obj))
    ret))

(defmethod legend-urls ((obj tabular))
  (let ((ret (fset:empty-map)))
    (mapcar #'(lambda (x)
                (setf ret
                      (fset:with ret (new-symbol (second x)) *url-default*)))
            (table obj))
    ret))

(defmethod add-legend-nodes ((obj tabular) graph)
  (mapcar #'(lambda (x)
              (graph:add-node graph (new-symbol (second x))))
          (table obj)))

(defmethod add-legend-edges ((obj tabular) graph)
  (loop for (x y) on (table obj) while y
     do (graph:add-edge graph (list (new-symbol (second x))
                                    (new-symbol (second y))))))

(defclass periods (tabular) ())

(defmethod classify ((obj periods) graph)
  (let ((ht (fset:empty-map)))
    (mapcar
     #'(lambda (x)
         (setf ht (fset:with ht
                             (new-symbol (first x))
                             (new-symbol (third x) nil))))
     (table obj))
    (mapcar
     #'(lambda (x)
         (setf (classification obj)
               (fset:with (classification obj)
                          (new-symbol (first x))
                          (fset:@ ht (new-symbol (fourth x))))))
     (contexts obj))))

(defclass phases (tabular) ())

(defmethod classify ((obj phases) graph)
  (let ((ht (fset:empty-map)))
    (mapcar
     #'(lambda (x)
         (setf ht (fset:with ht
                             (new-symbol (first x))
                             (new-symbol (third x) nil))))
     (table obj))
    (mapcar
     #'(lambda (x)
         (setf (classification obj)
               (fset:with (classification obj)
                          (new-symbol (first x))
                          (fset:@ ht (new-symbol (fifth x))))))
     (contexts obj))))

(defclass reachable (classifier)
  () (:documentation "A classification based on the reachability
  matrix of GRAPH that distinguishes the origin node, nodes reachable
  from the origin, and nodes not reachable from the origin."))

(defmethod classify ((obj reachable) graph)
  "Sets unreachable nodes to 'not-reachable, the origin node to
'origin, and reachable nodes to 'reachable.  The origin node is
typically set by the special variable *reachable-from*."
  (let ((reachable-list))
    (cond
      ((< *reachable-limit* 0)
       (setf reachable-list
             (graph-matrix:reachable-from graph (matrix obj)
                                          (origin-node obj))))
      ((eq *reachable-limit* 0) (setf reachable-list ()))
      ((eq *reachable-limit* 1)
       (mapc (lambda (x)
               (when (eql (first x) (origin-node obj))
                 (push (second x) reachable-list)))
             (graph:edges graph)))
      (t (setf reachable-list (graph-matrix:reachable-from
                               graph (matrix obj) (origin-node obj)))))
    (dolist (node reachable-list)
      (setf (classification obj) (fset:with (classification obj) node 'reachable)))
    (dolist (node (set-difference (graph:nodes graph) reachable-list))
      (setf (classification obj)
            (fset:with (classification obj) node 'not-reachable)))
    (setf (classification obj)
          (fset:with (classification obj) (origin-node obj) 'origin))))

(defmethod classify-by ((obj reachable) graph)
  (when (fset:empty? (classification obj)) (classify obj graph))
  (let ((ret (fset:empty-map)))
    (fset:do-map (key value (classification obj))
      (setf ret
            (fset:with ret key (case value
                                 (not-reachable (unreachable obj))
                                 (origin (origin obj))
                                 (reachable (reachable obj))))))
    ret))

(defmethod legend-shape ((obj reachable) &key default)
  (if default
      (-> (fset:empty-map)
          (fset:with (new-symbol "Reachable") *shape-node-legend*)
          (fset:with (new-symbol "Not reachable") *shape-node-legend*)
          (fset:with (new-symbol "Origin") *shape-node-legend*))
      (-> (fset:empty-map)
          (fset:with (new-symbol "Reachable") *shape-reachable*)
          (fset:with (new-symbol "Not reachable") *shape-not-reachable*)
          (fset:with (new-symbol "Origin") *shape-origin*))))

(defmethod legend-fill ((obj reachable) &key default)
  (if default
      (-> (fset:empty-map)
          (fset:with (new-symbol "Reachable") *color-fill-node-legend*)
          (fset:with (new-symbol "Not reachable") *color-fill-node-legend*)
          (fset:with (new-symbol "Origin") *color-fill-node-legend*))
      (-> (fset:empty-map)
          (fset:with (new-symbol "Reachable") *color-reachable*)
          (fset:with (new-symbol "Not reachable") *color-not-reachable*)
          (fset:with (new-symbol "Origin") *color-origin*))))

(defmethod legend-color ((obj reachable) &key default)
  (if default
      (-> (fset:empty-map)
          (fset:with (new-symbol "Reachable") *color-node-legend*)
          (fset:with (new-symbol "Not reachable") *color-node-legend*)
          (fset:with (new-symbol "Origin") *color-node-legend*))
      (-> (fset:empty-map)
          (fset:with (new-symbol "Reachable") *color-reachable*)
          (fset:with (new-symbol "Not reachable") *color-not-reachable*)
          (fset:with (new-symbol "Origin") *color-origin*))))

(defmethod legend-urls ((obj reachable))
  (-> (fset:empty-map)
      (fset:with (new-symbol "Reachable") *url-default*)
      (fset:with (new-symbol "Not reachable") *url-default*)
      (fset:with (new-symbol "Origin") *url-default*)))

(defmethod add-legend-nodes ((obj reachable) graph)
  (graph:add-node graph (new-symbol "Reachable"))
  (graph:add-node graph (new-symbol "Not reachable"))
  (graph:add-node graph (new-symbol "Origin")))

(defmethod add-legend-edges ((obj reachable) graph)
  (graph:add-edge graph (list
                         (new-symbol "Reachable") (new-symbol "Not reachable")))
  (graph:add-edge graph (list (new-symbol "Origin") (new-symbol "Reachable"))))

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


(defclass distance (classifier)
  () (:documentation "A classification based on the distance matrix of GRAPH."))

(defmethod classify ((obj distance) graph)
  "Sets unreachable nodes to 'not-reachable, the origin node to 'origin,
adjacent nodes to 'adjacent, and reachable nodes to 'reachable"
  (let (val)
    (mapc #'(lambda (node)
              (setf val (min (graph-matrix:distance-from-to
                              graph (matrix obj)
                              (origin-node obj) node)
                             (graph-matrix:distance-from-to
                              graph (matrix obj) node
                              (origin-node obj))))
              (setf (classification obj)
                    (fset:with (classification obj) node
                               (cond
                                 ((graph-matrix:infinitep val (matrix obj))
                                  'unreachable)
                                 ((eql (round val) 0) 'origin)
                                 ((eql (round val) 1) 'abutting)
                                 (t 'separated)))))
          (graph:nodes graph))))

(defmethod classify-by ((obj distance) graph)
  (when (fset:empty? (classification obj)) (classify obj graph))
  (let ((ret (fset:empty-map)))
    (fset:do-map (key value (classification obj))
      (setf ret
            (fset:with ret key
                       (case value
                         (unreachable (unreachable obj))
                         (origin (origin obj))
                         (abutting (adjacent obj))
                         (separated (reachable obj))))))
    ret))

(defmethod legend-shape ((obj distance) &key default)
  (if default
      (-> (fset:empty-map)
          (fset:with (new-symbol "Separated") *shape-node-legend*)
          (fset:with (new-symbol "Not reachable") *shape-node-legend*)
          (fset:with (new-symbol "Origin") *shape-node-legend*)
          (fset:with (new-symbol "Abutting") *shape-node-legend*))
      (-> (fset:empty-map)
          (fset:with (new-symbol "Separated") *shape-reachable*)
          (fset:with (new-symbol "Not reachable") *shape-not-reachable*)
          (fset:with (new-symbol "Origin") *shape-origin*)
          (fset:with (new-symbol "Abutting") *shape-adjacent*))))

(defmethod legend-fill ((obj distance) &key default)
  (if default
      (-> (fset:empty-map)
          (fset:with (new-symbol "Separated") *color-fill-node-legend*)
          (fset:with (new-symbol "Not reachable") *color-fill-node-legend*)
          (fset:with (new-symbol "Origin") *color-fill-node-legend*)
          (fset:with (new-symbol "Abutting") *color-fill-node-legend*))
      (-> (fset:empty-map)
          (fset:with (new-symbol "Separated") *color-reachable*)
          (fset:with (new-symbol "Not reachable") *color-not-reachable*)
          (fset:with (new-symbol "Origin") *color-origin*)
          (fset:with (new-symbol "Abutting") *color-adjacent*))))

(defmethod legend-color ((obj distance) &key default)
  (if default
      (-> (fset:empty-map)
          (fset:with (new-symbol "Separated") *color-node-legend*)
          (fset:with (new-symbol "Not reachable") *color-node-legend*)
          (fset:with (new-symbol "Origin") *color-node-legend*)
          (fset:with (new-symbol "Abutting") *color-node-legend*))
      (-> (fset:empty-map)
          (fset:with (new-symbol "Separated") *color-reachable*)
          (fset:with (new-symbol "Not reachable") *color-not-reachable*)
          (fset:with (new-symbol "Origin") *color-origin*)
          (fset:with (new-symbol "Abutting") *color-adjacent*))))

(defmethod legend-urls ((obj distance))
  (-> (fset:empty-map)
      (fset:with (new-symbol "Separated") *url-default*)
      (fset:with (new-symbol "Not reachable") *url-default*)
      (fset:with (new-symbol "Origin") *url-default*)
      (fset:with (new-symbol "Abutting") *url-default*)))

(defmethod add-legend-nodes ((obj distance) graph)
  (graph:add-node graph (new-symbol "Separated"))
  (graph:add-node graph (new-symbol "Not reachable"))
  (graph:add-node graph (new-symbol "Origin"))
  (graph:add-node graph (new-symbol "Abutting")))

(defmethod add-legend-edges ((obj distance) graph)
  (graph:add-edge graph
                  (list (new-symbol "Separated") (new-symbol "Not reachable")))
  (graph:add-edge graph (list (new-symbol "Abutting") (new-symbol "Separated")))
  (graph:add-edge graph (list (new-symbol "Origin") (new-symbol "Abutting"))))

;; pass context-table and inference-table
(defclass units (tabular)
  ())

(defmethod classify ((obj units) graph)
  (mapcar #'(lambda (x)
              (setf (classification obj)
                    (fset:with (classification obj) (new-symbol (first x))
                               (new-symbol (second x) nil))))
          (contexts obj))
  (when *assume-correlations-true*
    (dolist (part (table obj))
      (setf (classification obj)
            (fset:with (classification obj)
                       (read-from-string
                        (format nil "~a=~a" (first part) (second part)))
                       (fset:@ (classification obj) (new-symbol (first part))))))))

(defmethod classify-by ((obj units) graph)
  (when (fset:empty? (classification obj)) (classify obj graph))
  (let ((ret (fset:empty-map)))
    (fset:do-map (key value (classification obj))
      (setf ret
            (fset:with ret key
                       (case value
                         (deposit (deposit obj))
                         (interface (interface obj))))))
    ret))

(defmethod legend-shape ((obj units) &key default)
  (if default
      (-> (fset:empty-map)
          (fset:with (new-symbol "Deposit") *shape-node-legend*)
          (fset:with (new-symbol "Interface") *shape-node-legend*))
      (-> (fset:empty-map)
          (fset:with (new-symbol "Deposit") *shape-node-deposit*)
          (fset:with (new-symbol "Interface") *shape-node-interface*))))

(defmethod legend-fill ((obj units) &key default)
  (if default
      (-> (fset:empty-map)
          (fset:with (new-symbol "Deposit") *color-fill-node-legend*)
          (fset:with (new-symbol "Interface") *color-fill-node-legend*))
      (-> (fset:empty-map)
          (fset:with (new-symbol "Deposit") *color-fill-node-deposit*)
          (fset:with (new-symbol "Interface") *color-fill-node-interface*))))

(defmethod legend-color ((obj units) &key default)
  (if default
      (-> (fset:empty-map)
          (fset:with (new-symbol "Deposit") *color-node-legend*)
          (fset:with (new-symbol "Interface") *color-node-legend*))
      (-> (fset:empty-map)
          (fset:with (new-symbol "Deposit") *color-node-deposit*)
          (fset:with (new-symbol "Interface") *color-node-interface*))))

(defmethod legend-urls ((obj units))
  (-> (fset:empty-map)
      (fset:with (new-symbol "Deposit") *url-default*)
      (fset:with (new-symbol "Interface") *url-default*)))

(defmethod add-legend-nodes ((obj units) graph)
  (graph:add-node graph (new-symbol "Deposit"))
  (graph:add-node graph (new-symbol "Interface")))

(defmethod add-legend-edges ((obj units) graph)
  (graph:add-edge graph (list (new-symbol "Deposit") (new-symbol "Interface"))))

(defun hm-draw (cnf-file-path &optional style-file-path)
  "Read a configuration file and various data files, create a
stratigraphy graph and optionally a chronology graph, and write one or
more dot files according to the variables contained in the
configuration file."

  (initialize-special-variables)

  (let ((rejected)
        (node-fills (fset:empty-map))
        (node-shapes (fset:empty-map))
        (node-colors (fset:empty-map))
        (arc-urls (fset:empty-map))
        (node-urls (fset:empty-map))
        (chronology-graph)
        (context-list)
        (ranks)
        (node-index-hash (make-hash-table))
        (attributes)
        (context-table)
        (observation-table)
        (inference-table)
        (period-table)
        (phase-table)
        (event-table)
        (event-order-table)
        (graph (graph:populate (make-instance 'graph:digraph)))
        (cycle-graph (graph:populate (make-instance 'graph:digraph)))
        (adjacency-matrix)
        (reachability-matrix)
        (distance-matrix)
        (node-filler)
        (node-shaper)
        (node-colorer))


    ;; conditionally read style file
    (when style-file-path
      (unless (read-configuration-file style-file-path)
        (return-from hm-draw
          (format nil "Unable to read style file ~a" style-file-path))))

    ;; read configuration file

    (unless (read-configuration-file cnf-file-path)
      (return-from hm-draw
        (format nil "Unable to read configuration file ~a" cnf-file-path)))

    ;; check for some user configuration errors
    (setf attributes
          (-> (fset:empty-set)
              (fset:with *node-fill-by*)
              (fset:with *node-shape-by*)
              (fset:with *node-color-by*)))

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
      (setf *node-shape-by* (new-symbol "units" nil)))

    ;; read required tables

    (unless
        (setf context-table
              (read-table *context-table-name* *context-table-header*))
      (return-from hm-draw (format nil "Unable to read ~a"
                                   *context-table-name*)))

    (unless
        (setf observation-table
              (read-table *observation-table-name*
                          *observation-table-header*))
      (return-from hm-draw (format nil "Unable to read ~a"
                                   *observation-table-name*)))

    ;; read optional tables, if necessary

    (when *inference-table-name*
      (unless
          (setf inference-table
                (read-table *inference-table-name*
                            *inference-table-header*))
        (return-from hm-draw (format nil "Unable to read ~a"
                                     *inference-table-name*))))

    (when *period-table-name*
      (unless
          (setf period-table
                (read-table *period-table-name* *period-table-header*))
        (return-from hm-draw (format nil "Unable to read ~a"
                                     *period-table-name*))))

    (when *phase-table-name*
      (unless
          (setf phase-table
                (read-table *phase-table-name* *phase-table-header*))
        (return-from hm-draw (format nil "Unable to read ~a"
                                     *phase-table-name*))))

    (when (and *event-table-name* *create-chronology-graph*)
      (unless
          (setf event-table
                (read-table *event-table-name* *event-table-header*))
        (return-from hm-draw (format nil "Unable to read ~a"
                                     *event-table-name*))))

    (when (and *event-order-table-name* *create-chronology-graph*)
      (unless
          (setf event-order-table
                (read-table *event-order-table-name* *event-order-table-header*))
        (return-from hm-draw (format nil "Unable to read ~a"
                                     *event-order-table-name*))))

    ;; create sequence diagram graph

;;; add nodes
    (format t "Adding nodes to graph.~&")
    (dolist (node context-table)
      (graph:add-node graph (new-symbol (first node))))

    ;; Check here

;;; add arcs
    (format t "Adding arcs to graph.~&")
    (dolist (arc observation-table)
      (graph:add-edge graph
                      (list (new-symbol (first arc))
                            (new-symbol (second arc)))))

    (when (graph:cycles graph)
      (format t
              "A cycle has been found.~&")
      (dolist (arc observation-table rejected)
        (graph:add-edge cycle-graph
                        (list (new-symbol (first arc))
                              (new-symbol (second arc))))
        (unless rejected
          (and (graph:cycles cycle-graph) (push arc rejected))))

      ;; Report the cycle and shut down

      (when rejected
        (let ((bad-arc (pop rejected)))
             (return-from hm-draw
               (format t "A cycle that includes ~a and ~a is present."
                       (first bad-arc) (second bad-arc))))))

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

    (format t "Setting ranks.~&")
    (alexandria:appendf ranks (set-other-ranks context-table))

    (when (fset:contains? attributes 'distance)
      (format t "Creating distance matrix.~&")
      (setf distance-matrix (create-distance-matrix graph distance-matrix)))

    (when (fset:contains? attributes 'reachable)
      (format t "Creating reachability matrix.")
      (setf reachability-matrix
            (create-reachability-matrix graph reachability-matrix
                                        *reachable-limit*)))

    ;; optionally, fill nodes

    (when *node-fill-by*
      (format t "Creating node filler.~&")
      (setf node-filler
            (make-instance
             *node-fill-by*
             :table (if (eq *node-fill-by* 'periods) period-table phase-table)
             :contexts context-table
             :matrix (if (eq *node-fill-by* 'reachable)
                         reachability-matrix distance-matrix)
             :legend-node-shape *shape-node-legend*
             :legend-node-fill *color-fill-node-legend*
             :legend-node-color *color-node-legend*
             :origin-node *reachable-from*
             :origin *color-origin*
             :adjacent *color-adjacent*
             :reachable *color-reachable*
             :unreachable *color-not-reachable*
             :deposit *color-fill-node-deposit*
             :interface *color-fill-node-interface*))
      (setf node-fills (fset:map (fset:$ node-fills)
                                 (fset:$ (classify-by node-filler graph)))))

    ;; optionally, set node shapes

    (when *node-shape-by*
      (format t "Creating node shaper.~&")
      (setf node-shaper
            (make-instance
             *node-shape-by*
             :table (if (eql *node-shape-by* 'periods) period-table phase-table)
             :contexts context-table
             :matrix (if (eql *node-shape-by* 'reachable)
                         reachability-matrix distance-matrix)
             :legend-node-shape *shape-node-legend*
             :legend-node-fill *color-fill-node-legend*
             :legend-node-color *color-node-legend*
             :origin-node *reachable-from*
             :origin *shape-origin*
             :adjacent *shape-adjacent*
             :reachable *shape-reachable*
             :unreachable *shape-not-reachable*
             :deposit *shape-node-deposit*
             :interface *shape-node-interface*))
      (setf node-shapes
            (fset:map (fset:$ node-shapes)
                      (fset:$ (classify-by node-shaper graph)))))

    ;; optionally, color nodes

    (when *node-color-by*
      (format t "Creating node colorer.~&")
      (setf node-colorer
            (make-instance
             *node-color-by*
             :table (if (eql *node-color-by* 'periods) period-table phase-table)
             :contexts context-table
             :matrix (if (eql *node-color-by* 'reachable)
                         reachability-matrix distance-matrix)
             :legend-node-shape *shape-node-legend*
             :legend-node-fill *color-fill-node-legend*
             :legend-node-color *color-node-legend*
             :origin-node *reachable-from*
             :origin *color-origin*
             :adjacent *color-adjacent*
             :reachable *color-reachable*
             :unreachable *color-not-reachable*
             :deposit *color-node-deposit*
             :interface *color-node-interface*))
      (setf node-colors
            (fset:map (fset:$ node-colors)
                      (fset:$ (classify-by node-colorer graph)))))

    ;; optionally, add legend information

    (when *legend*
      (format t "Creating legend.~&")
      (when *node-fill-by*
        (add-legend-nodes node-filler graph)
        (add-legend-edges node-filler graph)
        (setf node-fills
              (fset:map
               (fset:$ node-fills)
               (fset:$ (legend-fill node-filler))))
        (when (and *shape-node-legend* (not (eq *node-shape-by* *node-fill-by*)))
          (setf node-shapes
                (fset:map
                 (fset:$ node-shapes)
                 (fset:$ (legend-shape node-filler :default t)))))
        (when (and *color-node-legend* (not (eq *node-color-by* *node-fill-by*)))
          (setf node-colors
                (fset:map
                 (fset:$ node-colors)
                 (fset:$ (legend-color node-filler :default t)))))
        (when *url-include*
          (setf node-urls
                (fset:map (fset:$ node-urls)
                          (fset:$ (legend-urls node-filler))))))

      (when *node-shape-by*
        (add-legend-nodes node-shaper graph)
        (add-legend-edges node-shaper graph)
        (setf node-shapes
              (fset:map
               (fset:$ node-shapes)
               (fset:$ (legend-shape node-shaper))))
        (when (and *color-fill-node-legend*
                   (not (eq *node-shape-by* *node-fill-by*)))
          (setf node-fills
                (fset:map
                 (fset:$ node-fills)
                 (fset:$ (legend-fill node-shaper :default t)))))
        (when (and *color-node-legend* (not (eq *node-shape-by* *node-color-by*)))
          (setf node-colors
                (fset:map
                 (fset:$ node-colors)
                 (fset:$ (legend-color node-shaper :default t)))))
        (when *url-include*
          (setf node-urls
                (fset:map (fset:$ node-urls)
                          (fset:$ (legend-urls node-shaper))))))

      (when *node-color-by*
        (add-legend-nodes node-colorer graph)
        (add-legend-edges node-colorer graph)
        (setf node-colors
              (fset:map
               (fset:$ node-colors)
               (fset:$ (legend-color node-colorer))))
        (when (and *color-fill-node-legend*
                   (not (eq *node-color-by* *node-fill-by*)))
          (setf node-fills
                (fset:map
                 (fset:$ node-fills)
                 (fset:$ (legend-fill node-colorer :default t)))))
        (when (and *shape-node-legend* (not (eq *node-color-by* *node-shape-by*)))
          (setf node-shapes
                (fset:map
                 (fset:$ node-shapes)
                 (fset:$ (legend-shape node-colorer :default t)))))
        (when *url-include*
          (setf node-urls
                (fset:map (fset:$ node-urls)
                          (fset:$ (legend-urls node-colorer)))))))

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
      (format t "Creating chronology graph.~&")
      (let ((counter -1))
        (setf chronology-graph (graph:populate (make-instance 'graph:digraph))
              distance-matrix (create-distance-matrix graph distance-matrix))
        (format t "Set distance matrix.~&")
        (mapc (lambda (node) (setf (gethash node node-index-hash) (incf counter)))
              (graph:nodes graph))
        (format t "Reading radiocarbon table.~&")
        (dolist (col event-table)
          (graph:add-node chronology-graph
                          (alexandria:symbolicate "alpha-" (second col)))
          (graph:add-node chronology-graph
                          (alexandria:symbolicate "beta-" (second col)))
          (graph:add-node chronology-graph
                          (alexandria:symbolicate "theta-" (first col)))
          (graph:add-edge chronology-graph
                          (list (alexandria:symbolicate "beta-" (second col))
                                (alexandria:symbolicate "alpha-" (second col)))
                          2)
          (push (new-symbol (second col)) context-list))
        (format t "Reading date order table.~&")
        (when event-order-table
          (dolist (pair event-order-table)
            (graph:add-edge chronology-graph
                            (list (alexandria:symbolicate "theta-" (second pair))
                                  (alexandria:symbolicate "theta-" (first pair)))
                            0)))
        (format t "Modeling radiocarbon dates.~&")
        (dolist (node event-table)
          (and (eq 0 (graph:indegree
                      chronology-graph
                      (alexandria:symbolicate "theta-" (first node))))
               (not (eq (new-symbol (fourth node)) (new-symbol "disparate")))
               (graph:add-edge chronology-graph
                               (list (alexandria:symbolicate "beta-" (second node))
                                     (alexandria:symbolicate "theta-" (first node)))
                               0))
          (and (eq 0 (graph:outdegree
                      chronology-graph
                      (alexandria:symbolicate "theta-" (first node))))
               (not (eq (new-symbol (fourth node)) (new-symbol "disjunct")))
               (graph:add-edge chronology-graph
                               (list
                                (alexandria:symbolicate "theta-" (first node))
                                (alexandria:symbolicate  "alpha-" (second node)))
                               0)))

        (setf context-list (append (unique-pairs context-list)
                                   (unique-pairs (reverse context-list))))
        (format t "Adding edge values to the chronology graph.~&")
        (dolist (pair context-list)
          (let ((distance (graph-matrix::matrix-ref
                           distance-matrix
                           (gethash (first pair) node-index-hash)
                           (gethash (second pair) node-index-hash))))
            (unless (graph-matrix:infinitep distance distance-matrix)
              (graph:add-edge
               chronology-graph
               (list (alexandria:symbolicate "alpha-" (first pair))
                     (alexandria:symbolicate "beta-" (second pair)))
               (if (eql 1 (round distance)) 1 2))))))

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
                        (color-filter *color-fill-graph-chronology*)
                        :quote t))
        (cons :fontname (format-attribute *font-name-graph-chronology*))
        (cons :fontsize (format-attribute *font-size-graph-chronology*))
        (cons :fontcolor (format-attribute
                          (color-filter *font-color-graph-chronology*)
                          :quote t))
        (cons :splines (format-attribute *graph-splines-chronology*))
        (cons :page (format-attribute *graph-page-chronology*))
        (cons :size (format-attribute *graph-size-chronology*))
        (cons :ratio (format-attribute *graph-ratio-chronology*))
        (cons :label (format-attribute *graph-title-chronology*
                                       :quote t :preserve-case t))
        (cons :labelloc (format-attribute *graph-labelloc-chronology*)))
       :edge-attrs
       (list
        (cons :style
              (lambda (e)
                (case (graph:edge-value chronology-graph e)
                  (0 (format-attribute *edge-date-chronology*))
                  (1 (format-attribute *edge-abutting-chronology*))
                  (2 (format-attribute *edge-separated-chronology*)))))
        (cons :label (lambda (x) (format-attribute nil)))
        (cons :arrowhead (lambda (x) (format-attribute *edge-arrowhead-chronology*)))
        (cons :colorscheme (lambda (x) (format-attribute *color-scheme-chronology*)))
        (cons :color (lambda (x) (format-attribute (color-filter *color-edge-chronology*)
                                              :quote t)))
        (cons :fontname (lambda (x) (format-attribute *font-name-edge-chronology*)))
        (cons :fontsize (lambda (x) (format-attribute *font-size-edge-chronology*)))
        (cons :fontcolor (lambda (x) (format-attribute
                                 (color-filter *font-color-edge-chronology*)
                                 :quote t))))
       :node-attrs
       (list
        (cons :label (lambda (x) (chronology-graph-html-label x *font-size-subscript*)))
        (cons :shape
              (lambda (n)
                (if (equal (char (symbol-name n) 0) #\t)
                    (format-attribute *shape-date*)
                    (format-attribute *shape-phase*))))
        (cons :style (lambda (x) (format-attribute *style-node-chronology*)))
        (cons :fontname (lambda (x) (format-attribute *font-name-node-chronology*)))
        (cons :fontsize (lambda (x) (format-attribute *font-size-node-chronology*)))
        (cons :colorscheme (lambda (x) (format-attribute *color-scheme-chronology*)))
        (cons :color (lambda (x) (format-attribute
                             (color-filter *color-node-chronology*) :quote t)))
        (cons :fillcolor (lambda (x) (format-attribute
                                 (color-filter *color-fill-node-chronology*)
                                 :quote t)))
        (cons :fontcolor (lambda (x) (format-attribute
                                 (color-filter *font-color-node-chronology*)
                                 :quote t)))))
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
      (cons :URL (format-attribute
                  (if *url-include* (url-decode *url-default*) "")
                  :preserve-case t :quote t))
      (cons :margin (format-attribute *graph-margin-sequence*))
      (cons :bgcolor (format-attribute (color-filter *color-fill-graph-sequence*)
                                       :quote t))
      (cons :fontname (format-attribute *font-name-graph-sequence*))
      (cons :fontsize (format-attribute *font-size-graph-sequence*))
      (cons :fontcolor (format-attribute
                        (color-filter *font-color-graph-sequence*) :quote t))
      (cons :splines (format-attribute *graph-splines-sequence*))
      (cons :page (format-attribute *graph-page-sequence*))
      (cons :size (format-attribute *graph-size-sequence*))
      (cons :ratio (format-attribute *graph-ratio-sequence*))
      (cons :label (format-attribute *graph-title-sequence* :preserve-case t
                                     :quote t))
      (cons :labelloc (format-attribute *graph-labelloc-sequence*)))
     :edge-attrs
     (list
      (cons :style (lambda (x) (format-attribute *style-edge-sequence*)))
      (cons :arrowhead (lambda (x) (format-attribute *edge-arrowhead-sequence*)))
      (cons :colorscheme (lambda (x) (format-attribute *color-scheme-sequence*)))
      (cons :color (lambda (x) (format-attribute (color-filter *color-edge-sequence*)
                                            :quote t)))
      (cons :fontname (lambda (x) (format-attribute *font-name-edge-sequence*)))
      (cons :fontsize (lambda (x) (format-attribute *font-size-edge-sequence*)))
      (cons :fontcolor (lambda (x) (format-attribute
                               (color-filter *font-color-edge-sequence*) :quote t)))
      (cons :URL (lambda (x) (format-attribute
                         (if *url-include* (url-decode (fset:@ arc-urls x)) "")
                         :preserve-case t :quote t))))
     :node-attrs
     (list
      (cons :shape (lambda (x) (format-attribute
                           (if *node-shape-by* (shape-filter (fset:@ node-shapes x))
                               *shape-node-deposit*))))
      (cons :style (lambda (x) (format-attribute *style-node-sequence*)))
      (cons :fontname (lambda (x) (format-attribute *font-name-node-sequence*)))
      (cons :fontsize (lambda (x) (format-attribute *font-size-node-sequence*)))
      (cons :colorscheme (lambda (x) (format-attribute *color-scheme-sequence*)))
      (cons :color (lambda (x) (format-attribute
                           (if *node-color-by*
                               (color-filter (fset:@ node-colors x))
                               *color-node-sequence*) :quote t)))
      (cons :fillcolor (lambda (x) (format-attribute
                               (if *node-fill-by*
                                   (color-filter (fset:@ node-fills x))
                                   *color-fill-node-sequence*) :quote t)))
      (cons :fontcolor (lambda (x) (format-attribute
                               (if *node-fill-by*
                                   (label-color (fset:@ node-fills x))
                                   *font-color-node-sequence*) :quote t)))
      (cons :penwidth (lambda (x) (format-attribute *node-outline-width*)))
      (cons :URL (lambda (x) (format-attribute
                         (if *url-include*
                             (url-decode (fset:@ node-urls x))
                             "") :preserve-case t :quote t)))))
    (format t "Wrote ~a"
            (probe-file (string-downcase (string *output-file-sequence*))))))
