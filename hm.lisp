;;; hm.lisp

;; Copyright (C) Thomas Dye 2014

;; Licensed under the Gnu Public License Version 3 or later

(in-package #:hm)

(require "graph-dot")
(require "graph-matrix")
(require "cl-csv")
(require "do-urlencode")

;; declare global variables that will be set by the configuration file

(defparameter *out-file* nil
  "Output file path for the sequence diagram dot file.")
(defparameter *chronology-out-file* nil
  "Output file path for the chronology diagram dot file.")
(defparameter *merged-out-file* nil
  "Output file path for the merged sequence diagram dot file.")
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
  "Fill nodes according to some procedure; nil for no procedure, or one of 'levels',
  'reachable', 'periods', 'phases', 'connected', or 'distance'.")
(defparameter *label-break* nil
  "Switch from dark to light label color; 'nil' for no switch, or a
  number corresponding to the highest level node that is labeled with
  a dark color--higher level nodes will be light-colored.")
(defparameter *color-scheme* nil
  "Either 'nil' for no color scheme, or the name of a Brewer color
  scheme used by GraphViz dot.")
(defparameter *color-space* nil
  "Choose the color space for color names; 'nil' for the GraphViz dot
  default, or one of 'x11' or 'svg'.")
(defparameter *label-color-dark* nil
  "The dark color for labels used with nodes having a light fill
  color; either a color name from the active color space, or an integer
  index into a Brewer color scheme.")
(defparameter *label-color-light* nil
  "The light color for labels used with nodes having a dark fill
  color; either a color name from the active color space, or an integer
  index into a Brewer color scheme.")
(defparameter *reachable-from* nil
  "The label of the start node for calculating reachability.")
(defparameter *reachable-limit* nil
  "A number that determines the scope of the reachability calculation;
  < 0 for all reachable nodes, 0 for the start node only, 1 for
  adjacent nodes, etc.")
(defparameter *reachable-color* nil
  "The fill color for reachable nodes; either a color name from the
  active color space, or an integer index into a Brewer color scheme.")
(defparameter *reachable-not-color* nil
  "The fill color for unreachable nodes; either a color name from the
  active color space, or an integer index into a Brewer color scheme.")
(defparameter *origin-color* nil
  "The fill color for the origin node; either a color name from the
  active color space, or an integer index into a Brewer color scheme.")
(defparameter *adjacent-color* nil
  "The fill color for nodes adjacent to the origin node; either a
  color name from the active color space, or an integer index into a
  Brewer color scheme.")
(defparameter *url-include* nil
  "Switch for URL information for nodes and edges; 'nil' for no URL
  information, non-nil otherwise.")
(defparameter *url-default* nil
  "A default URL to use for nodes and edges if specific URL
  information is absent.")
(defparameter *graph-title* nil
  "An optional title for the graph.")
(defparameter *graph-labelloc* nil
  "The location of the title, either non-nil for centered or nil for
  not centered.")
(defparameter *graph-style* nil
  "A valid argument for the GraphViz dot style attribute for a graph.")
(defparameter *graph-size* nil
  "The maximum size of the graph in inches; either one value that
  serves as both width and height, or two values separated by a
  comma.  Leave this nil for the GraphViz dot default.")
(defparameter *graph-ratio* nil
  "A valid argument for the GraphViz dot ratio attribute for a graph;
  typically a numeric value indicating the aspect ratio, but also
  various strings that trigger complex responses.")
(defparameter *graph-page* nil
  "The maximum size of the page in inches; either one value that
  serves as both width and height, or two values separated by a
  comma.  Leave this nil for the GraphViz dot default.")
(defparameter *graph-dpi* nil
  "Specify the expected number of pixels per inch on a display device.
  Leave this nil for the GraphViz dot default.")
(defparameter *graph-margin* nil
  "The x and y margins of the graph in inches; either one value that
  serves as both x and y, or two values separated by a comma.  Leave
  this nil for the GraphViz dot default.")
(defparameter *graph-font-name* nil
  "The font used to render text associated with the graph (rather than
  the nodes and edges). The possibilities here are device dependent.
  The values 'times' and 'helvetica' select Roman and sans-serif
  fonts, respectively, on most platforms.")
(defparameter *graph-font-size* nil
  "Font size in points.")
(defparameter *graph-font-color* nil
  "The color of text associated with the graph (rather than nodes and
  edges). Either a color name from the active color space or an
  integer index into a Brewer color scheme.")
(defparameter *graph-splines* nil
  "Controls how, and if, edges are drawn; one of 'none' for no edges,
  'line' or 'false' for straight line edges, 'polyline' for straight
  line edges that avoid nodes, 'curved' for curved edges, 'ortho' for
  orthogonal edges, and 'spline' or 'true' for edges that avoid
  nodes.")
(defparameter *graph-bg-color* nil
  "One of 'transparent', a color name from the active color space, or
  an integer index into a Brewer color scheme.")
(defparameter *legend* nil
  "A switch for creating a rudimentary legend; non-nil to include a
  legend, nil otherwise.")
(defparameter *legend-node-shape* nil
  "A valid GraphViz dot node shape for node entries in the legend.")
(defparameter *edge-style* nil
  "A valid GraphViz dot style attribute for edges.")
(defparameter *edge-with-arrow* nil
  "A valid GraphViz dot arrowType attribute.")
(defparameter *edge-color* nil
  "The color of edges. Either a color name from the active color space
  or an integer index into a Brewer color scheme.")
(defparameter *edge-font-name* nil
  "The font used to render edge labels. The possibilities here are
  device dependent.  The values 'times' and 'helvetica' select Roman
  and sans-serif fonts, respectively, on most platforms.")
(defparameter *edge-font-size* nil
  "Font size in points of edge labels.")
(defparameter *edge-font-color* nil
  "The color of text for edge labels. Either a color name from the
  active color space or an integer index into a Brewer color scheme.")
(defparameter *node-style* nil
  "A valid GraphViz dot style attribute for nodes.")
(defparameter *node-color* nil
  "The color of the node outline.  Either a color name from the active
  color space or an integer index into a Brewer color scheme.")
(defparameter *node-font-name* nil
  "The font used to render node labels. The possibilities here are
  device dependent.  The values 'times' and 'helvetica' select Roman
  and sans-serif fonts, respectively, on most platforms.")
(defparameter *node-font-size* nil
  "Font size in points of node labels.")
(defparameter *node-font-color* nil
  "The color used to render node labels.  Either a color name from the
  active color space or an integer index into a Brewer color scheme.")
(defparameter *node-shape-interface* nil
  "A valid GraphViz dot node shape for interfacial contexts.")
(defparameter *node-shape-deposit* nil
  "A valid GraphViz dot node shape for depositional contexts.")
(defparameter *node-fill* nil
  "The color used to fill nodes.  Either a color name from the active
  color space or an integer index into a Brewer color scheme.")
(defparameter *assume-correlations-true* nil
  "A switch to toggle between a directed graph based solely on
  observations and one based on observations and inferences; nil for a
  directed graph based on observations and non-nil for a directed
  graph based on observations and inferences.")
(defparameter *chronology-color-scheme* nil
  "Either 'nil' for no color scheme, or the name of a Brewer color
  scheme used by GraphViz dot.")
(defparameter *chronology-graph-title* nil
  "An optional title for the graph.")
(defparameter *chronology-graph-labelloc* nil
  "The location of the title, either non-nil for centered or nil for
  not centered.")
(defparameter *chronology-graph-style* nil
  "A valid argument for the GraphViz dot style attribute for a graph.")
(defparameter *chronology-graph-size* nil
  "The maximum size of the graph in inches; either one value that
  serves as both width and height, or two values separated by a
  comma.  Leave this nil for the GraphViz dot default.")
(defparameter *chronology-graph-ratio* nil
  "A valid argument for the GraphViz dot ratio attribute for a graph;
  typically a numeric value indicating the aspect ratio, but also
  various strings that trigger complex responses.")
(defparameter *chronology-graph-page* nil
  "The maximum size of the page in inches; either one value that
  serves as both width and height, or two values separated by a
  comma.  Leave this nil for the GraphViz dot default.")
(defparameter *chronology-graph-dpi* nil
  "Specify the expected number of pixels per inch on a display device.
  Leave this nil for the GraphViz dot default.")
(defparameter *chronology-graph-margin* nil
  "The x and y margins of the graph in inches; either one value that
  serves as both x and y, or two values separated by a comma.  Leave
  this nil for the GraphViz dot default.")
(defparameter *chronology-graph-font-name* nil
  "The font used to render text associated with the graph (rather than
  the nodes and edges). The possibilities here are device dependent.
  The values 'times' and 'helvetica' select Roman and sans-serif
  fonts, respectively, on most platforms.")
(defparameter *chronology-graph-font-size* nil
  "Font size in points.")
(defparameter *chronology-graph-font-color* nil
  "The color of text associated with the graph (rather than nodes and
  edges). Either a color name from the active color space or an
  integer index into a Brewer color scheme.")
(defparameter *chronology-graph-splines* nil
  "Controls how, and if, edges are drawn; one of 'none' for no edges,
  'line' or 'false' for straight line edges, 'polyline' for straight
  line edges that avoid nodes, 'curved' for curved edges, 'ortho' for
  orthogonal edges, and 'spline' or 'true' for edges that avoid
  nodes.")
(defparameter *chronology-graph-bg-color* nil
  "One of 'transparent', a color name from the active color space, or
  an integer index into a Brewer color scheme.")
(defparameter *chronology-edge-with-arrow* nil
  "A valid GraphViz dot arrowType attribute.")
(defparameter *chronology-edge-color* nil
  "The color of edges. Either a color name from the active color space
  or an integer index into a Brewer color scheme.")
(defparameter *chronology-edge-font-name* nil
  "The font used to render edge labels. The possibilities here are
  device dependent.  The values 'times' and 'helvetica' select Roman
  and sans-serif fonts, respectively, on most platforms.")
(defparameter *chronology-edge-font-size* nil
  "Font size in points of edge labels.")
(defparameter *chronology-edge-font-color* nil
  "The color of text for edge labels. Either a color name from the
  active color space or an integer index into a Brewer color scheme.")
(defparameter *chronology-edge-date* nil
  "A valid GraphViz dot style attribute for an edge that connects to a date.")
(defparameter *chronology-edge-abutting* nil
  "A valid GraphViz dot style attribute for an edge that connects abutting phases.")
(defparameter *chronology-edge-separated* nil
  "A valid GraphViz dot style attribute for an edge that connects
separated phases.")
(defparameter *chronology-node-style* nil
  "A valid GraphViz dot style attribute for nodes.")
(defparameter *chronology-node-color* nil
  "The color of the node outline.  Either a color name from the active
  color space or an integer index into a Brewer color scheme.")
(defparameter *chronology-node-font-name* nil
  "The font used to render node labels. The possibilities here are
  device dependent.  The values 'times' and 'helvetica' select Roman
  and sans-serif fonts, respectively, on most platforms.")
(defparameter *chronology-node-font-size* nil
  "Font size in points of node labels.")
(defparameter *chronology-node-font-color* nil
  "The color used to render node labels.  Either a color name from the
  active color space or an integer index into a Brewer color scheme.")
(defparameter *chronology-node-shape-phase* nil
  "A valid GraphViz dot node shape for interfacial contexts.")
(defparameter *chronology-node-shape-date* nil
  "A valid GraphViz dot node shape for depositional contexts.")
(defparameter *chronology-node-fill* nil
  "The color used to fill nodes.  Either a color name from the active
  color space or an integer index into a Brewer color scheme.")
(defparameter *use-fast-matrix* t
  "Toggle fast matrix routines.")

;; global variables not set by the configuration file

(defparameter *ranks* nil
  "A list of graph:rank structures.")

(defparameter *distance-matrix* nil
  "Distance matrix of graph.")

;; function definitions

(defun initialize-special-variables ()
  (setf *out-file* nil
        *chronology-out-file* nil
        *merged-out-file* nil
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
        *label-break* nil
        *color-scheme* nil
        *color-space* nil
        *label-color-dark* nil
        *label-color-light* nil
        *reachable-from* nil
        *reachable-limit* nil
        *reachable-color* nil
        *reachable-not-color* nil
        *origin-color* nil
        *adjacent-color* nil
        *url-include* nil
        *url-default* nil
        *graph-title* nil
        *graph-labelloc* nil
        *graph-style* nil
        *graph-size* nil
        *graph-ratio* nil
        *graph-page* nil
        *graph-dpi* nil
        *graph-margin* nil
        *graph-font-name* nil
        *graph-font-size* nil
        *graph-font-color* nil
        *graph-splines* nil
        *graph-bg-color* nil
        *legend* nil
        *legend-node-shape* nil
        *edge-style* nil
        *edge-with-arrow* nil
        *edge-color* nil
        *edge-font-name* nil
        *edge-font-size* nil
        *edge-font-color* nil
        *node-style* nil
        *node-color* nil
        *node-font-name* nil
        *node-font-size* nil
        *node-font-color* nil
        *node-shape-interface* nil
        *node-shape-deposit* nil
        *node-fill* nil
        *assume-correlations-true* nil
        *chronology-color-scheme* nil
        *chronology-graph-title* nil
        *chronology-graph-labelloc* nil
        *chronology-graph-style* nil
        *chronology-graph-size* nil
        *chronology-graph-ratio* nil
        *chronology-graph-page* nil
        *chronology-graph-dpi* nil
        *chronology-graph-margin* nil
        *chronology-graph-font-name* nil
        *chronology-graph-font-size* nil
        *chronology-graph-font-color* nil
        *chronology-graph-splines* nil
        *chronology-graph-bg-color* nil
        *chronology-edge-with-arrow* nil
        *chronology-edge-color* nil
        *chronology-edge-font-name* nil
        *chronology-edge-font-size* nil
        *chronology-edge-font-color* nil
        *chronology-edge-date* nil
        *chronology-edge-abutting* nil
        *chronology-edge-separated* nil
        *chronology-node-style* nil
        *chronology-node-color* nil
        *chronology-node-font-name* nil
        *chronology-node-font-size* nil
        *chronology-node-font-color* nil
        *chronology-node-shape-phase* nil
        *chronology-node-shape-date* nil
        *chronology-node-fill* nil
        *use-fast-matrix* t
        *ranks* nil
        *distance-matrix* nil))

(defun hm-read-cnf-file (name)
  "Read csv file specified by NAME and set most global variables to
symbols, except strings that appear in the graph and must preserve
case.  URL's are encoded so they can be represented as symbols.
Return t if successful, nil otherwise."
  (alexandria:when-let (in-file (probe-file name))
    (format t "Reading configuration file ~a~%" in-file)
    (cl-csv:do-csv (row in-file)
      (let ((global-var (first row)))
        (setf
         (symbol-value (read-from-string global-var))
         (cond
           ((or (string= global-var "*graph-title*")
                (string= global-var "*chronology-graph-title*"))
            (second row))
           ((string= global-var "*url-default*")
            (new-symbol (do-urlencode:urlencode (second row))))
           (t (new-symbol (second row)))))))
    t))

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

(defun node-fill-by-table (table contexts)
  "Set the fill of nodes in CONTEXTS according to values recorded in TABLE."
  (let ((ht (make-hash-table))
        (ret (make-hash-table)))
    (mapcar #'(lambda (x)
                (setf (gethash (new-symbol (first x)) ht)
                      (new-symbol (third x))))
            table)
    (mapcar #'(lambda (x)
                (setf (gethash (new-symbol (first x)) ret)
                      (gethash (new-symbol (fourth x)) ht)))
            contexts)
    ret))

(defun node-fill-by-reachable (graph)
  "Use the reachability matrix of GRAPH to set node fills."
  (let ((ret (make-hash-table))
        (reachable-list
         (cond
           ((< *reachable-limit* 0)
            (graph-matrix:reachable-from
             graph (graph-matrix:to-reachability-matrix
                    graph (new-matrix *use-fast-matrix*))
             *reachable-from*))
           ((eq *reachable-limit* 0) (list *reachable-from*))
           ((eq *reachable-limit* 1)
            (cons *reachable-from*
                  (graph-matrix:reachable-from
                   graph (graph-matrix:to-adjacency-matrix
                    graph (new-matrix *use-fast-matrix*))
                   *reachable-from*)))
           (t (graph-matrix:reachable-from
               graph (graph-matrix:to-reachability-matrix
                      graph (new-matrix *use-fast-matrix*)
                      :limit *reachable-limit*)
               *reachable-from*)))))
    (dolist (node reachable-list)
      (setf (gethash node ret) *reachable-color*))
    (dolist (node (set-difference (graph:nodes graph) reachable-list))
      (setf (gethash node ret) *reachable-not-color*))
    (setf (gethash *reachable-from* ret) *origin-color*)
    ret))

(defun node-fill-by-connected (graph)
  "Use the connected component of GRAPH that includes the node
*reachable-from* to set node fills. Note that the component is
unilaterally connected.  Unilaterally connected component partitions
may not be well-defined, since it is possible for a given vertex to be
unilaterally connected to two vertices which are not unilaterally
connected with one another."
  (let ((ret (make-hash-table))
        (reachable-list (graph:connected-component graph *reachable-from*
                                                   :type :unilateral)))
    (dolist (node reachable-list)
      (setf (gethash node ret) *reachable-color*))
    (dolist (node (set-difference (graph:nodes graph) reachable-list))
      (setf (gethash node ret)*reachable-not-color*))
    ret))

(defun node-fill-by-distance (graph)
  "Use the distance matrix of GRAPH to set node fills."
  (let ((ret (make-hash-table))
        (d))
    (unless *distance-matrix*
      (setf *distance-matrix* (graph-matrix:to-distance-matrix
                               graph (new-matrix *use-fast-matrix*))))
    (mapc (lambda (node)
            (setf d (min (graph-matrix:distance-from-to
                          graph *distance-matrix* *reachable-from* node)
                         (graph-matrix:distance-from-to
                          graph *distance-matrix* node *reachable-from*))
                  (gethash node ret)
                  (cond
                    ((equal d graph-matrix:infinity) *reachable-not-color*)
                    ((equal d 0) *origin-color*)
                    ((equal d 1) *adjacent-color*)
                    (t *reachable-color*))))
          (graph:nodes graph))
    ret))

(defun set-same-ranks (table)
  "Use the values in TABLE to make a list of graph:rank structures
where the indicated nodes are to appear on the same rank of the graph
picture."
  (mapcar #'(lambda (x)
              (push (graph-dot::make-rank
                     :value "same"
                     :node-list (list (format nil "~s" (new-symbol (first x)))
                                      (format nil "~s" (new-symbol (second x)))))
                    *ranks*))
          table))

(defun set-other-ranks (table)
  "Use the values in TABLE to make a list of graph:rank structures
where the indicated nodes either appear at the top or the bottom of
the graph picture."
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
                      *ranks*)))
          table))

(defun set-node-shapes (table inferences)
  "Set node shapes for depositional and interfacial contexts listed in
TABLE and in INFERENCES.  The nodes created from information in
INFERENCES are assigned a shape based on the value in TABLE for the
first part of the inference.  Returns a hash table where the node
labels are keys and the values are symbols for node shapes."
  (let ((ret (make-hash-table)))
    (mapcar #'(lambda (x)
                (setf (gethash (new-symbol (first x)) ret)
                      (cond ((eq 'deposit (new-symbol (second x)))
                             *node-shape-deposit*)
                            ((eq 'interface (new-symbol (second x)))
                             *node-shape-interface*))))
            table)
    (when *assume-correlations-true*
      (dolist (part inferences)
        (setf (gethash (read-from-string
                        (format nil "~a=~a" (first part) (second part))) ret)
              (gethash (new-symbol (first part)) ret))))
    ret))

(defun get-node-urls-from (table inferences)
  "Reads URL's from TABLE.  Returns a hash table where the keys are
node labels and the values are symbols for URL's."
  (let ((ret (make-hash-table)))
    (dolist (node table)
      (setf (gethash (new-symbol (first node)) ret)
            (if (string= (sixth node) "")
                (if *url-default* *url-default* "")
                (sixth node))))
    (when *assume-correlations-true*
      (dolist (part inferences)
        (setf (gethash (read-from-string
                        (format nil "~a=~a" (first part) (second part))) ret)
              (if (string= (third part) "")
                  (if *url-default*  *url-default* "")
                  (third part)))))
    ret))

(defun get-arc-urls-from (table)
  "Reads URL's from TABLE.  Returns a hash table where the keys are
arcs and the values are URL's."
  (let ((ret (make-hash-table :test #'equal)))
    (dolist (arc table)
      (setf (gethash (list (new-symbol (first arc))
                           (new-symbol (second arc))) ret)
            (if (string= (third arc) "")
                (if *url-default* *url-default* "")
                (third arc))))
    ret))

(defun make-legend-for (table graph nodes units urls)
  "Make legend components for the node classification in TABLE."
  (mapcar #'(lambda (x)
              (setf (gethash (new-symbol (second x)) nodes)
                    (new-symbol (third x)))
              (when *symbolize-unit-type*
                (setf (gethash (new-symbol (second x)) units)
                      *legend-node-shape*))
              (when *url-include*
                (setf (gethash (new-symbol (second x)) urls)
                      (fourth x)))
              (push
               (graph-dot::make-rank
                :value "sink"
                :node-list (list (new-symbol (second x))))
               *ranks*)
              (graph:add-node graph (new-symbol (second x))))
          table))

(defun make-reachable-legend (graph nodes units urls)
  "Make legend components when nodes are filled by reachability."
  (setf (gethash 'reachable nodes) *reachable-color*
        (gethash 'not-reachable nodes) *reachable-not-color*
        (gethash 'origin nodes) *origin-color*)
  (when *symbolize-unit-type*
    (setf (gethash 'origin units) *legend-node-shape*
          (gethash 'reachable units) *legend-node-shape*
          (gethash 'not-reachable units) *legend-node-shape*))
  (when *url-include*
    (setf (gethash 'origin urls) (if *url-default* *url-default* "")
          (gethash 'reachable urls) (if *url-default* *url-default* "")
          (gethash 'not-reachable urls) (if *url-default* *url-default* "")))
  (push (graph-dot::make-rank
         :value "sink"
         :node-list (list (format nil "~s" 'reachable)
                          (format nil "~s" 'not-reachable)
                          (format nil "~s" 'origin)))
        *ranks*)
  (graph:add-node graph 'reachable)
  (graph:add-node graph 'not-reachable)
  (graph:add-node graph 'origin))

(defun make-distance-legend (graph nodes units urls)
  "Make legend components when nodes are filled by distance from an origin node."
  (setf (gethash 'separated nodes) *reachable-color*
        (gethash 'not-reachable nodes) *reachable-not-color*
        (gethash 'origin nodes) *origin-color*
        (gethash 'abutting nodes) *adjacent-color*)
  (when *symbolize-unit-type*
    (setf (gethash 'origin units) *legend-node-shape*
          (gethash 'abutting units) *legend-node-shape*
          (gethash 'separated units) *legend-node-shape*
          (gethash 'not-reachable units) *legend-node-shape*))
  (when *url-include*
    (setf (gethash 'origin urls) (if *url-default* *url-default* "")
          (gethash 'abutting urls) (if *url-default* *url-default* "")
          (gethash 'reachable urls) (if *url-default* *url-default* "")
          (gethash 'not-reachable urls) (if *url-default* *url-default* "")))
  (push (graph-dot::make-rank
         :value "sink"
         :node-list (list (format nil "~s" 'separated)
                          (format nil "~s" 'not-reachable)
                          (format nil "~s" 'origin)
                          (format nil "~s" 'abutting)))
        *ranks*)
  (graph:add-node graph 'separated)
  (graph:add-node graph 'not-reachable)
  (graph:add-node graph 'origin)
  (graph:add-node graph 'abutting))

(defun pair-with (elem list)
  (mapcar (lambda (a) (list elem a)) list))

(defun unique-pairs (list)
  (mapcon (lambda (rest) (pair-with (first rest) (rest rest)))
          (remove-duplicates list)))

(defun hm-read-table (name header)
  "Checks that NAME is a file, then attempts to read it as
comma-separated values.  HEADER indicates whether or not the first
line of NAME contains column heads, rather than values."
  (alexandria:when-let (in-file (probe-file (string name)))
    (format t "Reading table ~a~%" in-file)
    (cl-csv:read-csv in-file :skip-first-p header)))

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
  (do-urlencode:urldecode (string url)))

(defun label-color (color)
  (if (and *label-break* (integerp color))
      (if (<= *label-break* color)
          (color-filter *label-color-light*) (color-filter *label-color-dark*))
      (color-filter *label-color-dark*)))

(defun graph-element-control
    (default &optional (pre-process #'identity) switch hash-table)
  "A template that connects a hash table with a graph element, either
a node or an arc.  SWITCH determines whether to use a value from
HASH-TABLE or DEFAULT.  PRE-PROCESS is an optional function with one
argument that is used to process either the HASH-TABLE result or the
DEFAULT value.  The default PRE-PROCESS function, IDENTITY, simply
passes through the value passed to it."
  (if (and switch hash-table)
      (lambda (x) (format-attribute (funcall pre-process (gethash x hash-table))))
      (constantly-format (funcall pre-process default))))

(defun hm-draw (cnf-file-path &optional verbose)
  "Read a configuration file and various data files, create a
stratigraphy graph and optionally a chronology graph, and write one or
more dot files according to the variables contained in the
configuration file."
  (let ((rejected)
        (node-fills)
        (unit-types)
        (arc-urls)
        (node-urls)
        (graph (graph:populate (make-instance 'graph:digraph)))
        (chronology-graph)
        (context-table)
        (observation-table)
        (inference-table)
        (period-table)
        (phase-table)
        (radiocarbon-table)
        (date-order-table)
        (context-list)
        (adjacency-matrix)
        (reachability-matrix)
        (node-index-hash (make-hash-table))
        (counter -1))

    (initialize-special-variables)
    
    (when (and *create-chronology-graph* *assume-correlations-true*)
      (return-from hm-draw
        "Cannot create chronology graph when *assume-correlations-true*"))

    ;; read configuration file

    (unless (hm-read-cnf-file cnf-file-path)
      (return-from hm-draw
        (format nil "Unable to read configuration file ~a" cnf-file-path)))

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
        (set-same-ranks inference-table))

    (set-other-ranks context-table)

    ;; optionally, fill nodes

    (when *node-fill-by*
      (setf node-fills
            (cond ((eq *node-fill-by* 'levels)
                   (graph:levels graph))
                  ((eq *node-fill-by* 'periods)
                   (node-fill-by-table period-table context-table))
                  ((eq *node-fill-by* 'phases)
                   (node-fill-by-table phase-table context-table))
                  ((and (eq *node-fill-by* 'reachable) *reachable-from*)
                   (node-fill-by-reachable graph))
                  ((and (eq *node-fill-by* 'connected) *reachable-from*)
                   (node-fill-by-connected graph))
                  ((and (eq *node-fill-by* 'distance) *reachable-from*)
                   (node-fill-by-distance graph))
                  (t (return-from hm-draw
                       (format t "Incorrect *node-fill-by* value: ~a"
                               *node-fill-by*)))))
      (when verbose (format t "~d nodes filled~%" (hash-table-count node-fills))))
    

    ;; optionally, set node shapes

    (when *symbolize-unit-type*        
      (setf unit-types (set-node-shapes context-table inference-table))
      (when verbose (format t "~d node shapes~%" (hash-table-count unit-types))))

    ;; optionally, add url information

    (when *url-include*                
      (setf node-urls (get-node-urls-from context-table inference-table)
            arc-urls (get-arc-urls-from observation-table))
      (when verbose (format t "~d node and ~d arc urls added~%"
                            (hash-table-count node-urls)
                            (hash-table-count arc-urls))))

    ;; optionally, construct a legend

    (when *legend*
      (cond ((eq *node-fill-by* 'periods)
             (make-legend-for period-table graph node-fills
                              unit-types node-urls))
            ((eq *node-fill-by* 'phases)
             (make-legend-for phase-table graph node-fills
                              unit-types node-urls))
            ((and *reachable-from* (eq *node-fill-by* 'reachable))
             (make-reachable-legend graph node-fills
                                    unit-types node-urls))
            ((and *reachable-from* (eq *node-fill-by* 'distance))
             (make-distance-legend graph node-fills
                                   unit-types node-urls)))
      (when verbose (format t "constructed a legend~%")))

    ;; optionally, construct the chronology graph

    (when *create-chronology-graph*
      (setf chronology-graph (graph:populate
                              (make-instance 'graph:digraph))
            adjacency-matrix (graph-matrix:to-adjacency-matrix
                              graph (new-matrix *use-fast-matrix*))
            reachability-matrix (graph-matrix:to-reachability-matrix
                                 graph (new-matrix *use-fast-matrix*)))
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
       chronology-graph (string-downcase (string *chronology-out-file*))
       :attributes
       (list
        (cons :style (format-attribute *chronology-graph-style*))
        (cons :colorscheme (format-attribute *chronology-color-scheme*))
        (cons :dpi (format-attribute *chronology-graph-dpi*))
        (cons :margin (format-attribute *chronology-graph-margin*))
        (cons :bgcolor (format-attribute
                        (color-filter *chronology-graph-bg-color*)))
        (cons :fontname (format-attribute *chronology-graph-font-name*))
        (cons :fontsize (format-attribute *chronology-graph-font-size*))
        (cons :fontcolor (format-attribute
                          (color-filter *chronology-graph-font-color*)))
        (cons :splines (format-attribute *chronology-graph-splines*))
        (cons :page (format-attribute *chronology-graph-page*))
        (cons :size (format-attribute *chronology-graph-size*))
        (cons :ratio (format-attribute *chronology-graph-ratio*))
        (cons :label (format-attribute *chronology-graph-title*))
        (cons :labelloc (format-attribute *chronology-graph-labelloc*)))
       :edge-attrs
       (list
        (cons :style
              (lambda (e)
                (case (graph:edge-value chronology-graph e)
                  (0 (format-attribute *chronology-edge-date*))
                  (1 (format-attribute *chronology-edge-abutting*))
                  (2 (format-attribute *chronology-edge-separated*)))))
        (cons :label (constantly-format ""))
        (cons :arrowhead (constantly-format *chronology-edge-with-arrow*))
        (cons :colorscheme (constantly-format *chronology-color-scheme*))
        (cons :color (constantly-format (color-filter *chronology-edge-color*)))
        (cons :fontname (constantly-format *chronology-edge-font-name*))
        (cons :fontsize (constantly-format *chronology-edge-font-size*))
        (cons :fontcolor (constantly-format
                          (color-filter *chronology-edge-font-color*))))
       :node-attrs
       (list
        (cons :shape
              (lambda (n)
                (if (equal (char (symbol-name n) 0) #\T)
                    (format-attribute *chronology-node-shape-date*)
                    (format-attribute *chronology-node-shape-phase*))))
        (cons :style (constantly-format *chronology-node-style*))
        (cons :fontname (constantly-format *chronology-node-font-name*))
        (cons :fontsize (constantly-format *chronology-node-font-size*))
        (cons :colorscheme (constantly-format *chronology-color-scheme*))
        (cons :color (constantly-format (color-filter *chronology-node-color*)))
        (cons :fillcolor (constantly-format (color-filter *chronology-node-fill*)))
        (cons :fontcolor (constantly-format
                          (color-filter *chronology-node-font-color*)))))
      (format t "Wrote ~a~%" (probe-file
                              (string-downcase (string *chronology-out-file*)))))
    
    ;; write the dot file for the sequence diagram

    (graph-dot:to-dot-file
     graph (string-downcase (string *out-file*))
     :ranks *ranks*
     :attributes
     (list
      (cons :style (format-attribute *graph-style*))
      (cons :colorscheme (format-attribute *color-scheme*))
      (cons :dpi (format-attribute *graph-dpi*))
      (cons :URL (format-attribute (url-decode *url-default*)))
      (cons :margin (format-attribute *graph-margin*))
      (cons :bgcolor (format-attribute (color-filter *graph-bg-color*)))
      (cons :fontname (format-attribute *graph-font-name*))
      (cons :fontsize (format-attribute *graph-font-size*))
      (cons :fontcolor (format-attribute (color-filter *graph-font-color*)))
      (cons :splines (format-attribute *graph-splines*))
      (cons :page (format-attribute *graph-page*))
      (cons :size (format-attribute *graph-size*))
      (cons :ratio (format-attribute *graph-ratio*))
      (cons :label (format nil "~s" *graph-title*))
      (cons :labelloc (format-attribute *graph-labelloc*)))
     :edge-attrs
     (list
      (cons :style (graph-element-control *edge-style*))
      (cons :arrowhead (constantly-format *edge-with-arrow*))
      (cons :colorscheme (constantly-format *color-scheme*))
      (cons :color (constantly-format (color-filter *edge-color*)))
      (cons :fontname (constantly-format *edge-font-name*))
      (cons :fontsize (constantly-format *edge-font-size*))
      (cons :fontcolor (constantly-format (color-filter *edge-font-color*)))
      (cons :URL (graph-element-control "" #'url-decode *url-include* arc-urls)))
     :node-attrs
     (list
      (cons :shape (graph-element-control *node-shape-deposit* #'identity
                                          *symbolize-unit-type* unit-types))
      (cons :style (constantly-format *node-style*))
      (cons :fontname (constantly-format *node-font-name*))
      (cons :fontsize (constantly-format *node-font-size*))
      (cons :colorscheme (constantly-format *color-scheme*))
      (cons :color (constantly-format (color-filter *node-color*)))
      (cons :fillcolor (graph-element-control *node-fill* #'color-filter
                                              *node-fill-by* node-fills))
      (cons :fontcolor (graph-element-control *node-font-color* #'label-color
                                              *node-fill-by* node-fills))
      (cons :URL (graph-element-control "" #'url-decode *url-include* node-urls))))
    (return-from hm-draw
      (format nil "Wrote ~a" (probe-file (string-downcase (string *out-file*)))))))

