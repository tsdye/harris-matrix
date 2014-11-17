;;;; hm.lisp

;; Copyright (C) Thomas Dye 2014

;; Licensed under the Gnu Public License Version 3 or later

(in-package #:hm)

(require "graph-dot")
(require "graph-matrix")
(require "cl-csv")

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

;; global variables not set by the configuration file
(defparameter *ranks* nil
  "A list of graph:rank structures.")

(defparameter *distance-matrix* nil
  "Distance matrix of graph.")

;; (defparameter *chronology-graph* nil
;;   "Debug a problem with the chronology graph.")


(defun hm-read-cnf-file (config-file-name)
  "Read csv file specified by CONFIG-FILE-NAME and set global
variables.  Return t if successful, nil otherwise."
  (when (probe-file config-file-name)
    (cl-csv:do-csv (row (probe-file config-file-name))
      (setf (symbol-value (read-from-string (first row)))
            (cond
              ((string= "reachable" (second row)) 'reachable)
              ((string= "periods" (second row)) 'periods)
              ((string= "phases" (second row)) 'phases)
              ((string= "levels" (second row)) 'levels)
              ((string= "connected" (second row)) 'connected)
              ((string= "distance" (second row)) 'distance)
              ((string= "t" (second row)) t)
              ((string= "nil" (second row)) nil)
              ((string= "" (second row)) nil)
              ((every #'float-char-p (second row))
               (read-from-string (second row)))
              (t (second row)))))
    t))

(defun color-filter (col extra-quotes)
  "Returns a valid Graphviz dot color designator. The result is either
an integer or a color name appended to a color space.  COL can either
be an integer, in which case Brewer colors are assumed, or a string
with a color name."
  (if (integerp col) col
      (if (eq (read-from-string col) 'transparent)
          col
          (if extra-quotes
              (format nil "\"/~a/~a\"" *color-space* col)
              (format nil "/~a/~a" *color-space* col)))))

(defun float-char-p (char)
  "Is CHAR plausibly part of a real number?"
  (or (digit-char-p char) (char= char #\,) (char= char #\.)))

(defun make-attribute (att)
  "A convenience function to transform a Lisp symbol into a GraphViz
dot attribute."
  (format nil "~(~s~)" (if att att "")))

(defun constantly-format (x)
  "A convenience function for situations where Lisp requires a
function to express a constant."
  (constantly (format nil "~s" (if x x ""))))

(defun node-fill-by-table (table contexts)
  "Set the fill of nodes in CONTEXTS according to values recorded in TABLE."
  (let ((ht (make-hash-table))
        (ret (make-hash-table)))
    (mapcar #'(lambda (x)
                (setf (gethash (read-from-string (first x)) ht)
                      (read-from-string (third x))))
            table)
    (mapcar #'(lambda (x)
                (setf (gethash (read-from-string (first x)) ret)
                      (gethash (read-from-string (fourth x)) ht)))
            contexts)
    ret))

(defun node-fill-by-reachable (graph)
  "Use the reachability matrix of GRAPH to set node fills."
  (let ((ret (make-hash-table))
        (reachable-list
         (cond
           ((< *reachable-limit* 0)
            (graph-matrix:reachable-from
             graph
             (graph-matrix:to-reachability-matrix
              graph
              (make-instance 'graph-matrix:fast-matrix))
             *reachable-from*))
           ((eql *reachable-limit* 0) (list *reachable-from*))
           ((eql *reachable-limit* 1)
            (cons *reachable-from*
                  (graph-matrix:reachable-from
                   graph
                   (graph-matrix:to-adjacency-matrix
                    graph
                    (make-instance 'graph-matrix:fast-matrix))
                   *reachable-from*)))
           (t (graph-matrix:reachable-from
               graph (graph-matrix:to-reachability-matrix
                      graph (make-instance 'graph-matrix:fast-matrix)
                      :limit *reachable-limit*)
               *reachable-from*)))))
    (dolist (node reachable-list)
      (setf (gethash (read-from-string (format nil "~a" node)) ret)
            (color-filter *reachable-color* nil)))
    (dolist (node (set-difference (graph:nodes graph) reachable-list))
      (setf (gethash (read-from-string (format nil "~a" node)) ret)
            (color-filter *reachable-not-color* nil)))
    (setf (gethash *reachable-from* ret) (color-filter *origin-color* nil))
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
      (setf (gethash (read-from-string (format nil "~a" node)) ret)
            (color-filter *reachable-color* nil)))
    (dolist (node (set-difference (graph:nodes graph) reachable-list))
      (setf (gethash (read-from-string (format nil "~a" node)) ret)
            (color-filter *reachable-not-color* nil)))
    ret))

(defun node-fill-by-distance (graph)
  "Use the distance matrix of GRAPH to set node fills."
  (let ((ret (make-hash-table))
        (d))
    (unless *distance-matrix*
      (setq *distance-matrix* (graph-matrix:to-distance-matrix
                               graph
                               (make-instance 'graph-matrix:fast-matrix))))
    (mapc (lambda (node)
            (setq d (min (graph-matrix:distance-from-to
                          graph *distance-matrix* *reachable-from* node)
                         (graph-matrix:distance-from-to
                          graph *distance-matrix* node *reachable-from*)))
            (setf
             (gethash (read-from-string (format nil "~a" node)) ret)
             (cond
               ((equal d graph-matrix:infinity)
                (color-filter *reachable-not-color* nil))
               ((equal d 0) (color-filter *origin-color* nil))
               ((equal d 1) (color-filter *adjacent-color* nil))
               (t (color-filter *reachable-color* nil)))))
          (graph:nodes graph))
    ret))

(defun set-same-ranks (table)
  "Use the values in TABLE to make a list of graph:rank structures
where the indicated nodes are to appear on the same rank of the graph
picture."
  (mapcar #'(lambda (x)
              (push (graph-dot::make-rank
                     :value "same"
                     :node-list (list (first x) (second x)))
                    *ranks*))
          table))

(defun set-other-ranks (table)
  "Use the values in TABLE to make a list of graph:rank structures
where the indicated nodes either appear at the top or the bottom of
the graph picture."
  (mapcar #'(lambda (x)
              (when (or (eq (read-from-string (third x)) 'basal)
                        (eq (read-from-string (third x)) 'surface))
                (push (graph-dot::make-rank
                       :value
                       (cond
                         ((eq (read-from-string (third x)) 'basal) "sink")
                         ((eq (read-from-string (third x)) 'surface) "source"))
                       :node-list
                       (list (if (stringp (first x))
                                 (read-from-string
                                  (format nil "~:@(~s~)" (first x)))
                                 (first x))))
                      *ranks*)))
          table))

(defun set-node-shapes (table inferences)
  "Set node shapes for depositional and interfacial contexts listed in
TABLE and in INFERENCES.  The nodes created from information in
INFERENCES are assigned a shape based on the value in TABLE for the
first part of the inference.  Returns a hash table where the node
labels are keys and the values are node shapes."
  (let ((ret (make-hash-table)))
    (mapcar #'(lambda (x)
                (setf (gethash (read-from-string (first x)) ret)
                      (cond ((eq 'deposit (read-from-string (second x)))
                             *node-shape-deposit*)
                            ((eq 'interface (read-from-string (second x)))
                             *node-shape-interface*))))
            table)
    (when *assume-correlations-true*
      (dolist (part inferences)
        (setf (gethash (read-from-string
                        (format nil "~a=~a" (first part) (second part))) ret)
              (gethash (read-from-string (first part)) ret))))
    ret))

(defun get-node-urls-from (table inferences)
  "Reads URL's from TABLE.  Returns a hash table where the keys are
node labels and the values are URL's."
  (let ((ret (make-hash-table)))
    (dolist (node table)
      (setf (gethash (read-from-string (first node)) ret)
            (if (string= (sixth node) "")
                (if *url-default* *url-default* "")
                (sixth node))))
    (when *assume-correlations-true*
      (dolist (part inferences)
        (setf (gethash (read-from-string
                        (format nil "~a=~a" (first part) (second part))) ret)
              (if (string= (third part) "")
                  (if *url-default* *url-default* "")
                  (third part)))))
    ret))

(defun get-arc-urls-from (table)
  "Reads URL's from TABLE.  Returns a hash table where the keys are
arcs and the values are URL's."
  (let ((ret (make-hash-table :test #'equal)))
    (dolist (arc table)
      (setf (gethash (list (read-from-string (first arc))
                           (read-from-string (second arc))) ret)
            (if (string= (third arc) "")
                (if *url-default* *url-default* "") (third arc))))
    ret))

(defun make-legend-for (table graph nodes units urls)
  "Make legend components for the node classification in TABLE."
  (mapcar #'(lambda (x)
              (setf (gethash (read-from-string (second x)) nodes)
                    (read-from-string (third x)))
              (when *symbolize-unit-type*
                (setf (gethash (read-from-string (second x)) units)
                      *legend-node-shape*))
              (when *url-include*
                (setf (gethash (read-from-string (second x)) urls)
                      (fourth x)))
              (push
               (graph-dot::make-rank
                :value "sink"
                :node-list (list (format nil "~s" (read-from-string (second x)))))
               *ranks*)
              (graph:add-node graph (read-from-string (second x))))
          table))

(defun make-reachable-legend (graph nodes units urls)
  "Make legend components when nodes are filled by reachability."
  (setf (gethash 'reachable nodes) *reachable-color*)
  (setf (gethash 'not-reachable nodes) *reachable-not-color*)
  (setf (gethash 'origin nodes) *origin-color*)
  (when *symbolize-unit-type*
    (setf (gethash 'origin units) *legend-node-shape*)
    (setf (gethash 'reachable units) *legend-node-shape*)
    (setf (gethash 'not-reachable units) *legend-node-shape*))
  (when *url-include*
    (setf (gethash 'origin urls) (if *url-default* *url-default* ""))
    (setf (gethash 'reachable urls) (if *url-default* *url-default* ""))
    (setf (gethash 'not-reachable urls) (if *url-default* *url-default* "")))
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
  (setf (gethash 'separated nodes) *reachable-color*)
  (setf (gethash 'not-reachable nodes) *reachable-not-color*)
  (setf (gethash 'origin nodes) *origin-color*)
  (setf (gethash 'abutting nodes) *adjacent-color*)
  (when *symbolize-unit-type*
    (setf (gethash 'origin units) *legend-node-shape*)
    (setf (gethash 'abutting units) *legend-node-shape*)
    (setf (gethash 'separated units) *legend-node-shape*)
    (setf (gethash 'not-reachable units) *legend-node-shape*))
  (when *url-include*
    (setf (gethash 'origin urls) (if *url-default* *url-default* ""))
    (setf (gethash 'abutting urls) (if *url-default* *url-default* ""))
    (setf (gethash 'reachable urls) (if *url-default* *url-default* ""))
    (setf (gethash 'not-reachable urls) (if *url-default* *url-default* "")))
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


(defun hm-draw (cnf-file-path)
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
    (when (and *create-chronology-graph* *assume-correlations-true*)
      (return-from hm-draw "The variable *assume-correlations-true* must be nil to create a chronology graph"))
    ;; read configuration file
    (if (not (hm-read-cnf-file cnf-file-path))
        (format t "Unable to read configuration file from ~a" cnf-file-path)
        (progn
          (format t "Read configuration file from ~a~%" cnf-file-path)
          ;; read required tables
          (if (and *context-table-name*
                   (probe-file *context-table-name*)
                   (setf context-table
                         (cl-csv:read-csv
                          (probe-file *context-table-name*)
                          :skip-first-p *context-table-header*)))
              (format t "Read context table from ~a~%" *context-table-name*)
              (return-from hm-draw (format nil "Unable to read ~a"
                                           *context-table-name*)))
          (if (and *observation-table-name*
                   (probe-file *observation-table-name*)
                   (setf observation-table
                         (cl-csv:read-csv
                          (probe-file *observation-table-name*)
                          :skip-first-p *observation-table-header*)))
              (format t "Read observation table from ~a~%" *observation-table-name*)
              (return-from hm-draw (format nil "Unable to read ~a"
                                           *observation-table-name*)))
          ;; read optional tables, if necessary
          (when *inference-table-name*
            (if (and (probe-file *inference-table-name*)
                     (setf inference-table
                           (cl-csv:read-csv
                            (probe-file *inference-table-name*)
                            :skip-first-p *inference-table-header*)))
                (format t "Read optional inference table from ~a~%"
                        *inference-table-name*)
                (return-from hm-draw (format nil "Unable to read optional file ~a"
                                             *inference-table-name*))))
          (when *period-table-name*
            (if (and (probe-file *period-table-name*)
                     (setf period-table
                           (cl-csv:read-csv
                            (probe-file *period-table-name*)
                            :skip-first-p *period-table-header*)))
                (format t "Read optional period table from ~a~%"
                        *period-table-name*)
                (return-from hm-draw (format nil "Unable to read optional file ~a"
                                             *period-table-name*))))
          (when *phase-table-name*
            (if (and (probe-file *phase-table-name*)
                     (setf phase-table
                           (cl-csv:read-csv
                            (probe-file *phase-table-name*)
                            :skip-first-p *phase-table-header*)))
                (format t "Read optional phase table from ~a~%" *phase-table-name*)
                (return-from hm-draw (format nil "Unable to read optional file ~a"
                                             *phase-table-name*))))
          (when (and *create-chronology-graph* *radiocarbon-table-name*)
            (if (and (probe-file *radiocarbon-table-name*)
                     (setf radiocarbon-table
                           (cl-csv:read-csv
                            (probe-file *radiocarbon-table-name*)
                            :skip-first-p *radiocarbon-table-header*)))
                (format t "Read optional radiocarbon table from ~a~%"
                        *radiocarbon-table-name*)
                (return-from hm-draw (format nil "Unable to read optional file ~a"
                                             *radiocarbon-table-name*))))
          (when (and *create-chronology-graph* *date-order-table-name*)
            (if (and (probe-file *date-order-table-name*)
                     (setf date-order-table
                           (cl-csv:read-csv
                            (probe-file *date-order-table-name*)
                            :skip-first-p *date-order-table-header*)))
                (format t "Read optional date order table from ~a~%"
                        *date-order-table-name*)
                (return-from hm-draw (format nil "Unable to read optional file ~a"
                                             *date-order-table-name*))))
          ;; create sequence diagram graph
          (dolist (node context-table)
            (graph:add-node graph (read-from-string (first node))))
          (dolist (arc observation-table rejected)
            (graph:add-edge graph
                            (list (read-from-string (first arc))
                                  (read-from-string (second arc))))
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
                   graph (read-from-string (second part))
                   (read-from-string (first part))
                   :new (read-from-string (format nil "~a=~a" (first part)
                                                  (second part)))))
                (when (graph:cycles graph)
                  (return-from hm-draw
                    (format t "Correlated contexts introduced a cycle."))))
              (set-same-ranks inference-table))

          (set-other-ranks context-table)
          ;; fill nodes
          (when *node-fill-by*
            (setq node-fills
                  (cond ((eq *node-fill-by* 'levels)
                         (graph:levels graph))
                        ((eq *node-fill-by* 'periods)
                         (node-fill-by-table period-table context-table))
                        ((eq *node-fill-by* 'periods)
                         (node-fill-by-table phase-table context-table))
                        ((and *reachable-from*
                              (eq *node-fill-by* 'reachable))
                         (node-fill-by-reachable graph))
                        ((and *reachable-from*
                              (eq *node-fill-by* 'connected))
                         (node-fill-by-connected graph))
                        ((and *reachable-from*
                              (eq *node-fill-by* 'distance))
                         (node-fill-by-distance graph))
                        (t (return-from hm-draw
                             (format t "Incorrect *node-fill-by* value: ~a"
                                     *node-fill-by*))))))
          (when *symbolize-unit-type*   ; node shapes
            (setq unit-types (set-node-shapes context-table inference-table)))
          (when *url-include*           ; add url information
            (setq node-urls (get-node-urls-from context-table inference-table))
            (setq arc-urls (get-arc-urls-from observation-table)))
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
                                         unit-types node-urls))))
          ;; optionally, construct the chronology graph
          (when *create-chronology-graph*
            (setf chronology-graph (graph:populate
                                    (make-instance 'graph:digraph)))
            (setf adjacency-matrix (graph-matrix:to-adjacency-matrix
                                    graph
                                    (make-instance 'graph-matrix:fast-matrix)))
            (setf reachability-matrix (graph-matrix:to-reachability-matrix
                                       graph
                                       (make-instance 'graph-matrix:fast-matrix)))
            (mapc (lambda (node) (setf (gethash node node-index-hash) (incf counter)))
                  (graph:nodes graph))
            (dolist (col radiocarbon-table)
              (graph:add-node chronology-graph
                              (read-from-string
                               (format nil "alpha-~a" (second col))))
              (graph:add-node chronology-graph
                              (read-from-string
                               (format nil "beta-~a" (second col))))
              (graph:add-node chronology-graph
                              (read-from-string
                               (format nil "theta-~a" (first col)))))
            (when date-order-table
              (dolist (pair date-order-table)
                (graph:add-edge chronology-graph
                                (list (read-from-string
                                       (format nil "theta-~a" (second pair)))
                                      (read-from-string
                                       (format nil "theta-~a" (first pair))))
                                0)))
            (dolist (node radiocarbon-table)
              (when (eq 0 (graph:indegree
                           chronology-graph
                           (read-from-string
                            (format nil "theta-~a" (first node)))))
                (graph:add-edge chronology-graph
                                (list (read-from-string
                                       (format nil "beta-~a" (second node)))
                                      (read-from-string
                                       (format nil "theta-~a" (first node))))
                                0))
              (when (eq 0 (graph:outdegree
                           chronology-graph
                           (read-from-string
                            (format nil "theta-~a" (first node)))))
                (graph:add-edge chronology-graph
                                (list (read-from-string
                                       (format nil "theta-~a" (first node)))
                                      (read-from-string
                                       (format nil "alpha-~a" (second node))))
                                0)))
            (dolist (arc radiocarbon-table)
              (push (read-from-string (second arc)) context-list))
            (setf context-list (append (unique-pairs context-list)
                                       (unique-pairs (reverse context-list))))
            (dolist (pair context-list)
              (when (graph-matrix:reachablep
                     graph reachability-matrix (first pair) (first (rest pair)))
                (graph:add-edge
                 chronology-graph
                 (list (read-from-string
                        (format nil "alpha-~a" (first pair)))
                       (read-from-string
                        (format nil "beta-~a" (first (rest pair)))))
                 (if (eq 1 (graph-matrix:matrix-ref
                            adjacency-matrix
                            (gethash (first pair) node-index-hash)
                            (gethash (first (rest pair)) node-index-hash))) 1 2))))
            ;; write the dot file for the chronology graph
            (graph-dot:to-dot-file
             chronology-graph *chronology-out-file*
             :attributes
             (list
              (cons :style (make-attribute *chronology-graph-style*))
              (cons :colorscheme (make-attribute *chronology-color-scheme*))
              (cons :dpi (make-attribute *chronology-graph-dpi*))
              (cons :margin (make-attribute *chronology-graph-margin*))
              (cons :bgcolor
                    (format nil "~(~s~)"
                            (if *chronology-graph-bg-color*
                                (color-filter
                                 *chronology-graph-bg-color* nil) "")))
              (cons :fontname (make-attribute *chronology-graph-font-name*))
              (cons :fontsize (make-attribute *chronology-graph-font-size*))
              (cons :fontcolor
                    (format nil "~(~s~)"
                            (if *chronology-graph-font-color*
                                (color-filter *chronology-graph-font-color* nil) "")))
              (cons :splines (make-attribute *chronology-graph-splines*))
              (cons :page (make-attribute *chronology-graph-page*))
              (cons :size (make-attribute *chronology-graph-size*))
              (cons :ratio (make-attribute *chronology-graph-ratio*))
              (cons :label (make-attribute *chronology-graph-title*))
              (cons :labelloc (make-attribute *chronology-graph-labelloc*)))
             :edge-attrs (list
                          (cons :style
                                (lambda (e)
                                  (case (graph:edge-value chronology-graph e)
                                    (0 *chronology-edge-date*)
                                    (1 *chronology-edge-abutting*)
                                    (2 *chronology-edge-separated*))))
                          (cons :label
                                (constantly-format ""))
                          (cons :arrowhead
                                (constantly-format *chronology-edge-with-arrow*))
                          (cons :colorscheme
                                (constantly-format *chronology-color-scheme*))
                          (cons :color
                                (constantly-format
                                 (if *chronology-edge-color*
                                     (color-filter *chronology-edge-color* nil)
                                     "")))
                          (cons :fontname
                                (constantly-format *chronology-edge-font-name*))
                          (cons :fontsize
                                (constantly-format *chronology-edge-font-size*))
                          (cons :fontcolor
                                (constantly-format
                                 (if *chronology-edge-font-color*
                                     (color-filter *chronology-edge-font-color* nil)
                                     ""))))
             :node-attrs (list
                          (cons :shape
                                (lambda (n)
                                  (if (equal (char (symbol-name n) 0) #\T)
                                      *chronology-node-shape-date*
                                      *chronology-node-shape-phase*)))
                          (cons :style (constantly-format *chronology-node-style*))
                          (cons :fontname (constantly-format
                                           *chronology-node-font-name*))
                          (cons :fontsize (constantly-format
                                           *chronology-node-font-size*))
                          (cons :colorscheme
                                (constantly-format *chronology-color-scheme*))
                          (cons :color
                                (constantly-format
                                 (if *chronology-node-color*
                                     (color-filter *chronology-node-color* nil)
                                     "")))
                          (cons :fillcolor
                                (constantly
                                 (if *chronology-node-fill*
                                     (color-filter *chronology-node-fill* t)
                                     "")))
                          (cons :fontcolor
                                (constantly
                                 (if *chronology-node-font-color*
                                     (color-filter
                                      *chronology-node-font-color* t) "")))))
            (format t "Wrote ~a~%" (probe-file *chronology-out-file*)))
          ;; write the dot file for the sequence diagram
          (graph-dot:to-dot-file
           graph *out-file*
           :ranks *ranks*
           :attributes
           (list
            (cons :style (make-attribute *graph-style*))
            (cons :colorscheme (make-attribute *color-scheme*))
            (cons :dpi (make-attribute *graph-dpi*))
            (cons :URL (make-attribute *url-default*))
            (cons :margin (make-attribute *graph-margin*))
            (cons :bgcolor
                  (format nil "~(~s~)"
                          (if *graph-bg-color*
                              (color-filter
                               *graph-bg-color* nil) "")))
            (cons :fontname (make-attribute *graph-font-name*))
            (cons :fontsize (make-attribute *graph-font-size*))
            (cons :fontcolor
                  (format nil "~(~s~)"
                          (if *graph-font-color*
                              (color-filter *graph-font-color* nil) "")))
            (cons :splines (make-attribute *graph-splines*))
            (cons :page (make-attribute *graph-page*))
            (cons :size (make-attribute *graph-size*))
            (cons :ratio (make-attribute *graph-ratio*))
            (cons :label (make-attribute *graph-title*))
            (cons :labelloc (make-attribute *graph-labelloc*)))
           :edge-attrs (list
                        (cons :style
                              (constantly-format *edge-style*))
                        (cons :arrowhead
                              (constantly-format *edge-with-arrow*))
                        (cons :colorscheme
                              (constantly-format *color-scheme*))
                        (cons :color
                              (constantly-format
                               (if *edge-color*
                                   (color-filter *edge-color* nil)
                                   "")))
                        (cons :fontname
                              (constantly-format *edge-font-name*))
                        (cons :fontsize
                              (constantly-format *edge-font-size*))
                        (cons :fontcolor
                              (constantly-format
                               (if *edge-font-color*
                                   (color-filter *edge-font-color* nil)
                                   "")))
                        (cons :URL (if (and *url-include* arc-urls
                                            (> (hash-table-count arc-urls) 0))
                                       (lambda (e)
                                         (format nil "~s"
                                                 (gethash e arc-urls)))
                                       (constantly-format ""))))
           :node-attrs (list
                        (cons :shape
                              (if (and *symbolize-unit-type* unit-types
                                       (> (hash-table-count unit-types) 0))
                                  (lambda (n)
                                    (format nil "~(~s~)"
                                            (gethash n unit-types)))
                                  (constantly-format *node-shape-deposit*)))
                        (cons :style (constantly-format *node-style*))
                        (cons :fontname (constantly-format *node-font-name*))
                        (cons :fontsize (constantly-format *node-font-size*))
                        (cons :colorscheme
                              (constantly-format *color-scheme*))
                        (cons :color
                              (constantly-format
                               (if *node-color*
                                   (color-filter *node-color* nil)
                                   "")))
                        (cons :fillcolor
                              (if (and node-fills
                                       (> (hash-table-count node-fills) 0))
                                  (lambda (n) (+ 1 (gethash n node-fills)))
                                  (constantly
                                   (if *node-fill*
                                       (color-filter *node-fill* t)
                                       ""))))
                        (cons :fontcolor
                              (if (and node-fills
                                       (> (hash-table-count node-fills) 0))
                                  (lambda (n)
                                    (if (<= *label-break*
                                            (gethash n node-fills))
                                        (color-filter *label-color-light* t)
                                        (color-filter *label-color-dark* t)))
                                  (constantly
                                   (if *node-font-color*
                                       (color-filter *node-font-color* t) ""))))
                        (cons :URL
                              (if (and *url-include* node-urls
                                       (> (hash-table-count node-urls) 0))
                                  (lambda (n) (format nil "~s"
                                                      (gethash n node-urls)))
                                  (constantly-format "")))))
          (format t "Wrote ~a" (probe-file *out-file*))))
    (return-from hm-draw "Apparent success")))
