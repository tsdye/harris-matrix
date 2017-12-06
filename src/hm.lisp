;;; hm.lisp

;; Copyright (C) Thomas Dye 2017

;; Licensed under the Gnu Public License Version 3 or later

(in-package #:hm)

;; The inferior-shell package should enable hm.lisp to call a script
;; that compiles and displays the dot file output.
;; (require "inferior-shell")

(defun <-dot-graph (attribute)
  "Format an attribute from the user's configuration for Graphviz."
  (quotes-around attribute))


(defun to-dot-macro (seq element dot-attr graph-type &optional (verbose t))
  "Returns an anonymous function that takes a node or edge label and returns the
behavior indicated in the user's configuration."
  (let ((user-class (graphviz-classification seq element dot-attr))
        (cfg (archaeological-sequence-configuration seq))
        (colorp (fset:contains? (color-attributes) dot-attr)))
    (cond
      ((fset:contains? (fset:union (matrix-classes) (vector-classes)) user-class)
       (make-map seq element dot-attr graph-type user-class verbose))
      ((not user-class)
       (let ((user-val (lookup-graphviz-option cfg dot-attr element graph-type))
             (scheme (when colorp
                         (lookup-graphviz-option
                          cfg element :colorscheme graph-type))))
         (if colorp
             (constantly (quotes-around (graphviz-color-string user-val scheme)))
             (constantly (quotes-around user-val)))))
       (t (error "Error: Unable to set ~a ~a.~&" element dot-attr)))))

;; passes test
(defun quotes-around (string)
  "Put quotes around STRING for output to dot."
  (if (emptyp string) string
      (let ((quote-char #\"))
        (if (and (eq (char string 0) quote-char)
                 (eq (char string (1- (length string))) quote-char))
            string
            (concatenate 'string "\"" string "\"")))))

(defun new-graph ()
  "Returns a new instance of an empty directed graph."
  (make-instance 'graph:digraph))

(defun add-nodes (graph cfg &optional (verbose t))
  "Add nodes to a graph, GRAPH, using the information in the
configuration CFG.  If VERBOSE, then advertise the activity.  Returns
the possibly modified GRAPH."
  (let ((ret (graph:copy graph))
        (contexts (read-table (input-file-name cfg "contexts")
                              (file-header-p cfg "contexts") verbose)))
    (dolist (node contexts)
      (graph:add-node ret (symbolicate (nth 0 node))))
    (when verbose (format t "Nodes added to the sequence graph.~&"))
    ret))

(defun add-arcs (graph cfg &optional (verbose t))
  "Add arcs to a graph, GRAPH, using the information in the configuation CFG. If
VERBOSE, then advertise the activity. Returns the possibly modified GRAPH."
  (let ((ret (graph:copy graph))
        (obs (read-table (input-file-name cfg "observations")
                         (file-header-p cfg "observations") verbose)))
    (dolist (arc obs)
      (graph:add-edge ret (list (symbolicate (nth 0 arc))
                                (symbolicate (nth 1 arc)))))
    (when verbose (format t "Arcs added to the sequence graph.~&"))
    ret))

(defun make-new-sequence-graph (cfg &optional (verbose t))
  "Given a configuration CFG, make a new digraph instance and populate it with
  nodes and arcs from the files specified in the configuration."
  (let ((new-graph (new-graph)))
    (setf new-graph (add-nodes new-graph cfg verbose))
    (setf new-graph (add-arcs new-graph cfg verbose))
    new-graph))

(defun check-cycles (graph)
  "Reports an error when cycles are present in GRAPH."
  (and (graph:cycles graph) (error "Error: Graph contains a cycle")))

(defun assume-correlations (graph cfg &optional (verbose t))
  "Given the information in a configuration CFG, possibly merge and rename the
  nodes of GRAPH. Check for cycles and error out if present, otherwise return
  the possibly modified GRAPH."
  (if (assume-correlations-p cfg)
      (let ((ret (graph:copy graph))
            (input-file-name (input-file-name cfg "inferences"))
            (file-header (file-header-p cfg "inferences"))
            (inferences))
        (if input-file-name
            (setf inferences (read-table input-file-name file-header verbose))
            (error "Error: No inference table specified."))
        (dolist (part inferences)
          (graph:merge-nodes ret (symbolicate (nth 1 part))
                             (symbolicate (nth 0 part))
                             :new (correlated-node (nth 0 part) (nth 1 part))))
        (check-cycles ret)
        ret)
      graph))

(defun correlated-node (node-1 node-2 &optional (as-string nil))
  "Given two correlated node symbols, NODE-1 and NODE-2, return a new
symbol for the correlated nodes.  If AS-STRING is non-nil, return the
correlated node symbol as a string."
  (let ((new-node (symbolicate node-1 "=" node-2)))
    (if as-string (string new-node) new-node)))

(defun graphviz-make-ranks (cfg &optional (verbose t))
  "Returns a list of ranks for Graphviz output if the user's configuration, CFG,
specifies that correlations should be assumed true, nil otherwise."
  (let ((ranks))
    (if (assume-correlations-p cfg)
        (let ((inferences (read-table (input-file-name cfg "inferences")
                                      (file-header-p cfg "inferences") verbose))
              (contexts (read-table (input-file-name cfg "contexts")
                                    (file-header-p cfg "contexts") verbose)))
          (when inferences
            (appendf ranks (set-same-ranks inferences))
            (when verbose (format t "Ranks set from inferences.~&")))
          (when contexts
            (appendf ranks (set-other-ranks contexts))
            (when verbose (format t "Ranks set from contexts.~&")))))
    (when verbose (format t "Ranks not set.~&"))
    ranks))

(defstruct (archaeological-sequence (:print-function print-archaeological-sequence)) "A structure to hold the user configuration, the resulting sequence
and (optional) chronology graphs, and the various closures required to
visualize the archaeological sequence with d3 and GraphViz."
           (graph nil)
           (chronology-graph nil)
           (classifiers (fset:empty-map))
           (configuration nil))

(defun print-archaeological-sequence (seq stream depth)
  (format stream
          "#<Seq ~a,~a; Chron ~a,~a; Config ~a; Class ~d>"
          (if (archaeological-sequence-graph seq)
              (length (graph:nodes (archaeological-sequence-graph seq)))
              0)
          (if (archaeological-sequence-graph seq)
              (length (graph:edges (archaeological-sequence-graph seq)))
              0)
          (if (archaeological-sequence-chronology-graph seq)
              (length (graph:nodes (archaeological-sequence-chronology-graph
                                    seq)))
              0)
          (if (archaeological-sequence-chronology-graph seq)
              (length (graph:edges (archaeological-sequence-chronology-graph
                                    seq)))
              0)
          (if (archaeological-sequence-configuration seq) "yes" "no")
          (fset:size (archaeological-sequence-classifiers seq))))

(defun configure-archaeological-sequence (seq cfg &optional (verbose t))
  "Configures the archaeological sequence SEQ using the information in
the configuration CFG, and returns the possibly modified
archaeological sequence.  Checks for common configuration
discrepancies and errors out if it finds one."
  (unless (typep seq 'archaeological-sequence) (error "Error: No sequence found."))
  (unless (typep cfg 'config) (error "Error: No configuration found."))
  (configuration-errors? cfg)
  (let ((ret (copy-structure seq)))
    (setf (archaeological-sequence-configuration ret) cfg)
    (setf (archaeological-sequence-graph ret) (make-new-sequence-graph cfg verbose))
    ;; (setf (archaeological-sequence-graph ret)
    ;;       (add-missing-interfaces (archaeological-sequence-graph ret)
    ;;                               cfg verbose))
    (setf (archaeological-sequence-graph ret)
          (assume-correlations (archaeological-sequence-graph ret) cfg verbose))
    (setf (archaeological-sequence-chronology-graph ret)
          (create-chronology-graph ret verbose))
    (fset:do-set (classifier (classifiers))
                 (let ((class (sequence-classifier cfg classifier)))
                   (when class
                     (when verbose
                       (format t "Making classifier for ~a.~&" class))
                     (setf (archaeological-sequence-classifiers ret)
                           (fset:with
                            (archaeological-sequence-classifiers ret)
                            class (make-classifier class ret verbose))))))
    (when verbose (format t "Archaeological sequence configured.~&"))
    ret))



(defun create-chronology-graph (seq &optional (verbose t))
  "If the user has requested a chronology graph, then create and return a
chronology graph, given an archaeological sequence, SEQ. Otherwise, return an
empty graph. If VERBOSE, then advertise progress."
  (if (chronology-graph-p (archaeological-sequence-configuration seq))
      (when verbose
        (format t "Creating chronology graph.~&"))
      (progn
        (when verbose (format t "Chronology graph off.~&"))
        (return-from create-chronology-graph (new-graph))))
  (let* ((ret (new-graph))
         (distance-matrix
           (create-distance-matrix (archaeological-sequence-configuration seq)
                                   (archaeological-sequence-graph seq)))
         (cfg (archaeological-sequence-configuration seq))
         (event-table (read-table (input-file-name cfg "events")
                                  (file-header-p cfg "events")
                                  verbose))
         (event-order-table
           (when (input-file-name-p "event-order")
             (read-table (input-file-name cfg "event-order")
                         (file-header-p cfg "event-order")
                         verbose))))
    ;; If assume-correlations then adjust the event-table and
    ;; event-order table accordingly
    (when (assume-correlations-p cfg)
      (let ((inference-table
              (read-table (input-file-name cfg "inferences")
                          (file-header-p cfg "inferences")
                          verbose))
            (inference-map (fset:empty-map)))
        (dolist (row inference-table)
          (setq inference-map
                (fset:with inference-map
                           (nth 0 row)
                           (correlated-node (nth 0 row) (nth 1 row) t)))
          (setq inference-map
                (fset:with inference-map
                           (nth 1 row)
                           (correlated-node (nth 0 row) (nth 1 row) t))))
        (setf event-table
              (mapcar #'(lambda (row)
                          (let ((node-1 (fset:lookup inference-map (nth 0 row)))
                                (node-2 (fset:lookup inference-map (nth 1 row))))
                            (when node-1 (setf (nth 0 row) node-1))
                            (when node-2 (setf (nth 1 row) node-2))))
                      event-table))
        (when event-order-table
          (setf event-order-table
                (mapcar #'(lambda (row)
                            (let ((node-1 (fset:lookup inference-map (nth 0 row)))
                                  (node-2 (fset:lookup inference-map (nth 1 row))))
                              (when node-1 (setf (nth 0 row) node-1))
                              (when node-2 (setf (nth 1 row) node-2))))
                        event-order-table)))))
    ;; Steps 1 and 2 of the algorithm
    (dolist (col event-table)
      (graph:add-node ret (symbolicate "alpha-"(nth 1 col)))
      (graph:add-node ret (symbolicate "beta-" (nth 1 col)))
      (graph:add-node ret (symbolicate "theta-"(nth 0 col))))
    ;; Step 3 of the algorithm
    (when event-order-table
      (dolist (pair event-order-table)
        (graph:add-edge ret
                        (list (symbolicate "theta-" (nth 1 pair))
                              (symbolicate "theta-" (nth 0 pair)))
                        0)))
    (when verbose
      (format t "Modeling radiocarbon dates.~&"))
    ;; Step 4 of the algorithm
    (dolist (node event-table)
      (and (eq 0 (graph:indegree ret (symbolicate "theta-" (nth 0 node))))
           (not (string= (nth 3 node) "disparate"))
           (graph:add-edge ret
                           (list (symbolicate "beta-" (nth 1 node))
                                 (symbolicate "theta-" (nth 0 node)))
                           0))
      (and (eq 0 (graph:outdegree ret
                                  (symbolicate "theta-" (nth 0 node))))
           (not (string= (nth 3 node) "disjunct"))
           (graph:add-edge ret
                           (list (symbolicate "theta-" (nth 0 node))
                                 (symbolicate "alpha-" (nth 1 node)))
                           0)))
    ;; Step 5 of the algorithm
    (let ((i (map-index-to-node (archaeological-sequence-graph seq)))
          (events))
      (when verbose
        (format t "Adding edge values to the chronology graph.~&"))
      (dolist (row event-table)
        (push (symbolicate (nth 1 row))
              events)
        (graph:add-edge ret
                        (list (symbolicate "beta-" (nth 1 row))
                              (symbolicate "alpha-" (nth 1 row)))
                        2))
      (dolist (pair (append (unique-pairs events)
                            (unique-pairs (reverse events))))
        (let ((distance (graph-matrix:matrix-ref distance-matrix
                                                 (fset:@ i (nth 0 pair))
                                                 (fset:@ i (nth 1 pair)))))
          (unless (graph-matrix:infinitep distance distance-matrix)
            (graph:add-edge ret
                            (list (symbolicate "alpha-" (nth 0 pair))
                                  (symbolicate "beta-" (nth 1 pair)))
                            (if (eql 1 (round distance)) 1 2))))))
    ;; Step 6 of the algorithm
    (when (graph:cycles ret)
      (error "Error: chronology graph has a cycle."))
    (setf ret (transitive-reduction ret))
    ret))

(defun transitive-reduction (graph)
  "Perform transitive reduction on the directed acyclic GRAPH. Returns the
possibly modified directed acyclic GRAPH."
  (let ((ret (graph:copy graph))
        (r (graph-matrix:to-adjacency-matrix graph (new-matrix nil)))
        (n (length (graph:nodes graph)))
        (i (map-index-to-node graph)))
    (loop :for x :below n :do
      (loop :for y :below n :do
        (loop :for z :below n :do
          (and (= 1 (graph-matrix:matrix-ref r x y))
               (= 1 (graph-matrix:matrix-ref r y z))
               (= 1 (graph-matrix:matrix-ref r x z))
               (progn
                 (setf (graph-matrix:matrix-ref r x z) 0)
                 (graph:delete-edge ret (list (fset:@ i x) (fset:@ i z))))))))
    ret))


;; The following two functions are from
;; http://stackoverflow.com/questions/14758218/two-element-combinations-of-the-elements-of-a-list-inside-lisp-without-duplicat

;; The functions answered this query:

;; From any given list in lisp, I want to get the two element combinations of the
;; elements of that list without having duplicate combinations
;; (meaning (a b) = (b a) and one should be removed)

;; So for example if I have a list that is (a b c d),

;; I want to get ((a b) (a c) (a d) (b c) (b d) (c d))

(defun pair-with (elem list)
  (mapcar (lambda (a)
            (list elem a))
          list))

(defun unique-pairs (list)
  (mapcon (lambda (rest)
            (pair-with (first rest)
                       (rest rest)))
          (remove-duplicates list)))

(defun new-matrix (&optional (fast t))
  "Makes a matrix instance.  If FAST is t, then uses fast matrix
routines.  If FAST is nil, then uses CL matrix routines."
  (if fast
      (make-instance 'graph-matrix:fast-matrix)
    (make-instance 'graph-matrix:matrix)))

;; this function should calculate the missing interfaces, then write out revised
;; .ini and .csv files, but leave the working graph alone
(defun add-missing-interfaces (graph cfg &optional (verbose t))
  "Check for edges in GRAPH that connect two depositional nodes and, if found,
  insert an interfacial node between them.  Returns the possibly
  modified GRAPH."
  (if (missing-interfaces-p cfg)
      (let ((hiatus)
            (g (graph:copy graph))
            (contexts (read-table (input-file-name cfg "contexts")
                                  (file-header-p cfg "contexts")
                                  verbose))
            (context-lookup (fset:empty-map)))
        (dolist (context contexts)
          (setf context-lookup (fset:with context-lookup
                                          (symbolicate (nth 0 context))
                                          (nth 1 context))))
        (dolist (edge (graph:edges g))
          (and (string= (fset:@ context-lookup
                                (nth 0 edge))
                        "deposit")
               (string= (fset:@ context-lookup
                                (nth 1 edge))
                        "deposit")
               (push edge hiatus)))
        (when verbose
          (format t
                  "Adding ~:d interfaces.~&"
                  (list-length hiatus)))
        (dolist (edge hiatus)
          (let ((new-node (symbolicate (nth 1 edge)
                                       "-*surface*")))
            (graph:add-node g new-node)
            (graph:add-edge g (list (nth 0 edge) new-node))
            (graph:add-edge g (list new-node (nth 1 edge)))
            (graph:delete-edge g edge)))
        g)
    graph))

;; graph structure functions

(defun set-same-ranks (table)
  "Use the values in TABLE to return a list of graph:rank structures
where the indicated nodes are to appear on the same rank of the graph
picture."
  (let ((ranks))
    (mapcar #'(lambda (x)
                (push (graph-dot::make-rank :value "same"
                                            :node-list (list (nth 0 x)
                                                             (nth 1 x)))
                      ranks))
            table)
    ranks))

(defun set-other-ranks (table)
  "Use the values in TABLE to return a list of graph:rank structures
where the indicated nodes either appear at the top or the bottom of
the graph picture."
  (let ((ranks))
    (mapcar
     #'(lambda (x)
         (let ((rank (nth 2 x)))
           (when (or (string= rank "basal") (string= rank "surface"))
             (push (graph-dot::make-rank
                    :value (cond
                             ((string= rank "basal") "sink")
                             ((string= rank "surface") "source"))
                    :node-list (list (nth 0 x)))
                   ranks))))
     table)
    ranks))

;; write the dot file for the sequence diagram
(defun write-sequence-graph-to-dot-file (seq &optional (verbose t))
  "Write a sequence graph to a Graphviz dot file, based on the information in
the archaeological sequence, SEQ."
  (let* ((cfg (archaeological-sequence-configuration seq))
         (graph (archaeological-sequence-graph seq))
         (out-file (output-file-name cfg :sequence-dot)))
    (graph-dot:to-dot-file
     graph out-file
     :ranks (graphviz-make-ranks cfg verbose)
     :attributes (list
                  (cons :style (quotes-around
                                (graphviz-sequence-graph-attribute cfg :style)))
                  (cons :dpi (quotes-around
                              (graphviz-sequence-graph-attribute cfg :dpi)))
                  (cons :margin (quotes-around
                                 (graphviz-sequence-graph-attribute cfg :margin)))
                  (cons :bgcolor (quotes-around
                                  (graphviz-sequence-graph-color cfg :bgcolor)))
                  (cons :fontname (quotes-around
                                   (graphviz-sequence-graph-attribute cfg :fontname)))
                  (cons :fontsize (quotes-around
                                   (graphviz-sequence-graph-attribute cfg :fontsize)))
                  (cons :fontcolor (quotes-around
                                    (graphviz-sequence-graph-color cfg :fontcolor)) )
                  (cons :splines (quotes-around
                                  (graphviz-sequence-graph-attribute cfg :splines)))
                  (cons :page (quotes-around
                               (graphviz-sequence-graph-attribute cfg :page)))
                  (cons :size (quotes-around
                               (graphviz-sequence-graph-attribute cfg :size)))
                  (cons :ratio (quotes-around
                                (graphviz-sequence-graph-attribute cfg :ratio)))
                  (cons :label (quotes-around
                                (graphviz-sequence-graph-attribute cfg :label)))
                  (cons :labelloc (quotes-around
                                   (graphviz-sequence-graph-attribute cfg :labelloc))))
     :edge-attrs (list
                  (cons :style (<-dot seq :edge :style :sequence verbose))
                  (cons :arrowhead (<-dot seq :edge :arrowhead :sequence verbose))
                  (cons :color (<-dot seq :edge :color :sequence verbose))
                  (cons :fontname (graphviz-sequence-edge-attribute cfg :fontname))
                  (cons :fontsize (<-dot seq :edge :fontsize :sequence verbose))
                  (cons :fontcolor (<-dot seq :edge :fontcolor :sequence verbose))
                  (cons :penwidth (<-dot seq :edge :penwidth :sequence verbose))
                  ;; (cons :URL (<-dot seq :edge :url :sequence verbose))
                  )
     :node-attrs (list
                  (cons :shape (<-dot seq :node :shape :sequence verbose))
                  (cons :style (<-dot seq :node :style :sequence verbose))
                  (cons :fontname (graphviz-sequence-node-attribute cfg :fontname))
                  (cons :fontsize (graphviz-sequence-node-attribute cfg :fontsize))
                  (cons :color (<-dot seq :node :color :sequence verbose))
                  (cons :fillcolor (<-dot seq :node :fillcolor :sequence verbose))
                  (cons :fontcolor (<-dot seq :node :fontcolor :sequence verbose))
                  (cons :penwidth (<-dot seq :node :penwidth :sequence verbose))
                  (cons :skew (<-dot seq :node :polygon-skew :sequence verbose))
                  (cons :sides (<-dot seq :node :polygon-sides :sequence verbose))
                  (cons :orientation (<-dot seq :node :polygon-orientation :sequence verbose))
                  (cons :distortion (<-dot seq :node :polygon-distortion :sequence verbose))
                  ;; (cons :URL (<-dot seq :node :url :sequence verbose))
                  ))
    (when verbose (format t "Wrote ~a.~%" out-file))))
