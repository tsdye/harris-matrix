;;; hm.lisp

;; Copyright (C) Thomas Dye 2017-2018

;; Licensed under the Gnu Public License Version 3 or later

(in-package #:hm)

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

(defun to-chron-macro (seq element &optional (verbose t))
  "Returns an anonymous function that takes a node or edge label for a
  chronology graph and returns the behavior indicated in the user's
  configuration."
  (if (eq element :node)
      (chronology-node-map seq verbose)
      (chronology-edge-map seq verbose)))

(defun quotes-around (string)
  "Ensure there are double quotes around STRING for output to dot."
  (if (emptyp string) "\"\""
      (let ((quote-char #\"))
        (if (and (eq (char string 0) quote-char)
                 (eq (char string (- (length string) 1)) quote-char))
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
        (contexts (read-context-table cfg verbose))
        (count 0))
    (when verbose (format t "Adding nodes to the sequence graph.~&"))
    (fset:do-map (node rec contexts)
      (incf count)
      (graph:add-node ret node))
    (when verbose (format t "~:d nodes added to the sequence graph.~&" count))
    ret))

(defun add-arcs (graph cfg &optional (verbose t))
  "Add arcs to a graph, GRAPH, using the information in the configuration CFG.
Check that both nodes are present in GRAPH and error out if not. When VERBOSE,
then advertise the activity. Returns the possibly modified GRAPH."
  (let ((ret (graph:copy graph))
        (obs (read-table (input-file-name cfg :observations)
                         (file-header-p cfg :observations) verbose))
        (count 0))
    (when verbose (format t "Adding arcs to the sequence graph.~&"))
    (dolist (arc obs)
      (let ((younger (ensure-symbol (nth 0 arc)))
            (older (ensure-symbol (nth 1 arc))))
        (incf count)
        (unless (graph:has-node-p ret younger)
          (error "Error: Younger node ~a absent from graph." younger))
        (unless (graph:has-node-p ret older)
          (error "Error: Older node ~a absent from graph." older))
        (graph:add-edge ret (list younger older))))
    (when verbose (format t "~:d arcs added to the sequence graph.~&" count))
    ret))

(defun make-new-sequence-graph (cfg &optional (verbose t))
  "Given a configuration CFG, make a new digraph instance, populate it with
  nodes and arcs from the files specified in the configuration, and return it."
  (let ((graph (new-graph)))
    (setf graph (add-nodes graph cfg verbose))
    (setf graph (add-arcs graph cfg verbose))
    (if (assume-correlations-p cfg)
      (let ((terms (correlation-terms cfg)))
        (setf graph (assume-correlations graph cfg :verbose verbose :terms terms)))
      (check-cycles graph :verbose verbose :prune-fvs nil))
    (setf graph (transitive-reduction-2 graph verbose))
    graph))

(defun check-cycles (graph &key (verbose t) (prune-fvs t))
  "Reports an error when cycles are present in GRAPH, or returns nil if no
cycles are found. The error message contains a list of suspicious nodes, which
will contain only inferred nodes if PRUNE-FVS is non-nil."
  (let ((fvs))
    (when verbose (format t "Checking directed graph ~a for cycles.~&" graph))
    (when (graph:basic-cycles graph)
      (when verbose (format t "Cycles detected, calculating feedback arc set.~&"))
      (setf fvs (array-fas graph verbose))
      (when prune-fvs (setf fvs (remove-if-not (lambda (x) (find #\= (string x))) fvs)))
      (when verbose
        (format t "Directed graph ~a is cyclical, check nodes ~a.~&" graph fvs))
      (error "Error: Directed graph is cyclical, check nodes ~a.~&" fvs))
    (when verbose (format t "No cycles found in directed graph ~a.~&" graph))))

(defun assume-correlations (graph cfg &key (verbose t) (terms 2))
  "Given the information in a configuration CFG, possibly merge and rename the
  nodes of GRAPH. Checks that both correlated nodes are in GRAPH and errors out
  if not. Checks for cycles and error out if present, otherwise return the
  possibly modified GRAPH and an fset map of user nodes and correlated nodes.
  If :VERBOSE then write messages to standard output. :TERMS refers to the
  number of terms in the assumption. 2 is the standard stratigraphic case. 3 and
  4 are horizontal stratigraphic cases where 3 assumes two terms are correlated
  and older than the third term, and 4 consists of two pairs, one or the other
  of which can be assumed and the other two are both younger than the two
  correlated terms."
  (let ((ret (graph:copy graph))
        (input-file-name (input-file-name cfg :inferences))
        (file-header (file-header-p cfg :inferences))
        (node-map (fset:empty-map))
        (count 0))
    (if input-file-name
        (when verbose
          (format t "Reading inference file ~a.~&" (enough-namestring input-file-name))
          (format t "Making inferences.~&"))
        (error "Error: Input file name is missing."))
    (case terms
      (2 (cl-csv:do-csv (part input-file-name :skip-first-p file-header)
           (let ((first-term (ensure-symbol (nth 0 part)))
                 (second-term (ensure-symbol (nth 1 part))))
             (loop while (fset:domain-contains? node-map first-term)
                   do (setf first-term (fset:lookup node-map first-term)))
             (loop while (fset:domain-contains? node-map second-term)
                   do (setf second-term (fset:lookup node-map second-term)))
             (unless (graph:has-node-p ret first-term)
               (error "Error: Correlated node ~a absent from graph." first-term))
             (unless (graph:has-node-p ret second-term)
               (error "Error: Correlated node ~a absent from graph." second-term))
             (let ((new-node (correlated-node first-term second-term)))
               (graph:merge-nodes ret second-term first-term :new new-node)
               (setf node-map (fset:with node-map first-term new-node))
               (setf node-map (fset:with node-map second-term new-node))
               (incf count)))))
      (3 (cl-csv:do-csv (part input-file-name :skip-first-p file-header)
           (let ((first-term (ensure-symbol (nth 0 part)))
                 (second-term (ensure-symbol (nth 1 part)))
                 (younger-term (ensure-symbol (nth 2 part)))
                 (flag (cond ((string= "true" (nth 3 part)) t)
                             ((string= "false" (nth 3 part)) nil)
                             (t (error "Value of INFER is incorrect, ~a" (nth 3 part))))))
             (if flag
                 (progn
                   (loop while (fset:domain-contains? node-map first-term)
                         do (setf first-term (fset:lookup node-map first-term)))
                   (loop while (fset:domain-contains? node-map second-term)
                         do (setf second-term (fset:lookup node-map second-term)))
                   (unless (graph:has-node-p ret first-term)
                     (error "Error: Correlated node ~a absent from graph." first-term))
                   (unless (graph:has-node-p ret second-term)
                     (error "Error: Correlated node ~a absent from graph." second-term))
                   (let ((new-node (correlated-node first-term second-term)))
                     (graph:merge-nodes ret second-term first-term :new new-node)
                     (setf node-map (fset:with node-map first-term new-node))
                     (setf node-map (fset:with node-map second-term new-node))
                     (graph:add-edge ret (list younger-term new-node))))
                 (progn
                   (graph:add-edge ret (list first-term younger-term))
                   (graph:add-edge ret (list second-term younger-term))))
             (incf count))))
      (4 (cl-csv:do-csv (part input-file-name :skip-first-p file-header)
           (let ((first-term (ensure-symbol (nth 0 part)))
                 (third-term (ensure-symbol (nth 2 part)))
                 (fourth-term (ensure-symbol (nth 3 part))))
             (loop while (fset:domain-contains? node-map first-term)
                   do (setf first-term (fset:lookup node-map first-term)))
             (graph:add-edge ret (list third-term first-term))
             (graph:add-edge ret (list fourth-term first-term))
             (incf count)))))
    (when verbose (format t "Made ~:d inferences.~&" count))
    (check-cycles ret :verbose verbose :prune-fvs t)
    (values ret node-map)))

(defun correlated-node (first-node second-node &optional as-string)
  "Given two correlated node symbols, FIRST-NODE and SECOND-NODE, return a new
symbol for the correlated nodes.  If AS-STRING is non-nil, return the
correlated node symbol as a string."
  (let ((new-node (symbolicate first-node "=" second-node)))
    (if as-string (string new-node) new-node)))

(defun graphviz-make-ranks (cfg &optional (verbose t))
  "Returns a list of ranks for Graphviz output if the user's configuration, CFG,
specifies that correlations should be assumed true, nil otherwise."
  (let ((ranks))
    (if (assume-correlations-p cfg)
        (let ((inferences (read-table (input-file-name cfg :inferences)
                                      (file-header-p cfg :inferences) verbose))
              (contexts (read-table (input-file-name cfg :contexts)
                                    (file-header-p cfg :contexts) verbose)))
          (when inferences (appendf ranks (set-same-ranks inferences))
                (when verbose (format t "Ranks set from inferences.~&")))
          (when contexts (appendf ranks (set-other-ranks contexts))
                (when verbose (format t "Ranks set from contexts.~&")))))
    (when verbose (format t "Ranks not set.~&"))
    ranks))

(defstruct (archaeological-sequence (:print-function print-archaeological-sequence))
  "A structure to hold the user configuration, the resulting sequence
and (optional) chronology graphs, and the various closures required to visualize
the archaeological sequence with d3 and GraphViz."
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
              (length (graph:nodes (archaeological-sequence-chronology-graph seq)))
              0)
          (if (archaeological-sequence-chronology-graph seq)
              (length (graph:edges (archaeological-sequence-chronology-graph seq)))
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
    (setf (archaeological-sequence-chronology-graph ret)
          (create-chronology-graph ret verbose))
    (fset:do-set (attribute (classifiable-attributes))
                 (let ((class (sequence-classifier cfg attribute)))
                   (when class
                     (setf (archaeological-sequence-classifiers ret)
                           (fset:with
                            (archaeological-sequence-classifiers ret)
                            class (make-classifier class ret verbose))))))
    (when verbose (format t "Archaeological sequence configured.~&"))
    ret))

(defun lookup-correlated-node (node map)
  "Returns a symbol for NODE or its correlated context."
  (let ((node-symbol (ensure-symbol node)))
    (if (and map (fset:domain-contains? map node-symbol))
        (fset:@ map node-symbol) node-symbol)))

(defun create-chronology-graph (seq &optional (verbose t))
  "If the user has requested a chronology graph, then create and return a
chronology graph, given an archaeological sequence, SEQ. Otherwise, return an
empty graph. If VERBOSE, then advertise progress."
  (if (chronology-graph-p (archaeological-sequence-configuration seq))
      (when verbose
        (format t "Creating the chronology graph.~&"))
      (progn
        (when verbose (format t "Chronology graph off.~&"))
        (return-from create-chronology-graph (new-graph))))
  (let* ((ret (new-graph))
         (distance-matrix (create-distance-matrix seq))
         (cfg (archaeological-sequence-configuration seq))
         (event-table (read-table (input-file-name cfg :events)
                                  (file-header-p cfg :events)
                                  verbose))
         (event-order-table
           (when (input-file-name-p cfg "event-order")
             (read-table (input-file-name cfg :event-order)
                         (file-header-p cfg :event-order)
                         verbose)))
         (inference-map (fset:empty-map)))
    ;; If assume-correlations then make a map to adjust the event-table and
    ;; event-order table accordingly
    (when (assume-correlations-p cfg)
      (let ((inference-table
              (read-table (input-file-name cfg :inferences)
                          (file-header-p cfg :inferences)
                          verbose)))
        (dolist (row inference-table)
          (setq inference-map
                (fset:with inference-map
                           (ensure-symbol (nth 0 row))
                           (correlated-node (nth 0 row) (nth 1 row))))
          (setq inference-map
                (fset:with inference-map
                           (ensure-symbol (nth 1 row))
                           (correlated-node (nth 0 row) (nth 1 row)))))))
    ;; Steps 1 and 2 of the algorithm
    (when verbose (format t "Adding phase nodes to the chronology graph.~&"))
    (dolist (col event-table)
      (graph:add-node ret (symbolicate
                           "alpha-" (lookup-correlated-node (nth 1 col) inference-map)))
      (graph:add-node ret (symbolicate
                           "beta-" (lookup-correlated-node (nth 1 col) inference-map)))
      (graph:add-node ret (symbolicate "theta-" (nth 0 col))))
    ;; Step 3 of the algorithm
    (when verbose (format t "Adding dated event nodes to the chronology graph.~&"))
    (when event-order-table
      (dolist (pair event-order-table)
        (let ((older (symbolicate "theta-" (nth 0 pair)))
              (younger (symbolicate "theta-" (nth 1 pair))))
          (graph:add-edge ret (list older younger) 0))))
    ;; Step 4 of the algorithm
    (dolist (node event-table)
      (let ((event (symbolicate "theta-" (nth 0 node)))
            (beta (symbolicate "beta-" (lookup-correlated-node
                                        (nth 1 node) inference-map)))
            (alpha (symbolicate "alpha-" (lookup-correlated-node
                                          (nth 1 node) inference-map))))
        (and (eq 0 (graph:indegree ret event))
             (not (equal (nth 3 node) "disparate"))
             (graph:add-edge ret (list beta event) 0))
        (and (eq 0 (graph:outdegree ret event))
             (not (equal (nth 3 node) "disjunct"))
             (graph:add-edge ret (list event alpha) 0))))
    ;; Step 5 of the algorithm
    (when verbose (format t "Adding edges to the chronology graph.~&"))
    (let ((i (map-node-to-index (archaeological-sequence-graph seq)))
          (events))
      (dolist (row event-table)
        (push (lookup-correlated-node (nth 1 row) inference-map) events)
        (graph:add-edge ret
                        (list (symbolicate "beta-" (lookup-correlated-node
                                                    (nth 1 row) inference-map))
                              (symbolicate "alpha-" (lookup-correlated-node
                                                     (nth 1 row) inference-map)))
                        2))
      (map-permutations
       #'(lambda (pair)
           (let ((distance (graph/matrix:matrix-ref distance-matrix
                                                    (fset:@ i (nth 0 pair))
                                                    (fset:@ i (nth 1 pair)))))
             (unless (graph/matrix:infinitep distance distance-matrix)
               (graph:add-edge ret
                               (list (symbolicate "alpha-" (nth 0 pair))
                                     (symbolicate "beta-" (nth 1 pair)))
                               (if (= 1 distance) 1 2)))))
       (remove-duplicates events) :length 2))
    ;; Step 6 of the algorithm
    (when verbose (format t "Checking the chronology graph for cycles.~&"))
    (when (graph:basic-cycles ret)
      (error "Error: The chronology graph is cyclical.~&Nodes: ~a~&Edges: ~a~&"
             (graph:nodes ret) (graph:edges ret)))
    (when verbose (format t "Performing transitive reduction of the chronology graph.~&"))
    (setq ret (transitive-reduction-2 ret verbose))
    ret))

(defun transitive-reduction-2 (graph &optional (verbose t))
  "Perform transitive reduction on the directed acyclic GRAPH. Return the
possibly modified GRAPH. The algorithm was described here
https://cs.stackexchange.com/q/29133."
  (when verbose (format t "Starting transitive reduction.~&"))
  (let ((ret (graph:copy graph)))
    (dolist (node (graph:nodes ret))
      (dolist (child (graph:adjacent-from ret node))
        (dolist (reachable (remove child (graph:connected-component ret child)))
          (let ((candidate (list node reachable)))
            (when (graph:has-edge-p ret candidate) (graph:delete-edge ret candidate)
                  (when verbose
                    (format t "Removed transitive arc ~a.~&" candidate)))))))
    (when verbose (format t "Transitive reduction finished.~&"))
    ret))

(defun new-matrix (&optional (fast t))
  "Makes a matrix instance.  If FAST is t, then uses fast matrix
routines.  If FAST is nil, then uses CL matrix routines."
  (if fast (make-instance 'graph/matrix:fast-matrix)
      (make-instance 'graph/matrix:matrix)))

;; graph structure functions

(defun set-same-ranks (table)
  "Use the values in TABLE to return a list of graph:rank structures
where the indicated nodes are to appear on the same rank of the graph
picture."
  (let ((ranks))
    (mapcar
     #'(lambda (x)
         (push (graph/dot::make-rank
                :value "same" :node-list (list (nth 0 x) (nth 1 x)))
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
           (when (or (equal rank "basal") (equal rank "surface"))
             (push (graph/dot::make-rank
                    :value (cond ((equal rank "basal") "sink")
                                 ((equal rank "surface") "source"))
                    :node-list (list (nth 0 x)))
                   ranks))))
     table)
    ranks))

(defun write-sequence-graph-to-dot-file (seq &optional (verbose t))
  "Write a sequence graph to a Graphviz dot file, based on the information in
the archaeological sequence, SEQ."
  (let* ((cfg (archaeological-sequence-configuration seq))
         (graph (archaeological-sequence-graph seq))
         (out-file (output-file-name cfg :sequence-dot)))
    (graph/dot:to-dot-file
     graph out-file
     :attributes
     (list
      (cons :style (quotes-around (graphviz-sequence-graph-attribute cfg :style)))
      (cons :dpi (quotes-around (graphviz-sequence-graph-attribute cfg :dpi)))
      (cons :margin (quotes-around (graphviz-sequence-graph-attribute cfg :margin)))
      (cons :bgcolor (quotes-around (graphviz-sequence-graph-color cfg :bgcolor)))
      (cons :fontname (quotes-around (graphviz-sequence-graph-attribute cfg :fontname)))
      (cons :fontsize (quotes-around (graphviz-sequence-graph-attribute cfg :fontsize)))
      (cons :fontcolor (quotes-around (graphviz-sequence-graph-color cfg :fontcolor)) )
      (cons :splines (quotes-around (graphviz-sequence-graph-attribute cfg :splines)))
      (cons :page (quotes-around (graphviz-sequence-graph-attribute cfg :page)))
      (cons :size (quotes-around (graphviz-sequence-graph-attribute cfg :size)))
      (cons :ratio (quotes-around (graphviz-sequence-graph-attribute cfg :ratio)))
      (cons :url (quotes-around (graphviz-sequence-graph-attribute cfg :url)))
      (cons :label (quotes-around (graphviz-sequence-graph-attribute cfg :label)))
      (cons :labelloc (quotes-around (graphviz-sequence-graph-attribute cfg :labelloc))))
     :edge-attrs
     (list
      (cons :style (<-seq seq :edge :style :sequence verbose))
      (cons :arrowhead (<-seq seq :edge :arrowhead :sequence verbose))
      (cons :color (<-seq seq :edge :color :sequence verbose))
      (cons :fontname (graphviz-sequence-edge-attribute cfg :fontname verbose))
      (cons :fontsize (<-seq seq :edge :fontsize :sequence verbose))
      (cons :fontcolor (<-seq seq :edge :fontcolor :sequence verbose))
      (cons :penwidth (<-seq seq :edge :penwidth :sequence verbose))
      (cons :URL (graphviz-sequence-edge-attribute cfg :url verbose)))
     :node-attrs
     (list
      (cons :shape (<-seq seq :node :shape :sequence verbose))
      (cons :style (<-seq seq :node :style :sequence verbose))
      (cons :fontname (graphviz-sequence-node-attribute cfg :fontname verbose))
      (cons :fontsize (graphviz-sequence-node-attribute cfg :fontsize verbose))
      (cons :color (<-seq seq :node :color :sequence verbose))
      (cons :fillcolor (<-seq seq :node :fillcolor :sequence verbose))
      (cons :fontcolor (<-seq seq :node :fontcolor :sequence verbose))
      (cons :penwidth (<-seq seq :node :penwidth :sequence verbose))
      (cons :skew (<-seq seq :node :polygon-skew :sequence verbose))
      (cons :sides (<-seq seq :node :polygon-sides :sequence verbose))
      (cons :orientation (<-seq seq :node :polygon-orientation :sequence verbose))
      (cons :distortion (<-seq seq :node :polygon-distortion :sequence verbose))
      (cons :URL (graphviz-sequence-node-attribute cfg :url verbose))))
    (when verbose (format t "Wrote ~a.~%" out-file))))

(defun write-chronology-graph-to-dot-file (seq &optional (verbose t))
  "Write a chronology graph to a Graphviz dot file, based on the information in
the archaeological sequence, SEQ."
  (let* ((cfg (archaeological-sequence-configuration seq))
         (graph (archaeological-sequence-chronology-graph seq))
         (out-file (output-file-name cfg :chronology-dot)))
    (graph/dot:to-dot-file
     graph out-file
     :attributes
     (list
      (cons :style (quotes-around (graphviz-chronology-graph-attribute cfg :style)))
      (cons :dpi (quotes-around (graphviz-chronology-graph-attribute cfg :dpi)))
      (cons :margin (quotes-around (graphviz-chronology-graph-attribute cfg :margin)))
      (cons :bgcolor (quotes-around (graphviz-chronology-graph-color cfg :bgcolor)))
      (cons :fontname (quotes-around (graphviz-chronology-graph-attribute cfg :fontname)))
      (cons :fontsize (quotes-around (graphviz-chronology-graph-attribute cfg :fontsize)))
      (cons :fontcolor (quotes-around (graphviz-chronology-graph-color cfg :fontcolor)) )
      (cons :splines (quotes-around (graphviz-chronology-graph-attribute cfg :splines)))
      (cons :page (quotes-around (graphviz-chronology-graph-attribute cfg :page)))
      (cons :size (quotes-around (graphviz-chronology-graph-attribute cfg :size)))
      (cons :ratio (quotes-around (graphviz-chronology-graph-attribute cfg :ratio)))
      (cons :label (quotes-around (graphviz-chronology-graph-attribute cfg :label)))
      (cons :labelloc (quotes-around
                       (graphviz-chronology-graph-attribute cfg :labelloc))))
     :edge-attrs
     (list
      (cons :style (<-chron seq :edge verbose))
      (cons :arrowhead (graphviz-chronology-edge-attribute cfg :arrowhead))
      (cons :color (graphviz-chronology-edge-attribute cfg :color))
      (cons :fontname (graphviz-chronology-edge-attribute cfg :fontname))
      (cons :fontsize (graphviz-chronology-edge-attribute cfg :fontsize))
      (cons :fontcolor (graphviz-chronology-edge-attribute cfg :fontcolor))
      (cons :label (constantly (quotes-around ""))))
     :node-attrs
     (list
      (cons :label (graphviz-chronology-label-attribute))
      (cons :shape (<-chron seq :node verbose))
      (cons :fontname (graphviz-chronology-node-attribute cfg :fontname))
      (cons :fontsize (graphviz-chronology-node-attribute cfg :fontsize))
      (cons :color (graphviz-chronology-node-attribute cfg :color))
      (cons :fillcolor (graphviz-chronology-node-attribute cfg :fillcolor))
      (cons :fontcolor (graphviz-chronology-node-attribute cfg :fontcolor))))
    (when verbose (format t "Wrote ~a.~%" out-file))))

(defun load-project (cfg-file &optional (verbose t))
  "Given a path to the user's configuration file, CFG-FILE, read the file,
configure the archaeological sequence, check it for errors, and return it."
  (let ((seq (hm::make-archaeological-sequence))
        (cfg (hm:read-configuration-from-files verbose cfg-file)))
    (hm::configure-archaeological-sequence seq cfg verbose)))

(defun run-sequence (seq &optional (verbose t) display (cmd "open"))
  "Given an archaeological sequence, SEQ, carry out its instructions, and write
  a dot file for the archaeological sequence. If the dot file for the
  archaeological sequence already exists, it will be deleted. If VERBOSE,
  advertise progress and check with the user whether or not to overwrite an
  existing file. If DISPLAY is set to a string with a valid dot file format,
  then run dot and display the resulting graphic file with the command, CMD."
  (let* ((cfg (hm::archaeological-sequence-configuration seq))
         (old-file (probe-file (hm::output-file-name cfg :sequence-dot))))
    (if verbose
        (when (and old-file (y-or-n-p "Overwrite ~a?" (enough-namestring old-file)))
          (uiop:delete-file-if-exists old-file))
        (uiop:delete-file-if-exists old-file))
    (hm::write-sequence-graph-to-dot-file seq verbose)
    (when display
      (let ((map (dot-output-format-map)))
        (unless (fset:domain-contains? map display)
          (error "Error: Graphviz dot does not recognize ~s as an output format.~&"
                 display))
        (hm::make-graphics-file cfg :sequence display :open cmd :verbose verbose)))))

(defun run-chronology (seq &optional (verbose t) display (cmd "open"))
  "Given an archaeological sequence, SEQ, carry out its instructions, and write
  a dot file for the chronology graph. If the dot file for the
  chronology graph already exists, it will be deleted. If VERBOSE,
  advertise progress and check with the user whether or not to overwrite an
  existing file. If DISPLAY is set to a string with a valid dot file format,
  then run dot and display the resulting graphic file with the command, CMD."
  (let* ((cfg (hm::archaeological-sequence-configuration seq))
         (old-file (probe-file (hm::output-file-name cfg :chronology-dot))))
    (if verbose
        (when (and old-file (y-or-n-p "Overwrite ~a?" (enough-namestring old-file)))
          (uiop:delete-file-if-exists old-file))
        (uiop:delete-file-if-exists old-file))
    (if (chronology-graph-p cfg)
        (hm::write-chronology-graph-to-dot-file seq verbose)
        (when verbose
          (format t "The configuration does not ask for a chronology graph.~&")))
    (when display
      (let ((map (dot-output-format-map)))
        (unless (fset:domain-contains? map display)
          (error "Error: Graphviz dot does not recognize ~s as an output format.~&"
                 display))
        (make-graphics-file cfg :chronology display :open cmd :verbose verbose)))))

(defun run-project (cfg-file &key (verbose t) (sequence-display "png")
                               (chronology-display "png") (sequence-cmd "xdg-open")
                               (chronology-cmd "xdg-open") (draw-sequence t)
                               (draw-chronology t) (delete-sequence nil)
                               (delete-chronology nil))
  "* Arguments
 - cfg-file :: A string or pathname.
 - verbose :: Boolean.
 - sequence-display :: A string indicating a Graphviz =dot= output file format.
 - chronology-display :: A string indicating a Graphviz =dot= output file format.
 - sequence-cmd :: A string naming the application used to open the sequence graph.
 - chronology-cmd :: A string naming the application used to open the chronology graph.
 - draw-sequence :: Boolean.
 - draw-chronology :: Boolean.
 - delete-sequence :: Boolean.  Delete the sequence graph file after it is displayed.
 - delete-chronology :: Boolean. Delete the chronology graph file after it is displayed.
* Returns
An archaeological sequence.
* Description
Run the project specified in the user's configuration file, CFG-FILE. If
DRAW-SEQUENCE is non-nil, then create a sequence graph in the format indicated
by SEQUENCE-DISPLAY and open the graphics file with the shell command,
SEQUENCE-CMD. If DELETE-SEQUENCE is non-nil, then delete the graphics file after
it is displayed. If DRAW-CHRONOLOGY is non-nil, then create a sequence graph in
the format indicated by CHRONOLOGY-DISPLAY and open the graphics file with the
shell command, CHRONOLOGY-CMD. If DELETE-CHRONOLOGY is non-nil, then delete the
graphics file after it is displayed. If VERBOSE is non-nil, then advertise
progress.
* Example
#+begin_src lisp
(run-project \"my-config.ini\" :verbose nil :sequence-cmd \"evince\")
#+end_src"
  (memoize-functions)
  (let* ((seq (load-project cfg-file verbose))
         (cfg (archaeological-sequence-configuration seq)))
    (when draw-sequence
      (run-sequence seq verbose sequence-display sequence-cmd)
      (when (and delete-sequence (y-or-n-p "Delete graphics file?"))
        (delete-graphics-file cfg :sequence sequence-display)))
    (when (and draw-chronology (chronology-graph-p cfg))
      (run-chronology seq verbose chronology-display chronology-cmd)
      (when (and delete-chronology (y-or-n-p "Delete graphics file?"))
        (delete-graphics-file cfg :chronology chronology-display)))
    (unmemoize-functions)
    seq))

(defun run-project/example (example &key (verbose t) (sequence-display "png")
                                      (chronology-display "png") (sequence-cmd "xdg-open")
                                      (chronology-cmd "xdg-open") (draw-sequence t)
                                      (draw-chronology t) (delete-sequence t)
                                      (delete-chronology t))
  "* Arguments
 - example :: A keyword, one of :catal-hoyuk, :catal-hoyuk-levels, :catal-hoyuk-distance, :roskams-h, :roskams-h-solarized-light, :roskams-h-solarized-dark, :roskams-jumps, :complex-h-structure, :complex-h-structure-reachable, :fig-12, :fig-12-correlations, :fig-12-periods.
 - verbose :: Boolean.
 - sequence-display :: A string indicating a Graphviz =dot= output file format.
 - chronology-display :: A string indicating a Graphviz =dot= output file format.
 - sequence-cmd :: A string naming the application used to open the sequence graph.
 - chronology-cmd :: A string naming the application used to open the chronology graph.
 - draw-sequence :: Boolean.
 - draw-chronology :: Boolean.
 - delete-sequence :: Boolean.  Delete the sequence graph file after it is displayed.
 - delete-chronology :: Boolean. Delete the chronology graph file after it is displayed.
* Returns
An archaeological sequence.
* Description
Given a keyword, EXAMPLE, that indicates one of the example projects defined
for the =hm= package, run the project described by the appropriate =.ini= file.
* Example
#+begin_src lisp
  (run-project/example :roskams-h :delete-sequence nil)
#+end_src
"

  (let ((cfg-file (case example
                    (:fig-12-polygon
                     (uiop:merge-pathnames*
                      "examples/fig-12-chronology/fig-12-polygon-1.ini"
                      (asdf:system-source-directory :hm)))
                    (:fig-12
                     (uiop:merge-pathnames*
                      "examples/fig-12-chronology/fig-12-chronology.ini"
                      (asdf:system-source-directory :hm)))
                    (:fig-12-correlations
                     (uiop:merge-pathnames*
                      "examples/fig-12-chronology/fig-12-correlations.ini"
                      (asdf:system-source-directory :hm)))
                    (:fig-12-periods
                     (uiop:merge-pathnames*
                      "examples/fig-12-chronology/fig-12-periods.ini"
                      (asdf:system-source-directory :hm)))
                    (:catal-hoyuk-levels
                     (uiop:merge-pathnames*
                      "examples/bldg-1-5/bldg-1-5-levels.ini"
                      (asdf:system-source-directory :hm)))
                    (:catal-hoyuk-reachable
                     (uiop:merge-pathnames*
                      "examples/bldg-1-5/bldg-1-5-reachable-1332+.ini"
                      (asdf:system-source-directory :hm)))
                    (:catal-hoyuk-distance
                     (uiop:merge-pathnames*
                      "examples/bldg-1-5/bldg-1-5-distance-1332+.ini"
                      (asdf:system-source-directory :hm)))
                    (:catal-hoyuk
                     (uiop:merge-pathnames*
                      "examples/bldg-1-5/bldg-1-5-plain.ini"
                      (asdf:system-source-directory :hm)))
                    (:roskams-h
                     (uiop:merge-pathnames*
                      "examples/roskams-h/roskams-h.ini"
                      (asdf:system-source-directory :hm)))
                    (:roskams-h-solarized-light
                     (uiop:merge-pathnames*
                      "examples/roskams-h/roskams-h-solarized-light.ini"
                      (asdf:system-source-directory :hm)))
                    (:roskams-h-solarized-dark
                     (uiop:merge-pathnames*
                      "examples/roskams-h/roskams-h-solarized-dark.ini"
                      (asdf:system-source-directory :hm)))
                    (:roskams-jumps
                     (uiop:merge-pathnames*
                      "examples/roskams-jumps/roskams-jumps.ini"
                      (asdf:system-source-directory :hm)))
                    (:complex-h-structure
                     (uiop:merge-pathnames*
                      "examples/h-structure/complex-h-structure.ini"
                      (asdf:system-source-directory :hm)))
                    (:complex-h-structure-reachable
                     (uiop:merge-pathnames*
                      "examples/h-structure/complex-h-structure-reachable-2.ini"
                      (asdf:system-source-directory :hm)))
                    (t (error "Error: The ~a project is not known.~&" example)))))
    (
     run-project cfg-file :verbose verbose :sequence-display sequence-display
                          :chronology-display chronology-display
                          :sequence-cmd sequence-cmd :chronology-cmd chronology-cmd
                          :draw-sequence draw-sequence :draw-chronology draw-chronology
                          :delete-sequence delete-sequence
                          :delete-chronology delete-chronology)))

(defun show-classifiers ()
  "* Arguments
None.
* Returns
Nothing.  Called for its side effects.
* Description
Write a list of classifiers to standard output.
* Example
#+begin_src lisp
(show-classifiers)
#+end_src"
  (fset:do-set (x (classifiers))
    (format t "~(~A~%~)" x)))

(defun show-classifiable-attributes ()
  "* Arguments
None.
* Returns
Nothing.  Called for its side-effects.
* Description
Write a list of classifiable attributes to standard output.
* Example
#+begin_src lisp
(show-classifiable-attributes)
#+end_src"
  (fset:do-set (x (classifiable-attributes))
    (format t "~(~A~%~)" x)))

(defun show-map (attribute)
  "* Arguments
A keyword, ATTRIBUTE, one of :edge-style, :node-style, :node-shape, or :arrow-shape.
* Returns
Nothing.  Called for its side-effects
* Description
Write a lookup map of attributes to standard output.  Raise an error if ATTRIBUTES is out of range.
* Example
#+begin_src lisp
(show-map :edge-style)
#+end_src"
  (let ((map (case attribute
               (:edge-style (graphviz-edge-style-map))
               (:node-style (graphviz-node-style-map))
               (:node-shape (graphviz-node-shape-map))
               (:arrow-shape (graphviz-arrow-shape-map))
               (t (error "Error: The ~a attribute is not known.~&" attribute)))))
    (fset:do-map (x y map)
      (format t "~A --> ~A~%" x y))))
