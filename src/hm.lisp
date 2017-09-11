;;; hm.lisp

;; Copyright (C) Thomas Dye 2016

;; Licensed under the Gnu Public License Version 3 or later

(in-package #:hm)
(named-readtables:in-readtable lol:lol-syntax)

;; The inferior-shell package should enable hm.lisp to call a script
;; that compiles and displays the dot file output.
;; (require "inferior-shell")

(lol:defmacro! <-dot-edge (o!seq classifier dot-attr)
  "Make functions to configure Graphviz edges."
  `(let* ((g!cfg (archaeological-sequence-configuration ,g!seq))
          (g!graph (archaeological-sequence-graph ,g!seq))
          (g!classifiers (archaeological-sequence-classifiers ,g!seq))
          (g!attribute ,dot-attr)
          (g!map (make-edge-lookup-map g!cfg))
          (g!class-with-node (graphviz-edge-classification g!cfg "classify"))
          (g!value (when ,classifier (sequence-classifier g!cfg ,classifier))))
     (if (or (not ,classifier) (and ,classifier (emptyp g!value))
             (not (fset:domain-contains? g!classifiers g!value)))
         (let ((g!y (process-user-value
                   (lookup-option g!map g!attribute) g!attribute g!value ,g!seq)))
           (constantly (if (emptyp g!y) "" g!y)))
       (cond
        ((string= "units" g!value)
         (lambda (x)
           (let ((g!user-value
                   (if (= 0 (round (fset:@ (fset:@ g!classifiers g!value)
                                           (if (string= "to" g!class-with-node)
                                               (nth 1 x)
                                               (nth 0 x)))))
                       (lookup-option g!map g!value ,classifier "deposit")
                       (lookup-option g!map g!value ,classifier "interface"))))
             (if g!user-value
                 (process-user-value g!user-value ,dot-attr g!value
                                     ,o!seq :element :edge)
                 (error "Unable to set edge function.  User value not set for ~s.~&"
                        ,classifier)))))
        ((string= "adjacent" g!value)
         (lambda (x)
           (let ((g!user-value
                   (case (round (fset:@ (fset:@ g!classifiers g!value)
                                        (if (string= "to" g!class-with-node)
                                            (nth 1 x)
                                            (nth 0 x))))
                     (0 (lookup-option g!map g!value ,classifier "origin"))
                     (1 (lookup-option g!map g!value ,classifier "adjacent"))
                     (t (lookup-option g!map g!value ,classifier "not-adjacent")))))
             (if g!user-value
                 (process-user-value g!user-value ,dot-attr g!value
                                     ,o!seq :element :edge)
               (error "Unable to set edge function.  User value not set for ~s.~&"
                      ,classifier)))))
        ((string= "reachable" g!value)
         (lambda (x)
           (let ((g!user-value
                   (case (round (fset:@ (fset:@ g!classifiers g!value)
                                        (if (string= "to" g!class-with-node)
                                            (nth 1 x)
                                            (nth 0 x))))
                     (0 (lookup-option g!map g!value ,classifier "origin"))
                     (1 (lookup-option g!map g!value ,classifier "adjacent"))
                     (2 (lookup-option g!map g!value ,classifier "reachable"))
                     (t (lookup-option g!map g!value ,classifier "not-reachable")))))
             (if g!user-value
                 (process-user-value g!user-value ,dot-attr g!value
                                     ,o!seq :element :edge)
               (error "Unable to set edge function.  User value not set for ~s.~&"
                      ,classifier)))))
        ((fset:contains? (fset:set "distance" "levels" "periods" "phases")
                         g!value)
         (lambda (x)
           (edge--process-matrix-value
            (fset:@ (fset:@ g!classifiers g!value)
                    (if (string= "to" g!class-with-node)
                        (nth 1 x)
                        (nth 0 x)))
            ,dot-attr
            g!value
            ,o!seq)))
        (t (error "Error: Unable to set edge function.~&"))))))

(lol:defmacro! <-dot-node (o!seq classifier dot-attr)
  `(let* ((g!sequence ,g!seq)
          (g!opt ,classifier)
          (g!attribute ,dot-attr)
          (g!cfg (archaeological-sequence-configuration ,g!seq))
          (g!classifiers (archaeological-sequence-classifiers ,g!seq))
          (g!value (when ,classifier (sequence-classifier g!cfg ,classifier)))
          (map (make-node-lookup-map g!cfg)))
     (if (or (not g!opt) (and g!opt (emptyp g!value))
             (not (fset:domain-contains? g!classifiers g!value)))
         (let ((g!y (process-user-value (lookup-option map g!attribute)
                                      g!attribute g!opt g!sequence)))
           (constantly (if (emptyp g!y) "" g!y)))
       (cond
        ((string= "units" g!value)
         (lambda (x)
           (let ((g!user-value
                   (if (= 0 (round (fset:@ (fset:@ g!classifiers g!value) x)))
                       (lookup-option map g!value g!opt "deposit")
                       (lookup-option map g!value g!opt "interface"))))
             (if (emptyp g!user-value)
                 (error "Unable to set node function.  User value not set for ~s.~&"
                        g!opt)
                 (let ((g!user-value-parsed
                         (parse-integer g!user-value :junk-allowed t)))
                   (process-user-value
                    (if g!user-value-parsed g!user-value-parsed g!user-value)
                    g!attribute g!value g!sequence :element :node))))))
        ((string= "adjacent" g!value)
         (lambda (x)
           (let ((g!user-value
                   (case (round (fset:@ (fset:@ g!classifiers g!value) x))
                     (0 (lookup-option map g!value g!opt "origin"))
                     (1 (lookup-option map g!value g!opt "adjacent"))
                     (t (lookup-option map g!value g!opt "not-adjacent")))))
             (if (emptyp g!user-value)
                 (error "Unable to set node function.  User value not set for ~s.~&"
                        g!opt)
                 (let ((g!user-value-parsed
                         (parse-integer g!user-value :junk-allowed t)))
                   (process-user-value
                    (if g!user-value-parsed g!user-value-parsed g!user-value)
                    g!attribute g!value g!sequence :element :node))))))
        ((string= "reachable" g!value)
         (lambda (x)
           (let ((g!user-value
                   (case (round (fset:@ (fset:@ g!classifiers g!value) x))
                     (0 (lookup-option map g!value g!opt "origin"))
                     (1 (lookup-option map g!value g!opt "adjacent"))
                     (2 (lookup-option map g!value g!opt "reachable"))
                     (t (lookup-option map g!value g!opt "not-reachable")))))
             (if (emptyp g!user-value)
                 (error "Unable to set node function.  User value not set for ~s.~&"
                        g!opt)
                 (let ((g!user-value-parsed
                         (parse-integer g!user-value :junk-allowed t)))
                   (process-user-value
                    (if g!user-value-parsed g!user-value-parsed g!user-value)
                    g!attribute g!value g!sequence :element :node))))))
        ((fset:contains? (fset:set "distance" "levels" "periods" "phases")
                         g!value)
         (lambda (x)
           (node--process-matrix-value (fset:@ (fset:@ g!classifiers g!value) x)
                                       g!attribute g!value g!sequence)))
        (t (error "Error: Unable to set node function.~&"))))))

;; Needs work!
(defun process-user-value (value dot-attr option seq &key element)
  (if (fset:contains? (fset:set "color" "fillcolor" "fontcolor")
                      dot-attr)
      (let* ((section (if (equal element :node)
                          "Graphviz sequence node attributes"
                        "Graphviz sequence edge attributes"))
             (scheme (get-option (archaeological-sequence-configuration seq)
                                 section
                                 "colorscheme"))
             (classifiers (archaeological-sequence-classifiers seq))
             (range (unless (emptyp option)
                      (round (1+ (- (fset:greatest (fset:range (fset:@ classifiers option)))
                                    (fset:least (fset:range (fset:@ classifiers option)))))))))
        (if (and option
                 (not (emptyp scheme)))
            (quotes-around (graphviz-color-string value scheme range))
          ""))
    (quotes-around value)))

(defun edge--process-matrix-value (value attr option seq)
  "Given a VALUE from a graph matrix, a Graphviz attribute, ATTR, an ini file
OPTION, and an archaeological sequence, SEQ, return a properly formatted
Graphviz dot value."
  (let ((range (fset:range (fset:@ (archaeological-sequence-classifiers seq)
                                   option))))
    (cond
     ((string= attr "style")
      (quotes-around (graphviz-edge-style value)))
     ((fset:contains? (fset:set "color" "fontcolor")
                      attr)
      (quotes-around (graphviz-color-string value
                                            (get-option (archaeological-sequence-configuration seq)
                                                        "Graphviz sequence edge attributes"
                                                        "colorscheme")
                                            (1+ (- (fset:greatest range)
                                                   (fset:least range))))))
     ((string= attr "penwidth")
      (let ((interval (- (get-option (archaeological-sequence-configuration seq)
                                     "Graphviz sequence edge attributes"
                                     "penwidth-max"
                                     :type :number)
                         (get-option (archaeological-sequence-configuration seq)
                                     "Graphviz sequence edge attributes"
                                     "penwidth-min"
                                     :type :number)))
            (base (get-option (archaeological-sequence-configuration seq)
                              "Graphviz sequence edge attributes"
                              "penwidth-min"
                              :type :number)))
        (quotes-around (+ base
                          (* value
                             (/ interval
                                (1+ (- (fset:greatest range)
                                       (fset:least range))))))))))))

(defun node--process-matrix-value (value attr option seq)
  "Given a VALUE from a graph matrix, a Graphviz attribute, ATTR, an ini file
OPTION, and an archaeological sequence, SEQ, return a properly formatted
Graphviz dot value."
  (let ((range (fset:range (fset:@ (archaeological-sequence-classifiers seq)
                                   option))))
    (cond
     ((string= attr "shape")
      (quotes-around (graphviz-node-shape value)))
     ((string= attr "style")
      (quotes-around (graphviz-node-style value)))
     ((fset:contains? (fset:set "color" "fillcolor" "fontcolor")
                      attr)
      (let ((scheme (get-option (archaeological-sequence-configuration seq)
                                "Graphviz sequence node attributes"
                                "colorscheme")))
        (quotes-around (graphviz-color-string value
                                              scheme
                                              (1+ (- (fset:greatest range)
                                                     (fset:least range)))))))
     ((string= attr "penwidth")
      (let ((interval (- (get-option (archaeological-sequence-configuration seq)
                                     "Graphviz sequence node attributes"
                                     "penwidth-max"
                                     :type :number)
                         (get-option (archaeological-sequence-configuration seq)
                                     "Graphviz sequence node attributes"
                                     "penwidth-min"
                                     :type :number)))
            (base (get-option (archaeological-sequence-configuration seq)
                              "Graphviz sequence node attributes"
                              "penwidth-min"
                              :type :number)))
        (quotes-around (+ base
                          (* value
                             (/ interval
                                (1+ (- (fset:greatest range)
                                       (fset:least range))))))))))))

;; Needs work, no docstring
(defun <-dot-graph (attribute)
  (quotes-around attribute))

;; From On Lisp, p. 92, a macro for testing macroexpansion

(defmacro mac (expr)
  `(pprint (macroexpand-1 ',expr)))

;; threading macro
;; http://www.teknoids.net/content/immutable-persistent-data-structures-common-lisp

(defmacro -> (x &optional
                (form nil form-supplied-p)
                &rest
                more)
  (if form-supplied-p
      (if more
          `(-> (-> ,x ,form)
               ,@more)
        (if (listp form)
            `(,(car form)
              ,x
              ,@
              (cdr form))
          (list form x)))
    x))

;; passes test
(defun quotes-around (string)
  "Put quotes around STRING for output to dot unless string is already quoted."
  (let ((quote-char #\"))
    (if (and (eq (char string 0) quote-char)
             (eq (char string (1- (length string))) quote-char))
        string
        (concatenate 'string "\"" string "\""))))

;; passes test
(defun graphviz-edge-style (index)
  "Given an integer, INDEX, return a string with a dot edge style."
  (let ((map (-> (fset:empty-map 0)
                 (fset:with 0 "solid")
                 (fset:with 1 "dashed")
                 (fset:with 2 "dotted")
                 (fset:with 3 "bold"))))
    (fset:lookup map (mod index (fset:size map)))))

(defun graphviz-node-style (index)
  "Given an integer, INDEX, return a string with a dot node style."
  (let ((map (-> (fset:empty-map 0)
                 (fset:with 0 "solid")
                 (fset:with 1 "dashed")
                 (fset:with 2 "dotted")
                 (fset:with 3 "bold")
                 (fset:with 4 "rounded")
                 (fset:with 5 "diagonals")
                 (fset:with 6 "filled")
                 (fset:with 7 "filled")
                 (fset:with 8 "striped")
                 (fset:with 9 "wedged"))))
    (fset:lookup map
                 (mod index
                      (fset:size map)))))

(defun graphviz-node-shape (index)
  "Given an integer, INDEX, return a string with a valid dot node
shape."
  (let ((map (-> (fset:empty-map 0)
                 (fset:with 0 "box")
                 (fset:with 1 "polygon")
                 (fset:with 2 "ellipse")
                 (fset:with 3 "egg")
                 (fset:with 4 "triangle")
                 (fset:with 5 "diamond")
                 (fset:with 6 "oval")
                 (fset:with 7 "circle")
                 (fset:with 8 "point")
                 (fset:with 9 "trapezium")
                 (fset:with 10 "parallelogram")
                 (fset:with 11 "house")
                 (fset:with 12 "pentagon")
                 (fset:with 13 "hexagon")
                 (fset:with 14 "septagon")
                 (fset:with 15 "octagon")
                 (fset:with 16 "doublecircle")
                 (fset:with 17 "doubleoctagon")
                 (fset:with 18 "tripleoctagon")
                 (fset:with 19 "invtriangle")
                 (fset:with 20 "invtrapezium")
                 (fset:with 21 "invhouse")
                 (fset:with 22 "Mdiamond")
                 (fset:with 23 "Msquare")
                 (fset:with 24 "Mcircle")
                 (fset:with 25 "square")
                 (fset:with 26 "star")
                 (fset:with 27 "underline")
                 (fset:with 28 "note")
                 (fset:with 29 "tab")
                 (fset:with 30 "folder")
                 (fset:with 31 "box3d")
                 (fset:with 32 "component")
                 (fset:with 33 "cds")
                 (fset:with 34 "signature")
                 (fset:with 35 "rpromoter")
                 (fset:with 36 "rarrow")
                 (fset:with 37 "larrow")
                 (fset:with 38 "lpromoter"))))
    (fset:@ map (mod index (fset:size map)))))

;; internal, tested
(defun graphviz-color-string (index scheme &optional range)
  "Given a colorscheme, SCHEME, optionally an integer, RANGE, that indicates how
  many colors to select from, and a zero-based INDEX, return the Graphviz color
  string. INDEX can be an integer, in which case SCHEME must be the base of a
  Brewer color name, or a string, in which case SCHEME must be `x11', `svg', or
  `solarized', else `x11' will be used."
  (let ((local-scheme scheme)
        (local-range (if (and range (> 3 range)) 3 range)))
    (etypecase index
      (integer (unless (brewer-colorscheme-distinctions scheme :member t)
                 (error "Error: ~s does not indicate a Brewer colorscheme.~&"
                        scheme))
       (unless range
         (error "Error: No range for `graphviz-color-string'.~&"))
       (let ((brewer-range
               (if (> local-range (brewer-colorscheme-distinctions scheme))
                   (brewer-colorscheme-distinctions scheme)
                   local-range)))
         (format nil
                 "/~a~s/~s"
                 scheme
                 brewer-range
                 (1+ (mod index brewer-range)))))
      (string (unless (fset:contains? (fset:set "x11" "svg" "solarized")
                                      scheme)
                (setf local-scheme "x11"))
       (if (string= "solarized" local-scheme)
           (if (solarized-map index :member t)
               (graphviz-hex-color (solarized-map index))
               (error "Error: ~s is not a solarized color name.~&"
                      index))
           (format nil "/~a/~a" local-scheme index))))))

;; internal
(defun color-name-to-rgb (color)
  "Return the rgb representation of the x11 color name COLOR."
  (eval (symbolicate #\+ (string-upcase color) #\+)))

;; internal, tested
(defun graphviz-hsv-string (color)
  "Given a cl-colors package x11 color name COLOR, return the Graphviz HSV
  string that specifies the color."
  (let ((hsv-color (etypecase color
                     (string (rgb-to-hsv (color-name-to-rgb color)))
                     (rgb (as-hsv color)))))
    (format nil "~,3f ~,3f ~,3f"
            (/ (hsv-hue hsv-color) 360)
            (hsv-saturation hsv-color)
            (hsv-value hsv-color))))

;; internal, tested
(defun graphviz-color-from-ramp (index color1 color2 steps)
  "Given two strings, COLOR1 and COLOR2, each with a valid x11 color
  name and the number of STEPS in the ramp, return a Graphviz hsv
  color string."
  (when (> index steps)
    (error "Error: Index out of bounds."))
  (let ((c1 (color-name-to-rgb color1))
        (c2 (color-name-to-rgb color2))
        (alpha (/ index steps)))
    (graphviz-hsv-string (rgb-combination c1 c2 alpha))))

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
    (when verbose (format t "Added nodes to the sequence graph.~&"))
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
    (when verbose (format t "Added arcs to the sequence graph.~&"))
    ret))

(defun make-new-sequence-graph (cfg &optional (verbose t))
  "Given a configuration CFG, make a new digraph instance and populate it with
  nodes and arcs from the files specified in the configuration."
  (let ((new-graph (make-instance 'graph:digraph)))
    (setf new-graph (add-nodes new-graph cfg verbose))
    (setf new-graph (add-arcs new-graph cfg verbose))
    new-graph))

(defun check-cycles (graph)
  "Reports an error when cycles are present in GRAPH."
  (and (graph:cycles graph) (error "Graph contains a cycle")))

(defun assume-correlations (graph cfg &optional (verbose t))
  "Given the information in a configuration CFG, possibly merge and rename the
  nodes of GRAPH. Check for cycles and error out if present, otherwise return
  the possibly modified GRAPH."
  (let ((ret (graph:copy graph))
        (make-assumption (get-option cfg "General configuration"
                                     "assume-correlations" :type :boolean))
        (input-file-name (get-option cfg "Input files" "inferences"))
        (file-header-p (get-option cfg "Input file headers" "inferences"
                                   :type boolean))
        (inferences))
    (when make-assumption
      (if input-file-name
          (setf inferences (read-table input-file-name file-header-p verbose))
          (error "Error: No inference table specified."))
      (dolist (part inferences)
        (graph:merge-nodes ret (symbolicate (nth 1 part))
                           (symbolicate (nth 0 part))
                           :new (correlated-node (nth 0 part) (nth 1 part))))
      (check-cycles ret))
    ret))

;; passes test
(defun correlated-node (node-1 node-2 &optional (as-string nil))
  "Given two correlated node symbols, NODE-1 and NODE-2, return a new
symbol for the correlated nodes.  If AS-STRING is non-nil, return the
correlated node symbol as a string."
  (let ((new-node (symbolicate node-1 "=" node-2)))
    (if as-string (string new-node) new-node)))

(defun graphviz-make-ranks (cfg &optional (verbose t))
  "Returns a list of ranks for Graphviz output if the user's configuration, CFG,
specifies that correlations should be assumed true, nil otherwise."
  (let ((inferences (read-table (input-file-name cfg "inferences")
                                (file-header-p cfg "inferences") verbose))
        (contexts (read-table (input-file-name cfg "contexts")
                              (file-header-p cfg "contexts") verbose))
        (ranks))
    (if (and (assume-correlations-p cfg) (or inferences contexts))
        (progn
          (when inferences
            (appendf ranks (set-same-ranks inferences)))
          (when contexts
            (appendf ranks (set-other-ranks contexts)))
          (when verbose (format t "Ranks set.~&")))
        (when verbose (format t "Ranks not set.~&")))
    ranks))

;; passes test
(defun make-node-index (graph)
  "Returns an fset map where the key is a node of graph GRAPH and the
value is an index into the matrix representation of GRAPH."
  (let ((counter -1)
        (node-index (fset:empty-map)))
    (mapc
     (lambda (node)
       (setf node-index (fset:with node-index (incf counter) node)))
          (graph:nodes graph))
    node-index))

(defun tables-to-map (contexts other-table table-type)
  "Given a CONTEXTS table, an OTHER-TABLE, and a TABLE-TYPE in
`periods' or `phases', return an fset map where the key is the context
and the value is either the period or phase of the context."
  (let ((ht (fset:empty-map))
        (ret (fset:empty-map))
        (n (cond
            ((string= table-type "periods") 3)
            ((string= table-type "phases") 4)
            (t (error "Error: unrecognized table type")))))
    (mapcar #'(lambda (x)
                (setf ht (fset:with ht
                                    (nth 0 x)
                                    (nth 2 x))))
            other-table)
    (mapcar #'(lambda (x)
                (setf ret (fset:with ret
                                     (symbolicate (nth 0 x))
                                     (read-from-string (fset:@ ht
                                                               (nth n x))))))
            contexts)
    ret))

(defun alist-to-map (alist)
  "Given an assoc-list, ALIST, return the corresponding fset map."
  (let ((ret (fset:empty-map)))
    (mapc #'(lambda (pair)
              (setq ret (fset:with ret
                                   (car pair)
                                   (cdr pair))))
          alist)
    ret))

(defun context-type-to-map (contexts)
  "Given a context table, CONTEXTS, return an fset map where the key
  is the context and the value is 0 if the unit-type is `deposit' or 1
  if the unit-type is `interface'."
  (let ((ret (fset:empty-map)))
    (mapcar #'(lambda (x)
                (setf ret
                      (fset:with ret (symbolicate (nth 0 x))
                                 (cond
                                   ((string= (nth 1 x) "deposit") 0)
                                   ((string= (nth 1 x) "interface") 1)))))
            contexts)
    ret))

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
          (length (graph:nodes (archaeological-sequence-graph seq)))
          (length (graph:edges (archaeological-sequence-graph seq)))
          (length (graph:nodes (archaeological-sequence-chronology-graph
                                seq)))
          (length (graph:edges (archaeological-sequence-chronology-graph
                                seq)))
          (if (archaeological-sequence-configuration seq) "yes" "no")
          (fset:size (archaeological-sequence-classifiers seq))))

(defun create-distance-matrix (cfg graph)
  "Returns a distance matrix of the directed graph, GRAPH, using the
instructions in the user's configuration, CFG."
  (graph-matrix:to-distance-matrix
   graph (new-matrix (fast-matrix-p cfg))))

(fmemo:memoize 'create-distance-matrix)

(defun create-reachability-matrix (cfg graph)
  "Returns a reachability matrix of the directed graph, GRAPH, using the
  instructions in the user's configuration, CFG."
  (let ((limit (reachable-limit cfg)))
    (if (or (not limit)(< limit 2))
        (graph-matrix:to-reachability-matrix
         graph (new-matrix (fast-matrix-p cfg)))
        (graph-matrix:to-reachability-matrix
         graph (new-matrix (fast-matrix-p cfg)) :limit limit))))

(fmemo:memoize 'create-reachability-matrix)

(defun create-adjacency-matrix (cfg graph)
  "Returns an adjacency matrix of the directed graph, GRAPH, using the
instructions in the user's configuration, CFG."
  (graph-matrix:to-adjacency-matrix
   graph (new-matrix (fast-matrix-p cfg))))

(fmemo:memoize 'create-adjacency-matrix)

(defun create-strong-component-matrix (cfg graph)
  "Returns a strong-component-matrix of the directed graph, GRAPH, using
  instructions in the user's configuration, CFG."
  (graph-matrix:to-strong-component-matrix
   graph (new-matrix (fast-matrix-p cfg))))

(fmemo:memoize 'create-strong-component-matrix)

(defun configure-archaeological-sequence (seq cfg &optional (verbose t))
  "Configures the archaeological sequence SEQ using the information in
the configuration CFG, and returns the possibly modified
archaeological sequence.  Checks for common configuration
discrepancies and errors out if it finds one."
  (unless (typep seq 'archaeological-sequence) (error "Error: No sequence found."))
  (unless (typep cfg 'config) (error "Error: No configuration found."))
  (configuration-errors? cfg)
  (let ((ret (copy-structure seq))
        (c (fset:set "node-fill-by" "node-shape-by" "node-color-by"
                     "node-penwidth-by" "node-style-by" "node-polygon-distortion-by"
                     "node-polygon-image-by" "node-polygon-orientation-by"
                     "node-polygon-sides-by" "node-polygon-skew-by"
                     "edge-color-by" "edge-fontcolor-by" "edge-penwidth-by"
                     "edge-style-by")))
    (setf (archaeological-sequence-configuration ret) cfg)
    (setf (archaeological-sequence-graph ret) (make-new-sequence-graph cfg verbose))
    (setf (archaeological-sequence-graph ret)
          (add-missing-interfaces (archaeological-sequence-graph ret)
                                  cfg verbose))
    (setf (archaeological-sequence-graph ret)
          (assume-correlations (archaeological-sequence-graph ret) cfg verbose))
    (setf (archaeological-sequence-chronology-graph ret)
          (create-chronology-graph ret verbose))
    (fset:do-set (classifier c)
                 (let ((class (sequence-classifier cfg)))
                   (unless (emptyp class)
                     (when verbose
                       (format t "Making classifier for ~a.~&" class))
                     (setf (archaeological-sequence-classifiers ret)
                           (fset:with
                            (archaeological-sequence-classifiers ret)
                            class (make-classifier class ret verbose))))))
    (when verbose (format t "Configured archaeological sequence.~&"))
    ret))

(defun make-classifier (classifier-type seq &optional (verbose t))
  "Given a string indicating CLASSIFIER-TYPE and an archaeological sequence SEQ
return an fset map where the key is a symbol for a node in the directed graph of
the archaeological sequence and whose value is a number in the range of
CLASSIFIER-TYPE. CLASSIFIER-TYPE is one of `distance', `reachable', `adjacent',
`periods', `phases', `units', or `levels'."
  (let ((cfg (archaeological-sequence-configuration seq))
        (graph (archaeological-sequence-graph seq)))
    (when verbose (format t "Creating %a classification.~&" classifier-type))
    (cond
      ((string= classifier-type "units")
       (context-type-to-map (read-table (input-file-name cfg  "contexts")
                                        (file-header-p cfg  "contexts")
                                        verbose)))
      ((string= classifier-type "distance")
       (let* ((m (create-distance-matrix cfg graph))
              (i (make-node-index graph))
              (o (reachable-from-node cfg))
              (new-key (fset:@ i o))
              (map (fset:empty-map 0)))
         (fset:do-map (key val i)
           (let ((new-val (round (min (graph-matrix:matrix-ref m new-key val)
                                      (graph-matrix:matrix-ref m val new-key)))))
             (unless (graph-matrix:infinitep new-val m)
               (setf map (fset:with map key new-val)))))
         map))
      ((string= classifier-type "reachable")
       (let ((m (create-reachability-matrix cfg graph))
             (i (make-node-index graph))
             (o (reachable-from-node cfg))
             (map (fset:empty-map)))
         (fset:do-map (key val i)
           (setf map
                 (fset:with map key (graph-matrix:matrix-ref m (fset:@ i o) val))))
         map))
      ((string= classifier-type "adjacent")
       (let ((m (create-adjacency-matrix cfg graph))
             (i (make-node-index graph))
             (o (reachable-from-node cfg))
             (map (fset:empty-map)))
         (fset:do-map (key val i)
           (setf map
                 (fset:with map key (graph-matrix:matrix-ref m (fset:@ i o) val))))
         map))
      ((member classifier-type '("periods" "phases") :test 'string=)
       (tables-to-map (read-table (input-file-name cfg "contexts")
                                  (file-header-p cfg "contexts")
                                  verbose)
                      (read-table (input-file-name cfg classifier-type)
                                  (file-header-p cfg classifier-type)
                                  verbose)
                      classifier-type))
      ((string= classifier-type "levels")
       (alist-to-map (graph:levels graph :alist 't)))
      (t (error "The classifier '~a' is not known." classifier-type)))))

(defun create-chronology-graph (seq &optional (verbose t))
  "If the user has requested a chronology graph, then create and return a
chronology graph, given an archaeological sequence, SEQ. Otherwise, return an
empty graph. If VERBOSE, then advertise progress."
  (if (chronology-graph-p (archaeological-sequence-configuration seq))
      (when verbose
        (format t "Creating chronology graph.~&"))
      (progn
        (when verbose (format t "Chronology graph off.~&"))
        (return-from create-chronology-graph (make-instance 'graph:digraph))))
  (let* ((ret (make-instance 'graph:digraph))
         (distance-matrix (create-distance-matrix (archaeological-sequence-configuration seq)
                                                  (archaeological-sequence-graph seq)))
         (cfg (archaeological-sequence-configuration seq))
         (event-table (read-table (input-file-name cfg "events")
                                  (file-header-p cfg "events")
                                  verbose))
         (event-order-table (when (input-file-name-p "event-order")
                              (read-table (input-file-name cfg "event-order")
                                          (file-header-p cfg "event-order")
                                          verbose))))
    ;; If assume-correlations then adjust the event-table and
    ;; event-order table accordingly
    (when (assume-correlations-p cfg)
      (let ((inference-table (read-table (input-file-name cfg "inferences")
                                         (file-header-p cfg "inferences")
                                         verbose))
            (inference-map (fset:empty-map)))
        (dolist (row inference-table)
          (setq inference-map (fset:with inference-map
                                         (nth 0 row)
                                         (correlated-node (nth 0 row)
                                                          (nth 1 row)
                                                          t)))
          (setq inference-map (fset:with inference-map
                                         (nth 1 row)
                                         (correlated-node (nth 0 row)
                                                          (nth 1 row)
                                                          t))))
        (setf event-table (mapcar #'(lambda (row)
                                      (let ((node-1 (fset:lookup inference-map
                                                                 (nth 0 row)))
                                            (node-2 (fset:lookup inference-map
                                                                 (nth 1 row))))
                                        (when node-1
                                          (setf (nth 0 row) node-1))
                                        (when node-2
                                          (setf (nth 1 row) node-2))))
                                  event-table))
        (when event-order-table
          (setf event-order-table
                (mapcar #'(lambda (row)
                            (let ((node-1 (fset:lookup inference-map
                                                       (nth 0 row)))
                                  (node-2 (fset:lookup inference-map
                                                       (nth 1 row))))
                              (when node-1
                                (setf (nth 0 row) node-1))
                              (when node-2
                                (setf (nth 1 row) node-2))))
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
    (let ((i (make-node-index (archaeological-sequence-graph seq)))
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
        (i (make-node-index graph)))
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

(defun solarized-map (name &key member)
  "Given a solarized color NAME, return as values the corresponding
hexadecimal representation string and a boolean indicating whether or
not color-scale was found.  If MEMBER is non-nil, then return nil if
NAME is not a map key and non-nil otherwise."
  (let ((map (-> (fset:empty-map)
                 (fset:with "base03" "002b36")
                 (fset:with "base02" "073642")
                 (fset:with "base01" "586e75")
                 (fset:with "base00" "657b83")
                 (fset:with "base0" "839496")
                 (fset:with "base1" "93a1a1")
                 (fset:with "base2" "eee8d5")
                 (fset:with "base3" "fdf6e3")
                 (fset:with "yellow" "b58900")
                 (fset:with "orange" "cb4b16")
                 (fset:with "red" "dc322f")
                 (fset:with "magenta" "d33682")
                 (fset:with "violet" "6c71c4")
                 (fset:with "blue" "268bd2")
                 (fset:with "cyan" "2aa198")
                 (fset:with "green" "859900"))))
    (if member
        (fset:domain-contains? map name)
      (fset:@ map name))))

(defun brewer-colorscheme-distinctions (color-scale &key member)
  "Given a COLOR-SCALE name as a string, return as values the number
of distinctions as an integer and a boolean indicating whether or not
color-scale was found.  If MEMBER is non-nil, then return nil if
COLOR-SCALE is not a map key and non-nil otherwise."
  (let ((map (-> (fset:empty-map)
                 (fset:with "accent" 8)
                 (fset:with "blues" 9)
                 (fset:with "brbg" 11)
                 (fset:with "bugn" 9)
                 (fset:with "bupu" 9)
                 (fset:with "dark2" 8)
                 (fset:with "gnbu" 9)
                 (fset:with "greens" 9)
                 (fset:with "greys" 9)
                 (fset:with "oranges" 9)
                 (fset:with "orrd" 9)
                 (fset:with "paired" 12)
                 (fset:with "pastel1" 9)
                 (fset:with "pastel2" 8)
                 (fset:with "piyg" 11)
                 (fset:with "prgn" 11)
                 (fset:with "pubu" 9)
                 (fset:with "pubugn" 9)
                 (fset:with "puor" 11)
                 (fset:with "purd" 9)
                 (fset:with "purples" 9)
                 (fset:with "rdbu" 11)
                 (fset:with "rdgy" 11)
                 (fset:with "rdpu" 9)
                 (fset:with "rdylbu" 11)
                 (fset:with "rdylgn" 11)
                 (fset:with "reds" 9)
                 (fset:with "set1" 9)
                 (fset:with "set2" 8)
                 (fset:with "set3" 12)
                 (fset:with "spectral" 11)
                 (fset:with "ylgn" 9)
                 (fset:with "ylgnbu" 9)
                 (fset:with "ylorbr" 9)
                 (fset:with "ylorrd" 9))))
    (if member
        (fset:domain-contains? map color-scale)
      (fset:@ map color-scale))))

(defun graphviz-hex-color (hex)
  "A convenience function to bridge a difference in how cl-colors and
Graphviz represent hexadecimal color specifications.  Given a 6
character hexidecimal string, returns a string where the hexadecimal
specification is prefixed with an octothorp."
  (assert (eq (length hex) 6)
          (hex)
          "Error: ~a is not 6 characters."
          hex)
  (format nil "#~a" hex))

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

(defun read-table (name header &optional (verbose t))
  "Checks that NAME is a file, then attempts to read it as
comma-separated values.  HEADER indicates whether or not the first
line of NAME contains column heads, rather than values.  If VERBOSE,
give notice."
  (if-let (in-file (probe-file (string name)))
      (progn
        (when verbose
          (format t "Reading table ~a.~%" in-file))
        (cl-csv:read-csv in-file :skip-first-p header))
    (error "Unable to read ~a.~&" name)))

;; filter function definitions

;; (defun color-filter (color color-scheme)
;;   "Returns a valid Graphviz dot color designator. The result is either
;; an integer or a symbol with a color name appended to a color space.
;; COLOR can either be an integer, in which case Brewer colors are assumed,
;; or a symbol whose string value is a color name.  Returns 0 if COLOR is
;; nil or a string."
;;   (cond
;;     ((not color) 0)
;;     ((integerp color) color)
;;     ((eq color 'transparent) color)
;;     ((symbolp color) (symbolicate "/" color-scheme "/" color))
;;     (t 0)))

;; (defun shape-filter (shape)
;;   "Returns a valid Graphviz dot node shape as a symbol."
;;   (let ((shapes (graphviz-shape-index-map)))
;;     (cond
;;       ((integerp shape) (symbolicate (fset:@ shapes shape)))
;;       ((stringp shape) (if (emptyp shape) nil
;;                            (symbolicate shape)))
;;       ((symbolp shape) shape)
;;       (t nil))))


;; (defun get-node-urls-from (table inferences)
;;   "Reads URL's from TABLE.  Returns a map where the keys are
;; node labels and the values are symbols for URL's."
;;   (let ((ret (fset:empty-map)))
;;     (dolist (node table)
;;       (setf ret (fset:with ret (new-symbol (first node))
;;                            (if (string= (sixth node) "")
;;                                (if *url-default* *url-default* "")
;;                                (do-urlencode:urlencode (sixth node))))))
;;     (when *assume-correlations-true*
;;       (dolist (part inferences)
;;         (setf ret (fset:with ret (read-from-string
;;                                   (format nil "~a=~a" (first part) (second part)))
;;                              (if (string= (third part) "")
;;                                  (if *url-default*  *url-default* "")
;;                                  (do-urlencode:urlencode  (third part)))))))
;;     ret))

;; (defun get-arc-urls-from (table)
;;   "Reads URL's from TABLE.  Returns a map where the keys are
;; arcs and the values are URL's."
;;   (let ((ret (fset:empty-map)))
;;     (dolist (arc table)
;;       (setf ret
;;             (fset:with ret (list (new-symbol (first arc))
;;                                  (new-symbol (second arc)))
;;                        (if (string= (third arc) "")
;;                            (if *url-default* *url-default* "")
;;                            (do-urlencode:urlencode (third arc))))))
;;     ret))

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


;; Note the function (constantly) will be useful in the drawing section.

;; (defun hm-draw (cnf-file-path &optional style-file-path)
;;   "Read a configuration file and various data files, create a
;; stratigraphy graph and optionally a chronology graph, and write one or
;; more dot files according to the variables contained in the
;; configuration file."

;;   (initialize-special-variables)

;;   (let ((rejected)
;;         (node-fills (fset:empty-map))
;;         (node-shapes (fset:empty-map))
;;         (node-colors (fset:empty-map))
;;         (arc-urls (fset:empty-map))
;;         (node-urls (fset:empty-map))
;;         (chronology-graph)
;;         (context-list)
;;         (ranks)
;;         (node-index-hash (make-hash-table))
;;         (attributes)
;;         (context-table)
;;         (observation-table)
;;         (inference-table)
;;         (period-table)
;;         (phase-table)
;;         (event-table)
;;         (event-order-table)
;;         (graph (graph:populate (make-instance 'graph:digraph)))
;;         (cycle-graph (graph:populate (make-instance 'graph:digraph)))
;;         (adjacency-matrix)
;;         (reachability-matrix)
;;         (distance-matrix)
;;         (node-filler)
;;         (node-shaper)
;;         (node-colorer))


;;     ;; conditionally read style file
;;     (when style-file-path
;;       (unless (read-configuration-file style-file-path)
;;         (return-from hm-draw
;;           (format nil "Unable to read style file ~a" style-file-path))))

;;     ;; read configuration file

;;     (unless (read-configuration-file cnf-file-path)
;;       (return-from hm-draw
;;         (format nil "Unable to read configuration file ~a" cnf-file-path)))

;;     ;; check for some user configuration errors
;;     (setf attributes
;;           (-> (fset:empty-set)
;;               (fset:with *node-fill-by*)
;;               (fset:with *node-shape-by*)
;;               (fset:with *node-color-by*)))

;;     (when (and *create-chronology-graph* *assume-correlations-true*)
;;       (return-from hm-draw
;;         "Cannot create chronology graph when *assume-correlations-true*"))

;;     (when (and (not *reachable-from*) (eq *node-fill-by* 'reachable))
;;       (return-from hm-draw
;;         "When *node-fill-by* is 'reachable' *reachable-from* must be
;;           specified."))

;;     (when (and (not *reachable-from*) (eq *node-fill-by* 'distance))
;;       (return-from hm-draw
;;         "When *node-fill-by* is 'distance' *reachable-from* must be
;;           specified."))

;;     (when *symbolize-unit-type*
;;       (setf *node-shape-by* (new-symbol "units" nil)))

;;     ;; read required tables

;;     (unless
;;         (setf context-table
;;               (read-table *context-table-name* *context-table-header*))
;;       (return-from hm-draw (format nil "Unable to read ~a"
;;                                    *context-table-name*)))

;;     (unless
;;         (setf observation-table
;;               (read-table *observation-table-name*
;;                           *observation-table-header*))
;;       (return-from hm-draw (format nil "Unable to read ~a"
;;                                    *observation-table-name*)))

;;     ;; read optional tables, if necessary

;;     (when *inference-table-name*
;;       (unless
;;           (setf inference-table
;;                 (read-table *inference-table-name*
;;                             *inference-table-header*))
;;         (return-from hm-draw (format nil "Unable to read ~a"
;;                                      *inference-table-name*))))

;;     (when *period-table-name*
;;       (unless
;;           (setf period-table
;;                 (read-table *period-table-name* *period-table-header*))
;;         (return-from hm-draw (format nil "Unable to read ~a"
;;                                      *period-table-name*))))

;;     (when *phase-table-name*
;;       (unless
;;           (setf phase-table
;;                 (read-table *phase-table-name* *phase-table-header*))
;;         (return-from hm-draw (format nil "Unable to read ~a"
;;                                      *phase-table-name*))))

;;     (when (and *event-table-name* *create-chronology-graph*)
;;       (unless
;;           (setf event-table
;;                 (read-table *event-table-name* *event-table-header*))
;;         (return-from hm-draw (format nil "Unable to read ~a"
;;                                      *event-table-name*))))

;;     (when (and *event-order-table-name* *create-chronology-graph*)
;;       (unless
;;           (setf event-order-table
;;                 (read-table *event-order-table-name* *event-order-table-header*))
;;         (return-from hm-draw (format nil "Unable to read ~a"
;;                                      *event-order-table-name*))))

;;     ;; create sequence diagram graph

;; ;;; add nodes
;;     (format t "Adding nodes to graph.~&")
;;     (dolist (node context-table)
;;       (graph:add-node graph (new-symbol (first node))))

;;     ;; Check here

;; ;;; add arcs
;;     (format t "Adding arcs to graph.~&")
;;     (dolist (arc observation-table)
;;       (graph:add-edge graph
;;                       (list (new-symbol (first arc))
;;                             (new-symbol (second arc)))))

;;     (when (graph:cycles graph)
;;       (format t
;;               "A cycle has been found.~&")
;;       (dolist (arc observation-table rejected)
;;         (graph:add-edge cycle-graph
;;                         (list (new-symbol (first arc))
;;                               (new-symbol (second arc))))
;;         (unless rejected
;;           (and (graph:cycles cycle-graph) (push arc rejected))))

;;       ;; Report the cycle and shut down

;;       (when rejected
;;         (let ((bad-arc (pop rejected)))
;;           (return-from hm-draw
;;             (values
;;              (format t "A cycle that includes ~a and ~a is present."
;;                      (first bad-arc) (second bad-arc))
;;              graph)))))

;;     ;; set ranks of nodes
;;     ;; possibly assume correlated contexts once-whole
;;     ;; check for cycles

;;     (if *assume-correlations-true*
;;         (progn
;;           (dolist (part inference-table)
;;             (graph:merge-nodes
;;              graph (new-symbol (second part))
;;              (new-symbol (first part))
;;              :new (symbolicate (new-symbol (first part)) "="
;;                                           (new-symbol (second part)))))
;;           (when (graph:cycles graph)
;;             (return-from hm-draw
;;               (format t "Correlated contexts introduced a cycle."))))
;;         (appendf ranks (set-same-ranks inference-table)))

;;     (format t "Setting ranks.~&")
;;     (appendf ranks (set-other-ranks context-table))

;;     (when (fset:contains? attributes 'distance)
;;       (format t "Creating distance matrix.~&")
;;       (setf distance-matrix (create-distance-matrix graph distance-matrix)))

;;     (when (fset:contains? attributes 'reachable)
;;       (format t "Creating reachability matrix.")
;;       (setf reachability-matrix
;;             (create-reachability-matrix graph reachability-matrix
;;                                         *reachable-limit*)))

;;     ;; optionally, fill nodes

;;     (when *node-fill-by*
;;       (format t "Creating node filler.~&")
;;       (setf node-filler
;;             (make-instance
;;              *node-fill-by*
;;              :table (if (eq *node-fill-by* 'periods) period-table phase-table)
;;              :contexts context-table
;;              :matrix (if (eq *node-fill-by* 'reachable)
;;                          reachability-matrix distance-matrix)
;;              :legend-node-shape *shape-node-legend*
;;              :legend-node-fill *color-fill-node-legend*
;;              :legend-node-color *color-node-legend*
;;              :origin-node *reachable-from*
;;              :origin *color-origin*
;;              :adjacent *color-adjacent*
;;              :reachable *color-reachable*
;;              :unreachable *color-not-reachable*
;;              :deposit *color-fill-node-deposit*
;;              :interface *color-fill-node-interface*))
;;       (setf node-fills (fset:map (fset:$ node-fills)
;;                                  (fset:$ (classify-by node-filler graph)))))

;;     ;; optionally, set node shapes

;;     (when *node-shape-by*
;;       (format t "Creating node shaper.~&")
;;       (setf node-shaper
;;             (make-instance
;;              *node-shape-by*
;;              :table (if (eql *node-shape-by* 'periods) period-table phase-table)
;;              :contexts context-table
;;              :matrix (if (eql *node-shape-by* 'reachable)
;;                          reachability-matrix distance-matrix)
;;              :legend-node-shape *shape-node-legend*
;;              :legend-node-fill *color-fill-node-legend*
;;              :legend-node-color *color-node-legend*
;;              :origin-node *reachable-from*
;;              :origin *shape-origin*
;;              :adjacent *shape-adjacent*
;;              :reachable *shape-reachable*
;;              :unreachable *shape-not-reachable*
;;              :deposit *shape-node-deposit*
;;              :interface *shape-node-interface*))
;;       (setf node-shapes
;;             (fset:map (fset:$ node-shapes)
;;                       (fset:$ (classify-by node-shaper graph)))))

;;     ;; optionally, color nodes

;;     (when *node-color-by*
;;       (format t "Creating node colorer.~&")
;;       (setf node-colorer
;;             (make-instance
;;              *node-color-by*
;;              :table (if (eql *node-color-by* 'periods) period-table phase-table)
;;              :contexts context-table
;;              :matrix (if (eql *node-color-by* 'reachable)
;;                          reachability-matrix distance-matrix)
;;              :legend-node-shape *shape-node-legend*
;;              :legend-node-fill *color-fill-node-legend*
;;              :legend-node-color *color-node-legend*
;;              :origin-node *reachable-from*
;;              :origin *color-origin*
;;              :adjacent *color-adjacent*
;;              :reachable *color-reachable*
;;              :unreachable *color-not-reachable*
;;              :deposit *color-node-deposit*
;;              :interface *color-node-interface*))
;;       (setf node-colors
;;             (fset:map (fset:$ node-colors)
;;                       (fset:$ (classify-by node-colorer graph)))))

;;     ;; optionally, add legend information

;;     (when *legend*
;;       (format t "Creating legend.~&")
;;       (when *node-fill-by*
;;         (add-legend-nodes node-filler graph)
;;         (add-legend-edges node-filler graph)
;;         (setf node-fills
;;               (fset:map
;;                (fset:$ node-fills)
;;                (fset:$ (legend-fill node-filler))))
;;         (when (and *shape-node-legend* (not (eq *node-shape-by* *node-fill-by*)))
;;           (setf node-shapes
;;                 (fset:map
;;                  (fset:$ node-shapes)
;;                  (fset:$ (legend-shape node-filler :default t)))))
;;         (when (and *color-node-legend* (not (eq *node-color-by* *node-fill-by*)))
;;           (setf node-colors
;;                 (fset:map
;;                  (fset:$ node-colors)
;;                  (fset:$ (legend-color node-filler :default t)))))
;;         (when *url-include*
;;           (setf node-urls
;;                 (fset:map (fset:$ node-urls)
;;                           (fset:$ (legend-urls node-filler))))))

;;       (when *node-shape-by*
;;         (add-legend-nodes node-shaper graph)
;;         (add-legend-edges node-shaper graph)
;;         (setf node-shapes
;;               (fset:map
;;                (fset:$ node-shapes)
;;                (fset:$ (legend-shape node-shaper))))
;;         (when (and *color-fill-node-legend*
;;                    (not (eq *node-shape-by* *node-fill-by*)))
;;           (setf node-fills
;;                 (fset:map
;;                  (fset:$ node-fills)
;;                  (fset:$ (legend-fill node-shaper :default t)))))
;;         (when (and *color-node-legend* (not (eq *node-shape-by* *node-color-by*)))
;;           (setf node-colors
;;                 (fset:map
;;                  (fset:$ node-colors)
;;                  (fset:$ (legend-color node-shaper :default t)))))
;;         (when *url-include*
;;           (setf node-urls
;;                 (fset:map (fset:$ node-urls)
;;                           (fset:$ (legend-urls node-shaper))))))

;;       (when *node-color-by*
;;         (add-legend-nodes node-colorer graph)
;;         (add-legend-edges node-colorer graph)
;;         (setf node-colors
;;               (fset:map
;;                (fset:$ node-colors)
;;                (fset:$ (legend-color node-colorer))))
;;         (when (and *color-fill-node-legend*
;;                    (not (eq *node-color-by* *node-fill-by*)))
;;           (setf node-fills
;;                 (fset:map
;;                  (fset:$ node-fills)
;;                  (fset:$ (legend-fill node-colorer :default t)))))
;;         (when (and *shape-node-legend* (not (eq *node-color-by* *node-shape-by*)))
;;           (setf node-shapes
;;                 (fset:map
;;                  (fset:$ node-shapes)
;;                  (fset:$ (legend-shape node-colorer :default t)))))
;;         (when *url-include*
;;           (setf node-urls
;;                 (fset:map (fset:$ node-urls)
;;                           (fset:$ (legend-urls node-colorer)))))))

;;     ;; optionally, add url information

;;     (when *url-include*
;;       (setf node-urls
;;             (fset:map (fset:$ node-urls)
;;                       (fset:$ (get-node-urls-from context-table inference-table)))
;;             arc-urls
;;             (fset:map (fset:$ arc-urls)
;;                       (fset:$ (get-arc-urls-from observation-table)))))

;;     ;; optionally, construct the chronology graph

;;       ;; write the dot file for the chronology graph

;;       (graph-dot:to-dot-file
;;        chronology-graph (string-downcase (string *output-file-chronology*))
;;        :attributes
;;        (list
;;         (cons :style (format-attribute *graph-style-chronology*))
;;         (cons :colorscheme (format-attribute *color-scheme-chronology*))
;;         (cons :dpi (format-attribute *graph-dpi-chronology*))
;;         (cons :margin (format-attribute *graph-margin-chronology*))
;;         (cons :bgcolor (format-attribute
;;                         (color-filter *color-fill-graph-chronology*)
;;                         :quote t))
;;         (cons :fontname (format-attribute *font-name-graph-chronology*))
;;         (cons :fontsize (format-attribute *font-size-graph-chronology*))
;;         (cons :fontcolor (format-attribute
;;                           (color-filter *font-color-graph-chronology*)
;;                           :quote t))
;;         (cons :splines (format-attribute *graph-splines-chronology*))
;;         (cons :page (format-attribute *graph-page-chronology*))
;;         (cons :size (format-attribute *graph-size-chronology*))
;;         (cons :ratio (format-attribute *graph-ratio-chronology*))
;;         (cons :label (format-attribute *graph-title-chronology*
;;                                        :quote t :preserve-case t))
;;         (cons :labelloc (format-attribute *graph-labelloc-chronology*)))
;;        :edge-attrs
;;        (list
;;         (cons :style
;;               (lambda (e)
;;                 (case (graph:edge-value chronology-graph e)
;;                   (0 (format-attribute *edge-date-chronology*))
;;                   (1 (format-attribute *edge-abutting-chronology*))
;;                   (2 (format-attribute *edge-separated-chronology*)))))
;;         (cons :label (lambda (x) (format-attribute nil)))
;;         (cons :arrowhead (lambda (x) (format-attribute *edge-arrowhead-chronology*)))
;;         (cons :colorscheme (lambda (x) (format-attribute *color-scheme-chronology*)))
;;         (cons :color (lambda (x) (format-attribute (color-filter *color-edge-chronology*)
;;                                               :quote t)))
;;         (cons :fontname (lambda (x) (format-attribute *font-name-edge-chronology*)))
;;         (cons :fontsize (lambda (x) (format-attribute *font-size-edge-chronology*)))
;;         (cons :fontcolor (lambda (x) (format-attribute
;;                                  (color-filter *font-color-edge-chronology*)
;;                                  :quote t))))
;;        :node-attrs
;;        (list
;;         (cons :label (lambda (x) (chronology-graph-html-label x *font-size-subscript*)))
;;         (cons :shape
;;               (lambda (n)
;;                 (if (equal (char (symbol-name n) 0) #\t)
;;                     (format-attribute *shape-date*)
;;                     (format-attribute *shape-phase*))))
;;         (cons :style (lambda (x) (format-attribute *style-node-chronology*)))
;;         (cons :fontname (lambda (x) (format-attribute *font-name-node-chronology*)))
;;         (cons :fontsize (lambda (x) (format-attribute *font-size-node-chronology*)))
;;         (cons :colorscheme (lambda (x) (format-attribute *color-scheme-chronology*)))
;;         (cons :color (lambda (x) (format-attribute
;;                              (color-filter *color-node-chronology*) :quote t)))
;;         (cons :fillcolor (lambda (x) (format-attribute
;;                                  (color-filter *color-fill-node-chronology*)
;;                                  :quote t)))
;;         (cons :fontcolor (lambda (x) (format-attribute
;;                                  (color-filter *font-color-node-chronology*)
;;                                  :quote t)))))
;;       (format t "Wrote ~a~%"
;;               (probe-file (string-downcase (string *output-file-chronology*)))))

;;     ;; write the dot file for the sequence diagram

(defun write-sequence-graph-to-dot-file (seq &optional (verbose t))
  "Write a sequence graph to a Graphviz dot file, based on the information in the archaeological sequence, SEQ."
  (let ((cfg (archaeological-sequence-configuration seq))
        (graph (archaeological-sequence-graph seq))))
  (graph-dot:to-dot-file
   graph (output-file-name cfg "sequence-dot")
   :ranks (graphviz-make-ranks cfg)
   :attributes (list
                (cons :style (<-dot-graph
                              (graphviz-sequence-graph-attribute cfg "style")))
                (cons :dpi (<-dot-graph
                            (graphviz-sequence-graph-attribute cfg "dpi")))
                (cons :URL (<-dot-graph (if (include-url-p cfg)
                                            (default-url cfg) "")))
                (cons :margin (<-dot-graph
                               (graphviz-sequence-graph-attribute cfg "margin")))
                (cons :bgcolor (<-dot-graph
                                (user-color
                                 cfg "Graphviz sequence graph attributes"
                                 "bgcolor")))
                (cons :fontname (<-dot-graph
                                 (graphviz-sequence-graph-attribute
                                  cfg "fontname")))
                (cons :fontsize (<-dot-graph
                                 (graphviz-sequence-graph-attribute
                                  cfg "fontsize")))
                (cons :fontcolor (<-dot-graph
                                  (user-color cfg
                                              "Graphviz sequence graph attributes"
                                              "fontcolor")))
                (cons :splines (<-dot-graph
                                (graphviz-sequence-graph-attribute cfg "splines")))
                (cons :page (<-dot-graph
                             (graphviz-sequence-graph-attribute cfg "page")))
                (cons :size (<-dot-graph
                             (graphviz-sequence-graph-attribute cfg "size")))
                (cons :ratio (<-dot-graph
                              (graphviz-sequence-graph-attribute cfg "ratio")))
                (cons :label (<-dot-graph
                              (graphviz-sequence-graph-attribute cfg "label")))
                (cons :labelloc (<-dot-graph
                                 (graphviz-sequence-graph-attribute
                                  cfg "labelloc"))))
   :edge-attrs (list (cons :style (<-dot-edge seq "edge-style-by" "style"))
                     (cons :arrowhead (<-dot-edge seq nil "arrowhead"))
                     (cons :color (<-dot-edge seq "edge-color-by" "color"))
                     (cons :fontname (<-dot-edge seq nil "fontname"))
                     (cons :fontsize (<-dot-edge seq nil "fontsize"))
                     (cons :fontcolor
                           (<-dot-edge seq "edge-fontcolor-by" "fontcolor"))
                     (cons :penwidth (<-dot-edge seq "edge-penwidth-by" "penwidth"))
                     ;; (cons :URL (lambda (x) (format-attribute
                     ;;                    (if *url-include* (url-decode (fset:@ arc-urls x)) "")
                     ;;                    :preserve-case t :quote t))))
                     )
   :node-attrs
   (list (cons :shape (<-dot-node seq "node-shape-by" "shape"))
         (cons :style (<-dot-node seq "node-style-by" "style"))
         (cons :fontname (<-dot-node seq nil "fontname"))
         (cons :fontsize (<-dot-node seq nil "fontsize"))
         (cons :color (<-dot-node seq "node-color-by" "color"))
         (cons :fillcolor (<-dot-node seq "node-fill-by" "fillcolor"))
         (cons :fontcolor (<-dot-node seq nil "fontcolor"))
         (cons :penwidth (<-dot-node seq "node-penwidth-by" "penwidth"))
         ;; (cons :URL (lambda (x) (format-attribute
         ;;                    (if *url-include*
         ;;                        (url-decode (fset:@ node-urls x))
         ;;                        "") :preserve-case t :quote t)))
         ))
  (when verbose (format t "Wrote ~a.~%"
                        (probe-file (output-file-name cfg "sequence-dot"))))
  (archaeological-sequence-graph seq))

(defun write-levels (graph out-file)
  (let ((levels (graph:levels graph)))
    (with-open-file (stream out-file :direction :output
                            :if-exists :overwrite
                            :if-does-not-exist :create)
                    (maphash (lambda (key value)
                               (cl-csv:write-csv-row (list key value)
                                                     :stream stream))
                             levels))))
