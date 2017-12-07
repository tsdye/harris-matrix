;;; hm-data-structures.lisp

;; Copyright (C) Thomas Dye 2017

;; Licensed under the Gnu Public License Version 3 or later

(in-package #:hm)

(defun classifiers ()
  "Returns an fset set of strings that are the hm classifiers in the standard hm
configuration."
  (let ((opts (options (empty-configuration) "Graphviz sequence classification"))
        (ret (fset:empty-set)))
    (dolist (opt opts)
      (setf ret (fset:with ret opt)))
    ret))

(defun matrix-classes ()
  "hm classifications that require a graph matrix."
  (fset:set :distance :reachable :adjacent))

(defun vector-classes ()
  "hm classifications that require a vector."
  (fset:set :levels :units :periods :phases))

(defun color-attributes ()
  "Graphviz attributes that expect a color string."
  (fset:set :color :fillcolor :fontcolor))

(defun category-attributes ()
  "Graphviz attributes that expect a category string."
  (fset:set :style :shape :arrowhead :polygon-sides :polygon-image))

(defun numeric-attributes ()
  "Graphviz attributes that expect a numeric value."
  (fset:set :fontsize :penwidth :polygon-skew :polygon-orientation
            :polygon-distortion))

(defun create-distance-matrix (seq)
  "Returns a distance matrix of the directed graph using the instructions in the
configured sequence, SEQ."
  (let ((graph (archaeological-sequence-graph seq))
        (cfg (archaeological-sequence-configuration seq)))
    (graph-matrix:to-distance-matrix graph (new-matrix (fast-matrix-p cfg)))))

(defun create-reachability-matrix (seq)
  "Returns a reachability matrix of the directed graph, GRAPH, using the
  instructions in the user's configuration, CFG."
  (let ((limit (reachable-limit (archaeological-sequence-configuration seq)))
        (graph (archaeological-sequence-graph seq))
        (cfg (archaeological-sequence-configuration seq)))
    (if (or (not limit) (< limit 2))
        (graph-matrix:to-reachability-matrix
         graph (new-matrix (fast-matrix-p cfg)))
        (graph-matrix:to-reachability-matrix
         graph (new-matrix (fast-matrix-p cfg)) :limit limit))))

(defun create-adjacency-matrix (seq)
  "Returns an adjacency matrix of the directed graph, GRAPH, using the
instructions in the user's configuration, CFG."
  (let ((graph (archaeological-sequence-graph seq))
          (cfg (archaeological-sequence-configuration seq)))
    (graph-matrix:to-adjacency-matrix graph (new-matrix (fast-matrix-p cfg)))))

;; (defun create-strong-component-matrix (seq)
;;   "Returns a strong-component-matrix of the directed graph, GRAPH, using
;;   instructions in the user's configuration, CFG."
;;   (let ((graph (archaeological-sequence-graph seq))
;;         (cfg (archaeological-sequence-configuration seq)))
;;     (graph-matrix:to-strong-component-matrix
;;      graph (new-matrix (fast-matrix-p cfg)))))

(defun max-value (matrix)
  "Return the maximum value in a matrix."
  (let ((rows (graph-matrix:matrix-n-rows matrix))
        (cols (graph-matrix:matrix-n-cols matrix))
        (max-val 0))
    (loop :for i :below rows :do
      (loop :for j :below cols :do
        (let ((this-val (graph-matrix:matrix-ref matrix i j)))
          (when (and (not (graph-matrix:infinitep this-val matrix))
                     (> this-val max-val))
            (setf max-val this-val)))))
    max-val))

(defun tables-to-map (contexts other-table table-type)
  "Given a CONTEXTS table, an OTHER-TABLE, and a TABLE-TYPE in
`periods' or `phases', return an fset map where the key is the context
and the value is either the period or phase of the context."
  (let ((ht (fset:empty-map))
        (ret (fset:empty-map))
        (n (cond
            ((equal table-type "periods") 3)
            ((equal table-type "phases") 4)
            (t (error "Error: unrecognized table type")))))
    (mapcar #'(lambda (x)
                (setf ht (fset:with ht (nth 0 x) (nth 2 x))))
            other-table)
    (mapcar #'(lambda (x)
                (setf ret
                      (fset:with ret (symbolicate (nth 0 x))
                                 (read-from-string (fset:@ ht (nth n x))))))
            contexts)
    ret))

(defun alist-to-map (alist)
  "Given an assoc-list, ALIST, return the corresponding fset map."
  (let ((ret (fset:empty-map)))
    (mapc #'(lambda (pair)
              (setq ret (fset:with ret (car pair) (cdr pair))))
          alist)
    ret))

(defun context-type-to-map (contexts)
  "Given a context table, CONTEXTS, return an fset map where the key is the
  context and the value is the keyword :deposit if the unit-type is `deposit' or
  :interface if the unit-type is `interface'."
  (let ((ret (fset:empty-map)))
    (mapcar #'(lambda (x)
                (setf ret
                      (fset:with ret (symbolicate (nth 0 x))
                                 (cond
                                   ((equal (nth 1 x) "deposit") :deposit)
                                   ((equal (nth 1 x) "interface") :interface)
                                   (t (error "Error: ~s is not a context type."
                                             (nth 1 x)))))))
            contexts)
    ret))

(defun make-classifier (classifier-type seq &optional (verbose t))
  "Given a keyword indicating CLASSIFIER-TYPE and an archaeological sequence SEQ
return an fset map where the key is a symbol for a node in the directed graph of
the archaeological sequence and whose value is a number or keyword depending on
CLASSIFIER-TYPE. CLASSIFIER-TYPE is one of: :distance, in which case the value
is a non-negative integer; :reachable, in which case value is one
of :origin, :reachable, or :not-reachable; :adjacent, in which case value is
one of :origin, :adjacent, or :not-adjacent; :periods, in which case value is
an integer; :phases, in which case value is an integer; :units, in which case
value is one of :deposit or :interface; or :levels, in which case value is a
non-negative integer."
  (let ((cfg (archaeological-sequence-configuration seq))
        (graph (archaeological-sequence-graph seq)))
    (when verbose (format t "Creating ~a classification.~&" classifier-type))
    (cond
      ((eq classifier-type :units)
       (context-type-to-map (read-table (input-file-name cfg  "contexts")
                                        (file-header-p cfg  "contexts")
                                        verbose)))
      ((eq classifier-type :distance)
       (let* ((m (create-distance-matrix seq))
              (i (graph:nodes graph))
              (o (distance-from-node cfg))
              (unreachable (+ 1 (truncate (max-value m))))
              (map (fset:empty-map)))
         (dolist (key i)
           (let ((new-val (truncate (graph-matrix:distance-from-to graph m o key))))
             (if (graph-matrix:infinitep new-val m)
                 (setf map (fset:with map key unreachable))
                 (setf map (fset:with map key new-val)))))
         map))
      ((eq classifier-type :reachable)
       (let ((m (create-reachability-matrix seq))
             (o (reachable-from-node cfg))
             (map (fset:empty-map)))
         (unless o (error "Error: Configuration lacks reachable from node."))
         (dolist (x (graph:nodes graph))
           (if (eq x o)
               (setf map (fset:with map x :origin))
               (if (graph-matrix:reachablep graph m o x)
                   (setf map (fset:with map x :reachable))
                   (setf map (fset:with map x :not-reachable)))))
         map))
      ((eq classifier-type :adjacent)
       (let ((m (create-adjacency-matrix seq))
             (i (map-node-to-index graph))
             (o (adjacent-from-node cfg))
             (map (fset:empty-map)))
         (unless o (error "Error: Configuration lacks adjacent origin."))
         (fset:do-map (key index i)
           (if (eq key o)
               (setf map (fset:with map key :origin))
               (setf map
                     (fset:with map key
                                (case (truncate
                                       (graph-matrix:matrix-ref m (fset:@ i o) index))
                                  (0 :not-adjacent)
                                  (1 :adjacent)
                                  (otherwise :not-adjacent))))))
         map))
      ((member classifier-type '(:periods :phases) :test 'eq)
       (tables-to-map (read-table (input-file-name cfg "contexts")
                                  (file-header-p cfg "contexts")
                                  verbose)
                      (read-table (input-file-name cfg classifier-type)
                                  (file-header-p cfg classifier-type)
                                  verbose)
                      classifier-type))
      ((eq classifier-type :levels)
       (alist-to-map (graph:levels graph :alist 't)))
      (t (error "The classifier '~a' is not known." classifier-type)))))

(defun map-node-to-index (graph)
  "Returns an fset map where the key is a node of graph GRAPH and the
value is an index into the matrix representation of GRAPH."
  (let ((counter -1)
        (node-index (fset:empty-map)))
    (mapc
     (lambda (node)
       (setf node-index (fset:with node-index node (incf counter))))
     (graph:nodes graph))
    node-index))

(defun map-index-to-node (graph)
  "Returns an fset map where the key is a node of graph GRAPH and the
value is an index into the matrix representation of GRAPH."
  (let ((counter -1)
        (node-index (fset:empty-map)))
    (mapc
     (lambda (node)
       (setf node-index (fset:with node-index (incf counter) node)))
     (graph:nodes graph))
    node-index))

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

(defun make-map (seq element dot-attr graph-type user-class &optional (verbose t))
  "Return a closure for the classification, USER-CLASS, that can be passed
directly to graph-dot for the attribute, DOT-ATTR, of the graph ELEMENT. ELEMENT
is one of `node', `edge'."
  (cond
    ((eq user-class :distance)
     (make-distance-map seq element dot-attr graph-type user-class verbose))
    ((eq user-class :adjacent)
     (make-adjacent-map seq element dot-attr graph-type user-class verbose))
    ((eq user-class :reachable)
     (make-reachable-map seq element dot-attr graph-type user-class verbose))
    ((eq user-class :units)
     (make-units-map seq element dot-attr user-class verbose))
    ((eq user-class :levels)
     (make-levels-map seq element dot-attr graph-type user-class verbose))
    ((eq user-class :phases) (make-phases-map seq element dot-attr user-class verbose))
    ((eq user-class :periods) (make-periods-map seq element dot-attr user-class verbose))))

(defun concatenate-classifier (element dot-attr)
  "Given two keywords, return a string that describes an hm classifier."
  (concatenate 'string (string-downcase element) "-"
               (string-downcase dot-attr) "-by"))

(defun make-reachable-map (seq element dot-attr graph-type user-class
                           &optional (verbose t))
  "Return a closure for a reachability classification, USER-CLASS, for the
attribute, DOT-ATTR, of the graph element, ELEMENT. ELEMENT is one of `node',
`edge'."
  (let* ((map (make-classifier :reachable seq verbose))
         (cfg (archaeological-sequence-configuration seq))
         (cls (classification-to-keyword (concatenate-classifier element dot-attr)))
         (node-p (eq element :node))
         (edge-node (edge-classify-by seq))
         (origin (lookup-graphviz-option
                  cfg element :origin graph-type cls user-class))
         (reachable (lookup-graphviz-option
                     cfg element :reachable graph-type cls user-class))
         (not-reachable (lookup-graphviz-option
                         cfg element :not-reachable graph-type cls user-class)))
    (cond
      ((fset:contains? (color-attributes) dot-attr)
       (let ((scheme (lookup-graphviz-option
                      cfg element :colorscheme graph-type cls user-class)))
         #'(lambda (x)
             (let* ((color (if node-p (fset:@ map x) (choose-node x edge-node map))))
               (quotes-around
                (graphviz-color-string (case color
                                         (:origin origin)
                                         (:reachable reachable)
                                         (:not-reachable not-reachable)
                                         (otherwise
                                          (error "Error: Unable to make reachable map.")))
                                       scheme 3))))))
      ((fset:contains? (fset:union (category-attributes) (numeric-attributes)) dot-attr)
       #'(lambda (x)
           (let ((index (if node-p (fset:@ map x) (choose-node x edge-node map))))
             (quotes-around
              (case index
                (:origin origin)
                (:reachable reachable)
                (:not-reachable not-reachable)
                (otherwise (error "Error: Unable to make reachable map.")))))))
      (t (error "Error: Unable to make reachable map.")))))

(defun make-adjacent-map (seq element dot-attr graph-type user-class
                          &optional (verbose t))
  "Return a closure for an adjacency classification, USER-CLASS, for the
attribute, DOT-ATTR, of the graph ELEMENT. ELEMENT is one of `node', `edge'."
  (let* ((map (make-classifier :adjacent seq verbose))
         (cfg (archaeological-sequence-configuration seq))
         (cls (classification-to-keyword (concatenate-classifier element dot-attr)))
         (node-p (eq element :node))
         (edge-node (edge-classify-by seq))
         (origin (lookup-graphviz-option
                  cfg element :origin graph-type cls user-class))
         (adjacent (lookup-graphviz-option
                    cfg element :adjacent graph-type cls user-class))
         (not-adjacent (lookup-graphviz-option
                        cfg element :not-adjacent graph-type cls user-class)))
    (cond
      ((fset:contains? (color-attributes) dot-attr)
       (let ((scheme (lookup-graphviz-option
                      cfg element :colorscheme graph-type cls user-class)))
         #'(lambda (x)
             (let* ((color (if node-p (fset:@ map x) (choose-node x edge-node map))))
               (quotes-around
                (graphviz-color-string
                 (case color
                   (:origin origin)
                   (:adjacent adjacent)
                   (:not-adjacent not-adjacent)
                   (otherwise
                    (error "Error: Unable to make adjacency map.~&")))
                 scheme 3))))))
      ((fset:contains? (fset:union (category-attributes) (numeric-attributes)) dot-attr)
       #'(lambda (x)
           (let ((index (if node-p (fset:@ map x) (choose-node x edge-node map))))
             (quotes-around
              (case index
                (:origin origin)
                (:adjacent adjacent)
                (:not-adjacent not-adjacent)
                (otherwise (error "Error: Unable to make adjacency map.")))))))
      (t (error "Error: Unable to make adjacency map.")))))

(defun category-attribute-map (cfg element dot-attr)
  (cond
    ((and (eq element :node) (eq dot-attr :style))
     (graphviz-node-style-map))
    ((and (eq element :node) (eq dot-attr :shape))
     (graphviz-node-shape-map))
    ((and (eq element :edge) (eq dot-attr :style))
     (graphviz-edge-style-map))
    ((and (eq element :edge) (eq dot-attr :arrowhead))
     (graphviz-arrow-shape-map))
    ((and (eq element :node) (eq dot-attr :polygon-sides))
     (graphviz-polygon-sides-map cfg))))

(defun numeric-attribute-min (cfg element dot-attr)
  "Return the minimum value of DOT-ATTR for the ELEMENT from the user's
configuration, CFG."
  (case dot-attr
    (:penwidth (penwidth-min cfg element))
    (:fontsize (fontsize-min cfg element))
    (:polygon-skew (polygon-skew-min cfg))
    (:polygon-orientation (polygon-orientation-min cfg))
    (:polygon-distortion (polygon-distortion-min cfg))))

(defun numeric-attribute-max (cfg element dot-attr)
  "Return the minimum value of DOT-ATTR for the ELEMENT from the user's
configuration, CFG."
  (case dot-attr
    (:penwidth (penwidth-max cfg element))
    (:fontsize (fontsize-max cfg element))
    (:polygon-skew (polygon-skew-max cfg))
    (:polygon-orientation (polygon-orientation-max cfg))
    (:polygon-distortion (polygon-distortion-max cfg))))

(defun choose-node (edge node map)
  "Return a graph node given an EDGE, a keyword NODE, one of :to or :from, and an
fset MAP."
  (if (eq node :from)
      (fset:@ map (nth 0 edge))
      (fset:@ map (nth 1 edge))))

(defun make-distance-map (seq element dot-attr graph-type user-class &optional (verbose t))
  "Return a closure for a distance classification for the attribute, DOT-ATTR,
of the graph ELEMENT. ELEMENT is one of :node, :edge."
  (let* ((cfg (archaeological-sequence-configuration seq))
         (map (make-classifier :distance seq verbose))
         (node-p (eq element :node))
         (edge-node (edge-classify-by seq)))
    (cond
      ((fset:contains? (color-attributes) dot-attr)
       (let ((colors (1+ (fset:greatest (fset:range map))))
             (scheme (lookup-graphviz-option cfg element :colorscheme
                                             graph-type :none user-class)))
         #'(lambda (x)
             (let ((color (if node-p (fset:@ map x) (choose-node x edge-node map))))
               (quotes-around
                (graphviz-color-string color scheme colors))))))
      ((fset:contains? (category-attributes) dot-attr)
       (let ((categories (category-attribute-map cfg element dot-attr)))
         #'(lambda (x)
             (let ((index (if node-p (fset:@ map x) (choose-node x edge-node map))))
               (quotes-around
                (fset:@ categories (mod index (fset:size categories))))))))
      ((fset:contains? (numeric-attributes) dot-attr)
       (let ((min (numeric-attribute-min cfg element dot-attr))
             (max (numeric-attribute-max cfg element dot-attr))
             (interval (/ 1 (fset:greatest (fset:range map)))))
         #'(lambda (x)
             (let ((index (if node-p (fset:@ map x) (choose-node x edge-node map))))
               (quotes-around (write-to-string
                               (+ min (* (- max min) (* interval index)))))))))
      (t (error "Error: Unable to make distance map.")))))

(defun make-units-map (seq element dot-attr user-class &optional (verbose t))
  "Return an fset map for a unit classification, whose key is an integer and
value is a string appropriate for the attribute, DOT-ATTR, of the graph ELEMENT.
ELEMENT is one of :node, :edge."
  (let* ((map (make-classifier :units seq verbose))
         (cfg (archaeological-sequence-configuration seq))
         (cls (classification-to-keyword (concatenate-classifier element dot-attr)))
         (node-p (eq element :node))
         (edge-node (edge-classify-by seq))
         (deposit (lookup-graphviz-option
                   cfg element :deposit :sequence cls user-class))
         (interface (lookup-graphviz-option
                     cfg element :interface :sequence cls user-class)))
    (cond
      ((fset:contains? (color-attributes) dot-attr)
       (let ((scheme (lookup-graphviz-option cfg
                      element :colorscheme :sequence cls user-class)))
         #'(lambda (x)
             (let ((index (if node-p (fset:@ map x) (choose-node x edge-node map))))
               (quotes-around (graphviz-color-string
                               (if (eq index :deposit) deposit interface) scheme 2))))))
      ((fset:contains? (fset:union (category-attributes) (numeric-attributes)) dot-attr)
       #'(lambda (x)
           (let ((index (if node-p (fset:@ map x) (choose-node x edge-node map))))
             (if (eq index :deposit) (quotes-around deposit) (quotes-around interface)))))
      (t (error "Error: Unable to make units map.")))))

(defun make-levels-map (seq element dot-attr graph-type user-class &optional (verbose t))
  "Return an fset map for a levels classification, whose key is an integer and
value is a string appropriate for the attribute, DOT-ATTR, of the graph ELEMENT.
ELEMENT is one of :node, :edge."
  (let* ((cfg (archaeological-sequence-configuration seq))
         (map (make-classifier :levels seq verbose))
         (edge-node (edge-classify-by seq))
         (node-p (eq element :node)))
    (cond
      ((fset:contains? (color-attributes) dot-attr)
       (let ((colors (1+ (fset:greatest (fset:range map))))
             (scheme (lookup-graphviz-option cfg element :colorscheme
                                             graph-type :none user-class)))
         #'(lambda (x)
             (let ((color (if node-p (fset:@ map x) (choose-node x edge-node map))))
               (quotes-around (graphviz-color-string color scheme colors))))))
      ((fset:contains? (category-attributes) dot-attr)
       (let ((categories (category-attribute-map cfg element dot-attr)))
         #'(lambda (x)
             (let ((index (if node-p (fset:@ map x) (choose-node x edge-node map))))
               (quotes-around (fset:@ categories (mod index (fset:size categories))))))))
      ((fset:contains? (numeric-attributes) dot-attr)
       (let ((min (numeric-attribute-min cfg element dot-attr))
             (max (numeric-attribute-max cfg element dot-attr))
             (interval (/ 1 (fset:greatest (fset:range map)))))
         #'(lambda (x)
             (let ((index (if node-p (fset:@ map x) (choose-node x edge-node map))))
               (quotes-around (write-to-string
                               (+ min (* (- max min) (* interval index)))))))))
      (t (error "Error: Unable to make levels map.")))))

(defun make-phases-map (seq element dot-attr user-class &optional (verbose t))
  "Return an fset map for a phases classification, whose key is an integer and
value is a string appropriate for the attribute, DOT-ATTR, of the graph ELEMENT.
ELEMENT is one of `node', `edge'."
  (let ((map (make-classifier :phases seq verbose))
        (cls (classification-to-keyword (concatenate-classifier element dot-attr)))
        (node-p (equal element :node))
        (edge-node (edge-classify-by seq)))
    (cond
      ((fset:contains? (color-attributes) dot-attr)
       (let ((scheme (lookup-graphviz-option
                      (archaeological-sequence-configuration seq)
                      element :colorscheme :sequence cls user-class)))
         #'(lambda (x)
             (let ((index (if node-p (fset:@ map x)
                              (if (equal edge-node "from")
                                  (fset:@ map (nth 0 x))
                                  (fset:@ map (nth 1 x))))))
               (graphviz-color-string index scheme)))))
      ((fset:contains? (fset:union (category-attributes) (numeric-attributes)) dot-attr)
       #'(lambda (x)
            (if node-p (fset:@ map x)
                (if (equal edge-node "from")
                    (fset:@ map (nth 0 x))
                    (fset:@ map (nth 1 x))))))
      (t (error "Error: Unable to make units map.")))))

(defun make-periods-map (seq element dot-attr user-class &optional (verbose t))
  "Return an fset map for a periods classification, whose key is an integer and
value is a string appropriate for the attribute, DOT-ATTR, of the graph ELEMENT.
ELEMENT is one of `node', `edge'."
  (let* ((map (make-classifier "periods" seq verbose))
         (cls (classifier-to-keyword (concatenate-classifier element dot-attr)))
         (node-p (equal element "node"))
         (edge-node (edge-classify-by seq)))
    (cond
      ((fset:contains? (color-attributes) dot-attr)
       (let ((scheme (lookup-graphviz-option
                      (archaeological-sequence-configuration seq)
                      element "colorscheme" "sequence" cls user-class)))
         #'(lambda (x)
             (let ((index (if node-p (fset:@ map x)
                              (if (equal edge-node "from")
                                  (fset:@ map (nth 0 x))
                                  (fset:@ map (nth 1 x))))))
               (graphviz-color-string index scheme)))))
      ((fset:contains? (fset:union (category-attributes) (numeric-attributes)) dot-attr)
       #'(lambda (x)
           (if node-p (fset:@ map x)
               (if (equal edge-node "from")
                   (fset:@ map (nth 0 x))
                   (fset:@ map (nth 1 x))))))
      (t (error "Error: Unable to make units map.")))))


(defun make-matrix (seq user-class)
  "Return a graph-matrix matrix appropriate for USER-CLASS, given the
  information on the user's configuration and a directed graph from SEQ."
  (let ((cfg (archaeological-sequence-configuration seq))
        (graph (archaeological-sequence-graph seq))))
  (cond
    ((eq :distance user-class) (create-distance-matrix cfg graph))
    ((eq :reachable user-class) (create-reachability-matrix cfg graph))
    ((eq :adjacent user-class) (create-adjacency-matrix cfg graph))))


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
                (setf ht (fset:with ht (nth 0 x) (nth 2 x))))
            other-table)
    (mapcar #'(lambda (x)
                (setf ret
                      (fset:with ret (symbolicate (nth 0 x))
                                 (read-from-string (fset:@ ht (nth n x))))))
            contexts)
    ret))

(defun alist-to-map (alist)
  "Given an assoc-list, ALIST, return the corresponding fset map."
  (let ((ret (fset:empty-map)))
    (mapc #'(lambda (pair)
              (setq ret (fset:with ret (car pair) (cdr pair))))
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
                                   ((string= (nth 1 x) "deposit") :deposit)
                                   ((string= (nth 1 x) "interface") :interface)))))
            contexts)
    ret))
