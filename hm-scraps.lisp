;; Classifiers
;; I think this can be implemented with closures
;; The idea is to map nodes to a classification in an fset map
;; then pass it to a little function that returns a closure
;; The closure can be stored with the archaeological sequence and
;; passed to the drawing routine using the #' syntax.
;; (defun classifier-class (map)
;;    (let ((classification map))
;;        (lambda (node)
;;           (fset:@ map node))))

;; (defclass classifier ()
;;   ((map :accessor classification
;;         :initform (fset:empty-map)
;;         :documentation "An fset map to hold the classification for the
;;         nodes.  The key is the node label and the value is set
;;         according to the CLASSIFY method.")
;;    (table :accessor table
;;           :initform nil
;;           :initarg :table
;;           :documentation "A table that holds user-specified
;;           classification information, either for periods or for
;;           phases.")
;;    (contexts :accessor contexts
;;              :initform nil
;;              :initarg :contexts
;;              :allocation :class
;;              :documentation "Contextual information for each of the
;;              nodes, supplied by the user.")
;;    (matrix :accessor matrix
;;            :initform nil
;;            :initarg :matrix
;;            :documentation "A matrix representation of the
;;            stratigraphic graph, typically the distance matrix.")
;;    (legend-node-shape :accessor legend-node-shape
;;                       :initform 'box
;;                       :initarg :legend-node-shape
;;                       :documentation "A default shape for nodes in the
;;                       legend.  Typically supplied by the configuration.")
;;    (legend-node-fill :accessor legend-node-fill
;;                      :initform 'white
;;                      :initarg :legend-node-fill
;;                      :documentation "A default fill color for nodes in
;;                      the legend.  Typically supplied by the configuration.")
;;    (legend-node-color :accessor legend-node-color
;;                       :initform 'black
;;                       :initarg :legend-node-color
;;                       :documentation "A default outline color for
;;                       nodes in the legend.  Typically supplied by the
;;                       configuration.")
;;    (origin-node :accessor origin-node
;;                 :initform nil
;;                 :initarg :origin-node
;;                 :documentation "The label of the node used as the
;;                 origin in computations of adjacency, reachability, and
;;                 distance.")
;;    (origin-attribute :accessor origin
;;                      :initform nil
;;                      :initarg :origin
;;                      :documentation "An attribute of the origin node.
;;                      Currently, this might be a shape, a fill color,
;;                      or a node color.")
;;    (adjacent-attribute :accessor adjacent
;;                        :initform nil
;;                        :initarg :adjacent
;;                        :documentation "An attribute of nodes adjacent
;;                        to the origin node.  Currently, this might be a
;;                        shape, a fill color, or a node color.")
;;    (reachable-attribute :accessor reachable
;;                         :initform nil
;;                         :initarg :reachable
;;                         :documentation "An attribute of nodes
;;                         reachable from the origin node.  Currently,
;;                         this might be a shape, a fill color, or a node
;;                         color.")
;;    (unreachable-attribute :accessor unreachable
;;                           :initform nil
;;                           :initarg :unreachable
;;                           :documentation "An attribute of nodes
;;                           unreachable from the origin node.
;;                           Currently, this might be a shape, a fill
;;                           color, or a node color.")
;;    (deposit-attribute :accessor deposit
;;                       :initform nil
;;                       :initarg :deposit
;;                       :documentation "An attribute of nodes that
;;                       represent deposits.  Currently, this might be a
;;                       shape, a fill color, or a node color.")
;;    (interface-attribute :accessor interface
;;                         :initform nil
;;                         :initarg :interface
;;                         :documentation "An attribute of nodes that
;;                         represent interfaces.  Currently, this might
;;                         be a shape, a fill color, or a node color.")))

;; (defgeneric classify (obj graph cfg)
;;   (:documentation "Make a classification and store it in the MAP slot."))

;; (defgeneric classify-by (obj graph)
;;   (:documentation "Classify objects according to the classification in
;;   OBJ.  Returns an fset map."))

;; (defgeneric legend-shape (obj cfg &key default)
;;   (:documentation "A shape for the legend node.  If DEFAULT, use a
;;   value supplied by the user."))

;; (defgeneric legend-fill (obj cfg &key default)
;;   (:documentation "A fill color for the legend node.  If DEFAULT, use
;;   a value supplied by the user."))

;; (defgeneric legend-color (obj cfg &key default)
;;   (:documentation "An outline color for the legend node.  If DEFAULT,
;;   use a value supplied by the user."))

;; (defgeneric add-legend-nodes (obj graph)
;;   (:documentation "Adds nodes for the legend to GRAPH.  Note that this
;;   method should be called just before drawing the graph and after
;;   all the classifications have been completed."))

;; (defgeneric add-legend-edges (obj graph)
;;   (:documentation "Adds edges for the legend to GRAPH. Note that this
;;   method should be called just before drawing the graph and after all
;;   the classifications have been completed."))

;; (defmethod classify-by ((obj classifier) graph)
;;   (when (fset:empty? (classification obj)) (classify obj graph))
;;   (classification obj))

;; (defclass levels (classifier)
;;   () (:documentation "A simple classifier that assigns a positive
;; integer to each node in GRAPH, called its level, where, for each
;; directed edge (a b) the corresponding integers satisfy a < b."))

;; (defmethod classify ((obj levels) graph cfg)
;;   "Set map according to the levels of GRAPH."
;;   (let ((lev (graph:levels graph)))
;;     (maphash-keys
;;      (lambda (key)
;;        (setf (classification obj)
;;              (fset:with (classification obj) key (gethash key lev))))
;;      lev)))

;; ;; Tabular class and methods for periods and phases

;; (defclass tabular (classifier)
;;   ()
;;   (:documentation "A classifier based on information contained in a
;;   user-specified table, which might hold information on periods or
;;   phases."))

;; (defmethod legend-shape ((obj tabular) cfg &key default)
;;   (let ((ret (fset:empty-map)))
;;     (mapcar #'(lambda (x)
;;                 (setf ret
;;                       (fset:with ret (new-symbol (second x))
;;                                  (if default (legend-node-shape obj)
;;                                      (new-symbol (third x))))))
;;             (table obj))
;;     ret))

;; (defmethod legend-fill ((obj tabular) cfg &key default)
;;   (let ((ret (fset:empty-map)))
;;     (mapcar #'(lambda (x)
;;                 (setf ret
;;                       (fset:with ret (new-symbol (second x))
;;                                  (if default (legend-node-fill obj)
;;                                      (new-symbol (third x) nil)))))
;;             (table obj))
;;     ret))

;; (defmethod legend-color ((obj tabular) cfg &key default)
;;   (let ((ret (fset:empty-map)))
;;     (mapcar #'(lambda (x)
;;                 (setf ret
;;                       (fset:with ret (new-symbol (second x))
;;                                  (if default (legend-node-color obj)
;;                                      (new-symbol (third x) nil)))))
;;             (table obj))
;;     ret))

;; (defmethod legend-urls ((obj tabular) cfg)
;;   (let ((ret (fset:empty-map))
;;         (default (get-option cfg "General configuration" "url-default")))
;;     (mapcar #'(lambda (x)
;;                 (setf ret
;;                       (fset:with ret (new-symbol (second x)) default)))
;;             (table obj))
;;     ret))

;; (defmethod add-legend-nodes ((obj tabular) graph)
;;   (mapcar #'(lambda (x)
;;               (graph:add-node graph (new-symbol (second x))))
;;           (table obj)))

;; (defmethod add-legend-edges ((obj tabular) graph)
;;   (loop for (x y) on (table obj) while y
;;      do (graph:add-edge graph (list (new-symbol (second x))
;;                                     (new-symbol (second y))))))

;; (defclass periods (tabular) ())

;; (defmethod classify ((obj periods) graph cfg)
;;   (let ((ht (fset:empty-map)))
;;     (mapcar
;;      #'(lambda (x)
;;          (setf ht (fset:with ht
;;                              (new-symbol (first x))
;;                              (new-symbol (third x) nil))))
;;      (table obj))
;;     (mapcar
;;      #'(lambda (x)
;;          (setf (classification obj)
;;                (fset:with (classification obj)
;;                           (new-symbol (first x))
;;                           (fset:@ ht (new-symbol (fourth x))))))
;;      (contexts obj))))

;; (defclass phases (tabular) ())

;; (defmethod classify ((obj phases) graph cfg)
;;   (let ((ht (fset:empty-map)))
;;     (mapcar
;;      #'(lambda (x)
;;          (setf ht (fset:with ht
;;                              (new-symbol (first x))
;;                              (new-symbol (third x) nil))))
;;      (table obj))
;;     (mapcar
;;      #'(lambda (x)
;;          (setf (classification obj)
;;                (fset:with (classification obj)
;;                           (new-symbol (first x))
;;                           (fset:@ ht (new-symbol (fifth x))))))
;;      (contexts obj))))

;; (defclass reachable (classifier)
;;   () (:documentation "A classification based on the reachability
;;   matrix of GRAPH that distinguishes the origin node, nodes reachable
;;   from the origin, and nodes not reachable from the origin."))

;; (defmethod classify ((obj reachable) graph cfg)
;;   "Sets unreachable nodes to 'not-reachable, the origin node to
;; 'origin, and reachable nodes to 'reachable.  The origin node is
;; typically set by the special variable *reachable-from*."
;;   (let ((reachable-list)
;;         (limit(get-option cfg "General configuration" "reachable-limit"
;;                           :type :number)))
;;     (cond
;;       ((< limit 0)
;;        (setf reachable-list
;;              (graph-matrix:reachable-from graph (matrix obj)
;;                                           (origin-node obj))))
;;       ((eq limit 0) (setf reachable-list ()))
;;       ((eq limit 1)
;;        (mapc (lambda (x)
;;                (when (eql (first x) (origin-node obj))
;;                  (push (second x) reachable-list)))
;;              (graph:edges graph)))
;;       (t (setf reachable-list (graph-matrix:reachable-from
;;                                graph (matrix obj) (origin-node obj)))))
;;     (dolist (node reachable-list)
;;       (setf (classification obj) (fset:with (classification obj) node 'reachable)))
;;     (dolist (node (set-difference (graph:nodes graph) reachable-list))
;;       (setf (classification obj)
;;             (fset:with (classification obj) node 'not-reachable)))
;;     (setf (classification obj)
;;           (fset:with (classification obj) (origin-node obj) 'origin))))

;; (defmethod classify-by ((obj reachable) graph)
;;   (when (fset:empty? (classification obj)) (classify obj graph))
;;   (let ((ret (fset:empty-map)))
;;     (fset:do-map (key value (classification obj))
;;       (setf ret
;;             (fset:with ret key (case value
;;                                  (not-reachable (unreachable obj))
;;                                  (origin (origin obj))
;;                                  (reachable (reachable obj))))))
;;     ret))

;; (defmethod legend-shape ((obj reachable) cfg &key default)
;;   (let ((default-shape (get-option cfg "Graphviz legend node attributes"
;;                                    "shape")))
;;     (if default
;;         (-> (fset:empty-map)
;;             (fset:with (new-symbol "Reachable") default-shape)
;;             (fset:with (new-symbol "Not reachable") default-shape)
;;             (fset:with (new-symbol "Origin") default-shape))
;;         (-> (fset:empty-map)
;;             (fset:with (new-symbol "Reachable")
;;                        (get-option cfg "Graphviz sequence reachability shapes"
;;                                    "reachable"))
;;             (fset:with (new-symbol "Not reachable")
;;                        (get-option cfg "Graphviz sequence reachability shapes"
;;                                    "not reachable"))
;;             (fset:with (new-symbol "Origin")
;;                        (get-option cfg "Graphviz sequence reachability shapes"
;;                                    "origin"))))))

;; (defmethod legend-fill ((obj reachable) cfg &key default)
;;   (let ((default-fillcolor (get-option cfg "Graphviz legend node attributes"
;;                                        "fillcolor")))
;;     (if default
;;         (-> (fset:empty-map)
;;             (fset:with (new-symbol "Reachable") default-fillcolor)
;;             (fset:with (new-symbol "Not reachable") default-fillcolor)
;;             (fset:with (new-symbol "Origin") default-fillcolor))
;;         (-> (fset:empty-map)
;;             (fset:with (new-symbol "Reachable")
;;                        (get-option cfg "Graphviz sequence reachability fills"
;;                                    "reachable"))
;;             (fset:with (new-symbol "Not reachable")
;;                        (get-option cfg "Graphviz sequence reachability fills"
;;                                    "not-reachable"))
;;             (fset:with (new-symbol "Origin")
;;                        (get-option cfg "Graphviz sequence reachability fills"
;;                                    "origin"))))))

;; (defmethod legend-color ((obj reachable) cfg &key default)
;;   (let ((default-color (get-option cfg "Graphviz legend node attributes"
;;                                    "color")))
;;     (if default
;;         (-> (fset:empty-map)
;;             (fset:with (new-symbol "Reachable") default-color)
;;             (fset:with (new-symbol "Not reachable") default-color)
;;             (fset:with (new-symbol "Origin") default-color))
;;         (-> (fset:empty-map)
;;             (fset:with (new-symbol "Reachable")
;;                        (get-option cfg "Graphviz sequence reachability colors"
;;                                    "reachable"))
;;             (fset:with (new-symbol "Not reachable")
;;                        (get-option cfg "Graphviz sequence reachability colors"
;;                                    "not-reachable"))
;;             (fset:with (new-symbol "Origin")
;;                        (get-optioin cfg "Graphviz sequence reachability colors"
;;                                     "origin"))))))

;; (defmethod legend-urls ((obj reachable) cfg)
;;   (let ((default-url (get-option cfg "General configuration" "url-default")))
;;     (-> (fset:empty-map)
;;         (fset:with (new-symbol "Reachable") default-url)
;;         (fset:with (new-symbol "Not reachable") default-url)
;;         (fset:with (new-symbol "Origin") default-url))))

;; (defmethod add-legend-nodes ((obj reachable) graph)
;;   (graph:add-node graph (new-symbol "Reachable"))
;;   (graph:add-node graph (new-symbol "Not reachable"))
;;   (graph:add-node graph (new-symbol "Origin")))

;; (defmethod add-legend-edges ((obj reachable) graph)
;;   (graph:add-edge graph (list
;;                          (new-symbol "Reachable") (new-symbol "Not reachable")))
;;   (graph:add-edge graph (list (new-symbol "Origin") (new-symbol "Reachable"))))

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


;; (defclass distance (classifier)
;;   () (:documentation "A classification based on the distance matrix of GRAPH."))

;; (defmethod classify ((obj distance) graph cfg)
;;   "Sets unreachable nodes to 'not-reachable, the origin node to 'origin,
;; adjacent nodes to 'adjacent, and reachable nodes to 'reachable"
;;   (let (val)
;;     (mapc #'(lambda (node)
;;               (setf val (min (graph-matrix:distance-from-to
;;                               graph (matrix obj)
;;                               (origin-node obj) node)
;;                              (graph-matrix:distance-from-to
;;                               graph (matrix obj) node
;;                               (origin-node obj))))
;;               (setf (classification obj)
;;                     (fset:with (classification obj) node
;;                                (cond
;;                                  ((graph-matrix:infinitep val (matrix obj))
;;                                   'unreachable)
;;                                  ((eql (round val) 0) 'origin)
;;                                  ((eql (round val) 1) 'abutting)
;;                                  (t 'separated)))))
;;           (graph:nodes graph))))

;; (defmethod classify-by ((obj distance) graph)
;;   (when (fset:empty? (classification obj)) (classify obj graph))
;;   (let ((ret (fset:empty-map)))
;;     (fset:do-map (key value (classification obj))
;;       (setf ret
;;             (fset:with ret key
;;                        (case value
;;                          (unreachable (unreachable obj))
;;                          (origin (origin obj))
;;                          (abutting (adjacent obj))
;;                          (separated (reachable obj))))))
;;     ret))

;; (defmethod legend-shape ((obj distance) cfg &key default)
;;   (let ((default-shape (get-option cfg "Graphviz legend node attributes" "shape")))
;;     (if default
;;         (-> (fset:empty-map)
;;             (fset:with (new-symbol "Separated") default-shape)
;;             (fset:with (new-symbol "Not reachable") default-shape)
;;             (fset:with (new-symbol "Origin") default-shape)
;;             (fset:with (new-symbol "Abutting") default-shape))
;;         (-> (fset:empty-map)
;;             (fset:with (new-symbol "Separated")
;;                        (get-option cfg "Graphviz sequence reachability shapes"
;;                                    "reachable"))
;;             (fset:with (new-symbol "Not reachable")
;;                        (get-option cfg "Graphviz sequence reachability shapes"
;;                                    "not-reachable"))
;;             (fset:with (new-symbol "Origin")
;;                        (get-option cfg "Graphviz sequence reachability shapes"
;;                                    "origin"))
;;             (fset:with (new-symbol "Abutting")
;;                        (get-option cfg "Graphviz sequence reachability shapes"
;;                                    "adjacent"))))))

;; (defmethod legend-fill ((obj distance) cfg &key default)
;;   (let ((default-fill (get-option cfg "Graphviz legend node attributes"
;;                                   "fillcolor")))
;;     (if default
;;         (-> (fset:empty-map)
;;             (fset:with (new-symbol "Separated") default-fill)
;;             (fset:with (new-symbol "Not reachable") default-fill)
;;             (fset:with (new-symbol "Origin") default-fill)
;;             (fset:with (new-symbol "Abutting") default-fill))
;;         (-> (fset:empty-map)
;;             (fset:with (new-symbol "Separated")
;;                        (get-option cfg "Graphviz sequence reachability fills"
;;                                    "reachable"))
;;             (fset:with (new-symbol "Not reachable")
;;                        (get-option cfg "Graphviz sequence reachability fills"
;;                                    "not-reachable"))
;;             (fset:with (new-symbol "Origin")
;;                        (get-option cfg "Graphviz sequence reachability fills"
;;                                    "origin"))
;;             (fset:with (new-symbol "Abutting")
;;                        (get-option cfg "Graphviz sequence reachability fills"
;;                                    "adjacent"))))))

;; (defmethod legend-color ((obj distance) cfg &key default)
;;   (let ((default-color (get-option cfg "Graphviz legend node attributes"
;;                                    "color")))
;;     (if default
;;         (-> (fset:empty-map)
;;             (fset:with (new-symbol "Separated") default-color)
;;             (fset:with (new-symbol "Not reachable") default-color)
;;             (fset:with (new-symbol "Origin") default-color)
;;             (fset:with (new-symbol "Abutting") default-color))
;;         (-> (fset:empty-map)
;;             (fset:with (new-symbol "Separated")
;;                        (get-option cfg "Graphviz sequence reachability colors"
;;                                    "reachable"))
;;             (fset:with (new-symbol "Not reachable")
;;                        (get-option cfg "Graphviz sequence reachability colors"
;;                                    "not-reachable"))
;;             (fset:with (new-symbol "Origin")
;;                        (get-option cfg "Graphviz sequence reachability colors"
;;                                    "origin"))
;;             (fset:with (new-symbol "Abutting")
;;                        (get-option cfg "Graphviz sequence reachability colors"
;;                                    "adjacent"))))))

;; (defmethod legend-urls ((obj distance) cfg)
;;   (let ((default-url (get-option cfg "General configuration" "url-default")))
;;     (-> (fset:empty-map)
;;         (fset:with (new-symbol "Separated") default-url)
;;         (fset:with (new-symbol "Not reachable") default-url)
;;         (fset:with (new-symbol "Origin") default-url)
;;         (fset:with (new-symbol "Abutting") default-url))))

;; (defmethod add-legend-nodes ((obj distance) graph)
;;   (graph:add-node graph (new-symbol "Separated"))
;;   (graph:add-node graph (new-symbol "Not reachable"))
;;   (graph:add-node graph (new-symbol "Origin"))
;;   (graph:add-node graph (new-symbol "Abutting")))

;; (defmethod add-legend-edges ((obj distance) graph)
;;   (graph:add-edge graph
;;                   (list (new-symbol "Separated") (new-symbol "Not reachable")))
;;   (graph:add-edge graph (list (new-symbol "Abutting") (new-symbol "Separated")))
;;   (graph:add-edge graph (list (new-symbol "Origin") (new-symbol "Abutting"))))

;; ;; pass context-table and inference-table
;; (defclass units (tabular)
;;   ())

;; (defmethod classify ((obj units) graph cfg)
;;   (mapcar #'(lambda (x)
;;               (setf (classification obj)
;;                     (fset:with (classification obj) (new-symbol (first x))
;;                                (new-symbol (second x) nil))))
;;           (contexts obj))
;;   (when
;;       (get-option cfg "General configuration" "assume-correlations" :type :boolean)
;;     (dolist (part (table obj))
;;       (setf (classification obj)
;;             (fset:with (classification obj)
;;                        (read-from-string
;;                         (format nil "~a=~a" (first part) (second part)))
;;                        (fset:@ (classification obj) (new-symbol (first part))))))))

;; (defmethod classify-by ((obj units) graph)
;;   (when (fset:empty? (classification obj)) (classify obj graph))
;;   (let ((ret (fset:empty-map)))
;;     (fset:do-map (key value (classification obj))
;;       (setf ret
;;             (fset:with ret key
;;                        (case value
;;                          (deposit (deposit obj))
;;                          (interface (interface obj))))))
;;     ret))

;; (defmethod legend-shape ((obj units) cfg &key default)
;;   (let ((default-shape (get-option cfg "Graphviz legend node attributes"
;;                                    "shape")))
;;     (if default
;;         (-> (fset:empty-map)
;;             (fset:with (new-symbol "Deposit") default-shape)
;;             (fset:with (new-symbol "Interface") default-shape))
;;         (-> (fset:empty-map)
;;             (fset:with (new-symbol "Deposit")
;;                        (get-option cfg "Graphviz sequence deposit attributes"
;;                                    "shape"))
;;             (fset:with (new-symbol "Interface")
;;                        (get-option cfg "Graphviz sequence interface attributes"
;;                                    "shape"))))))

;; (defmethod legend-fill ((obj units) cfg &key default)
;;   (let ((default-fill (get-option cfg "Graphviz legend node attributes"
;;                                   "fillcolor"))))
;;   (if default
;;       (-> (fset:empty-map)
;;           (fset:with (new-symbol "Deposit") default-fill)
;;           (fset:with (new-symbol "Interface") default-fill))
;;       (-> (fset:empty-map)
;;           (fset:with (new-symbol "Deposit")
;;                      (get-option cfg "Graphviz sequence deposit attributes"
;;                                  "fillcolor"))
;;           (fset:with (new-symbol "Interface")
;;                      (get-option cfg "Graphviz sequence interface attributes"
;;                                  "fillcolor")))))

;; (defmethod legend-color ((obj units) cfg &key default)
;;   (let ((default-color (get-option cfg "Graphviz legend node attributes"
;;                                    "color"))))
;;   (if default
;;       (-> (fset:empty-map)
;;           (fset:with (new-symbol "Deposit") default-color)
;;           (fset:with (new-symbol "Interface") default-color))
;;       (-> (fset:empty-map)
;;           (fset:with (new-symbol "Deposit")
;;                      (get-option cfg "Graphviz sequence deposit attributes"
;;                                  "color"))
;;           (fset:with (new-symbol "Interface")
;;                      (get-option cfg "Graphviz sequence interface attributes"
;;                                  "color")))))

;; (defmethod legend-urls ((obj units) cfg)
;;   (let ((default-url (get-option cfg "General configuration" "url-default")))
;;     (-> (fset:empty-map)
;;         (fset:with (new-symbol "Deposit") default-url)
;;         (fset:with (new-symbol "Interface") default-url))))

;; (defmethod add-legend-nodes ((obj units) graph)
;;   (graph:add-node graph (new-symbol "Deposit"))
;;   (graph:add-node graph (new-symbol "Interface")))

;; (defmethod add-legend-edges ((obj units) graph)
;;   (graph:add-edge graph (list (new-symbol "Deposit") (new-symbol "Interface"))))

