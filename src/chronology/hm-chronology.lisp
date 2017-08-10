;;; hm-chronology.lisp

;; Copyright (C) Thomas Dye 2016

;; Licensed under the Gnu Public License Version 3 or later

(in-package #:hm)

(defun chronology-graph-html-label (id size)
  "Convert the symbol ID into an html label for Graphviz dot.  This
function is designed specifically for use with chronology graphs."
  (let* ((s (string-downcase (string id))))
    (format nil "<&~a;<FONT POINT-SIZE=\"~a\"><SUB>~a</SUB></FONT>>"
            (subseq s 0 (position #\- s :test #'equal))
            size
            (subseq s (+ 1 (position #\- s :test #'equal))))))

;; API
(defun create-chronology-graph (distance-matrix cfg &optional (verbose t))
  "Create and return a chronology graph with the distance matrix
DISTANCE-MATRIX and the configuration CFG.  If VERBOSE, then advertise
progress."
  (unless (typep cfg 'config) (error "Error: No configuration found."))
  (if (get-option cfg "Chronology graph" "draw" :type :boolean)
      (when verbose (format t "Creating chronology graph.~&"))
      (return-from create-chronology-graph))
  (unless distance-matrix
    (error "Error: Distance matrix is absent."))
  (let ((ret (make-instance 'graph:digraph))
        (node-index (fset:empty-map))
        (event-table (read-table (get-option cfg "Input files" "events")
                                 (get-option cfg "Input file headers" "events")
                                 verbose))
        (event-order-table (read-table
                            (get-option cfg "Input files" "event-order")
                            (get-option cfg "Input file headers" "event-order")
                            verbose)))
    ;; If assume-correlations then adjust the event-table and
    ;; event-order table accordingly
    (when (get-option cfg "General configuration"
                      "assume-correlations" :type :boolean)
      (let ((inference-table
             (read-table (get-option cfg "Input files" "inferences")
                         (get-option cfg "Input file headers" "inferences")
                         verbose))
            (inference-map (fset:empty-map)))
        (dolist (row inference-table)
          (setq inference-map
                (fset:with inference-map (nth 0 row)
                           (correlated-node (nth 0 row) (nth 1 row) t)))
          (setq inference-map
                (fset:with inference-map (nth 1 row)
                           (correlated-node (nth 0 row) (nth 1 row) t))))
        (setf event-table
              (mapcar #'(lambda (row)
                          (let ((node-1 (fset:lookup inference-map (nth 0 row)))
                                (node-2 (fset:lookup inference-map (nth 1 row))))
                            (when node-1 (setf (nth 0 row) node-1))
                            (when node-2 (setf (nth 1 row) node-2))))
                      event-table))
        (setf event-order-table
              (mapcar #'(lambda (row)
                          (let ((node-1 (fset:lookup inference-map (nth 0 row)))
                                (node-2 (fset:lookup inference-map (nth 1 row))))
                            (when node-1 (setf (nth 0 row) node-1))
                            (when node-2 (setf (nth 1 row) node-2))))
                      event-order-table))))
    ;; Steps 1 and 2 of the algorithm
    (dolist (col event-table)
      (graph:add-node ret (symbolicate "alpha-" (nth 1 col)))
      (graph:add-node ret (symbolicate "beta-" (nth 1 col)))
      (graph:add-node ret (symbolicate "theta-" (nth 0 col))))
    ;; Step 3 of the algorithm
    (when event-order-table
      (dolist (pair event-order-table)
        (graph:add-edge ret
                        (list (symbolicate "theta-" (nth 1 pair))
                              (symbolicate "theta-" (nth 0 pair)))
                        0)))

    (when verbose (format t "Modeling radiocarbon dates.~&"))
    ;; Step 4 of the algorithm
    (dolist (node event-table)
      (and (eq 0 (graph:indegree
                  ret
                  (symbolicate "theta-" (nth 0 node))))
           (not (eq (new-symbol (nth 3 node)) (new-symbol "disparate")))
           (graph:add-edge ret
                           (list (symbolicate "beta-" (nth 1 node))
                                 (symbolicate "theta-" (nth 0 node)))
                           0))
      (and (eq 0 (graph:outdegree
                  ret
                  (symbolicate "theta-" (nth 0 node))))
           (not (eq (new-symbol (nth 3 node)) (new-symbol "disjunct")))
           (graph:add-edge ret
                           (list
                            (symbolicate "theta-" (nth 0 node))
                            (symbolicate  "alpha-" (nth 1 node)))
                           0)))
    ;; Step 5 of the algorithm
    (let ((counter -1))
      (mapc (lambda (node)
              (setf node-index (fset:with node-index node (incf counter))))
            (graph:nodes ret)))
    (let ((events))
      (when verbose (format t "Adding edge values to the chronology graph.~&"))
      (dolist (row event-table)
        (push (new-symbol (nth 1 row)) events)
        (graph:add-edge ret
                        (list (symbolicate "beta-" (nth 1 row))
                              (symbolicate "alpha-" (nth 1 row))) 2))
      (dolist (pair (append (unique-pairs events)
                            (unique-pairs (reverse events))))
        (let ((distance (graph-matrix::matrix-ref
                         distance-matrix
                         (fset:@ node-index (nth 0 pair))
                         (fset:@ node-index (nth 1 pair)))))
          (unless (graph-matrix:infinitep distance distance-matrix)
            (graph:add-edge ret
                            (list (symbolicate "alpha-" (nth 0 pair))
                                  (symbolicate "beta-" (nth 1 pair)))
                            (if (eql 1 (round distance)) 1 2))))))
    ;; Step 6 of the algorithm
    (when (graph:cycles ret) (error "Error: chronology graph has a cycle."))
    (transitive-reduction ret cfg)))

(defun transitive-reduction (graph cfg)
  "Perform transitive reduction on the directed acyclic graph GRAPH,
according to information in the configuration CFG.  Returns the
possibly modified directed acyclic graph."
  (let ((ret (graph:copy graph))
        (reach (graph-matrix:to-reachability-matrix
                graph (new-matrix
                       (get-option cfg "General configuration" "fast-matrix"
                                   :type :boolean))))
        (node-index (fset:empty-map)))
    (let ((counter -1))
      (mapc (lambda (node)
              (setf node-index (fset:with node-index (incf counter) node)))
            (graph:nodes ret)))
    (let ((n (graph-matrix:matrix-n-cols reach)))
      (loop for j below n do
           (loop for i below n do
                (when (graph-matrix:reachablep ret reach i j)
                  (loop for k below n do
                       (when (graph-matrix:reachablep ret reach j k)
                         (setf (graph-matrix:matrix-ref reach i k) 0)
                         (when (graph:has-edge-p ret
                                                 (list (fset:@ node-index j)
                                                       (fset:@ node-index k)))
                           (graph:delete-edge ret
                                              (list (fset:@ node-index i)
                                                    (fset:@ node-index k))))))))))
    ret))
