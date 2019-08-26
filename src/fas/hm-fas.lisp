;;; hm-fas.lisp

;; Eades et al. (1993) heuristic for a feedback arc set

;; Copyright (C) Thomas Dye 2019

;; Licensed under the Gnu Public License Version 3 or later

(in-package  #:hm)

(defun feedback-arc-set (dag)
  "Given a DAG with cycles, returns a list of edges possibly elements of a feedback arc set as determined by Eades' heuristic."
  (let ((good-vertex-sequence (gr dag)))
    (eades-fas good-vertex-sequence dag)))

(defun gr (cyclic)
  "Eades' et al. (1993) greedy algorithm for a 'good' vertex sequence. Given a
CYCLIC graph, returns a list of nodes."
  (let ((g (graph:copy cyclic))
        (s1 (fset:empty-seq))
        (s2 (fset:empty-seq))
        (dmax))
    (loop while (graph:nodes g)
          do (dolist (receiver (graph:receivers g))
               (setq s2 (fset:with-first s2 receiver))
               (graph:delete-node g receiver))
             (dolist (transmitter (graph:transmitters g))
               (setq s1 (fset:with-last s1 transmitter))
               (graph:delete-node g transmitter))
             (setq dmax (max-degree g))
             (setq s1 (fset:with-last s1 dmax))
             (graph:delete-node g dmax))
    (fset:convert 'list (fset:concat s1 s2))))

(defun max-degree (dag)
  "Given a DAG, returns a node whose degree is a maximum in DAG."
  (let ((max)
        (max-node))
    (dolist (node (graph:nodes dag))
      (when (or (not max) (> (graph:degree dag node) max))
        (setf max (graph:degree dag node))
        (setf max-node node)))
    max-node))

(defun eades-fas (vertex-sequence dag)
  "Given a VERTEX-SEQUENCE produced by Eades' greedy algorithm, and a DAG, returns a list of the 'leftward' arcs in DAG."
  (let ((leftward-arcs))
    (dolist (arc (graph:edges dag))
      (when (> (position (first arc) vertex-sequence) (position (second arc) vertex-sequence))
        (push arc leftward-arcs)))
    leftward-arcs))
