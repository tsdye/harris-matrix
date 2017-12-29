;;; hm-memo.lisp
;; Memoize lookup tables used by hm

;; Copyright (C) Thomas Dye 2017

;; Licensed under the Gnu Public License Version 3 or later

(in-package #:hm)

(defun memoize-functions ()
  "Called for its side-effects."
  (fmemo:memoize 'create-distance-matrix)
  (fmemo:memoize 'create-reachability-matrix)
  (fmemo:memoize 'create-adjacency-matrix)
  (fmemo:memoize 'read-table)
  (fmemo:memoize 'cet-map)
  (fmemo:memoize 'make-solarized-map)
  (fmemo:memoize 'make-brewer-map)
  (fmemo:memoize 'svg-map)
  (fmemo:memoize 'graphviz-node-style-map)
  (fmemo:memoize 'graphviz-edge-style-map)
  (fmemo:memoize 'graphviz-node-shape-map)
  (fmemo:memoize 'graphviz-arrow-shape-map)
  (fmemo:memoize 'dot-output-format-map)
  (fmemo:memoize 'x11-colors)
  (fmemo:memoize 'make-classifier))

(defun unmemoize-functions ()
  "Called for its side-effects."
  (fmemo:unmemoize 'create-distance-matrix)
  (fmemo:unmemoize 'create-reachability-matrix)
  (fmemo:unmemoize 'create-adjacency-matrix)
  (fmemo:unmemoize 'read-table)
  (fmemo:unmemoize 'cet-map)
  (fmemo:unmemoize 'make-solarized-map)
  (fmemo:unmemoize 'make-brewer-map)
  (fmemo:unmemoize 'svg-map)
  (fmemo:unmemoize 'graphviz-node-style-map)
  (fmemo:unmemoize 'graphviz-edge-style-map)
  (fmemo:unmemoize 'graphviz-node-shape-map)
  (fmemo:unmemoize 'graphviz-arrow-shape-map)
  (fmemo:unmemoize 'dot-output-format-map)
  (fmemo:unmemoize 'x11-colors)
  (fmemo:unmemoize 'make-classifier))
