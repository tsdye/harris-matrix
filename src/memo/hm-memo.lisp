;;; hm-memo.lisp
;; Memoize lookup tables used by hm

;; Copyright (C) Thomas Dye 2017

;; Licensed under the Gnu Public License Version 3 or later

(in-package #:hm)

(defun memoize-functions ()
  "Called for its side-effects."
  (unmemoize-functions)
  (fmemo:memoize 'create-distance-matrix)
  (fmemo:memoize 'create-reachability-matrix)
  (fmemo:memoize 'create-adjacency-matrix)
  (fmemo:memoize 'create-strong-component-matrix)
  (fmemo:memoize 'make-lookup-table)
  (fmemo:memoize 'cet-map)
  (fmemo:memoize 'make-solarized-map)
  (fmemo:memoize 'make-brewer-map)
  (fmemo:memoize 'svg-map)
  (fmemo:memoize 'graphviz-node-style-map)
  (fmemo:memoize 'graphviz-edge-style-map)
  (fmemo:memoize 'graphviz-node-shape-map)
  (fmemo:memoize 'graphviz-arrow-shape-map))

(defun unmemoize-functions ()
  "Called for its side-effects."
  (fmemo:unmemoize 'create-distance-matrix)
  (fmemo:unmemoize 'create-reachability-matrix)
  (fmemo:unmemoize 'create-adjacency-matrix)
  (fmemo:unmemoize 'create-strong-component-matrix)
  (fmemo:unmemoize 'make-lookup-table)
  (fmemo:unmemoize 'cet-map)
  (fmemo:unmemoize 'make-solarized-map)
  (fmemo:unmemoize 'make-brewer-map)
  (fmemo:unmemoize 'svg-map)
  (fmemo:unmemoize 'graphviz-node-style-map)
  (fmemo:unmemoize 'graphviz-edge-style-map)
  (fmemo:unmemoize 'graphviz-node-shape-map)
  (fmemo:unmemoize 'graphviz-arrow-shape-map))
