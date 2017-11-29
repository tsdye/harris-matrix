;;; hm-cfg.lisp

;; Copyright (C) Thomas Dye 2017

;; Licensed under the Gnu Public License Version 3 or later

(in-package #:hm)

;; master-table is generated from research.org, Harris matrix, Lookup table
;; 0 = user-section
;; 1 = user-option
;; 2 = element
;; 3 = dot-attribute
;; 4 = graph-type
;; 5 = graph-classification
;; 6 = graph-attribute
;; 7 = domain (currently not used)
;; 8 = default-value
;; use columns 2 and 3 to lookup .ini file section and option in columns 0 and 1
(defun master-table ()
(list '("Output files" "sequence-dot" "none" "none" "sequence" "none" "none" "output" "")
'("Output files" "chronology-dot" "none" "none" "chronology" "none" "none" "output" "")
'("Input files" "contexts" "none" "none" "sequence" "none" "none" "input" "")
'("Input files" "observations" "none" "none" "sequence" "none" "none" "input" "")
'("Input files" "inferences" "none" "none" "sequence" "none" "none" "input" "")
'("Input files" "periods" "none" "none" "sequence" "none" "none" "input" "")
'("Input files" "phases" "none" "none" "sequence" "none" "none" "input" "")
'("Input files" "events" "none" "none" "chronology" "none" "none" "input" "")
'("Input files" "event-order" "none" "none" "chronology" "none" "none" "input" "")
'("Input file headers" "contexts" "none" "none" "sequence" "none" "none" "header" "")
'("Input file headers" "observations" "none" "none" "sequence" "none" "none" "header" "")
'("Input file headers" "inferences" "none" "none" "sequence" "none" "none" "header" "")
'("Input file headers" "periods" "none" "none" "sequence" "none" "none" "header" "")
'("Input file headers" "phases" "none" "none" "sequence" "none" "none" "header" "")
'("Input file headers" "events" "none" "none" "chronology" "none" "none" "header" "")
'("Input file headers" "event-order" "none" "none" "chronology" "none" "none" "header" "")
'("General configuration" "chronology-graph-draw" "none" "none" "chronology" "none" "none" "none" "off")
'("General configuration" "project-directory" "none" "none" "none" "none" "none" "none" "")
'("General configuration" "url-include" "none" "none" "sequence" "none" "none" "none" "off")
'("General configuration" "url-default" "none" "none" "sequence" "none" "none" "none" "http://tsdye.github.io/harris-matrix/")
'("General configuration" "legend" "none" "none" "all" "none" "none" "none" "off")
'("General configuration" "assume-correlations" "none" "none" "sequence" "none" "none" "none" "no")
'("General configuration" "fast-matrix" "none" "none" "sequence" "none" "none" "none" "on")
'("General configuration" "add-interfaces" "none" "none" "sequence" "none" "none" "none" "no")
'("Graphviz sequence classification" "node-fill-by" "node" "none" "sequence" "none" "none" "classification" "")
'("Graphviz sequence classification" "node-fontcolor-by" "node" "none" "sequence" "none" "none" "classification" "")
'("Graphviz sequence classification" "node-shape-by" "node" "none" "sequence" "none" "none" "classification" "units")
'("Graphviz sequence classification" "node-color-by" "node" "none" "sequence" "none" "none" "classification" "")
'("Graphviz sequence classification" "node-penwidth-by" "node" "none" "sequence" "none" "none" "classification" "")
'("Graphviz sequence classification" "node-style-by" "node" "none" "sequence" "none" "none" "classification" "")
'("Graphviz sequence classification" "node-polygon-distortion-by" "node" "none" "sequence" "none" "none" "classification" "")
'("Graphviz sequence classification" "node-polygon-image-by" "node" "none" "sequence" "none" "none" "classification" "")
'("Graphviz sequence classification" "node-polygon-orientation-by" "node" "none" "sequence" "none" "none" "classification" "")
'("Graphviz sequence classification" "node-polygon-sides-by" "node" "none" "sequence" "none" "none" "classification" "")
'("Graphviz sequence classification" "node-polygon-skew-by" "node" "none" "sequence" "none" "none" "classification" "")
'("Graphviz sequence classification" "edge-color-by" "edge" "none" "sequence" "none" "none" "classification" "")
'("Graphviz sequence classification" "edge-fontcolor-by" "edge" "none" "sequence" "none" "none" "classification" "")
'("Graphviz sequence classification" "edge-fontsize-by" "edge" "none" "sequence" "none" "none" "classification" "")
'("Graphviz sequence classification" "edge-penwidth-by" "edge" "none" "sequence" "none" "none" "classification" "")
'("Graphviz sequence classification" "edge-arrowhead-by" "edge" "none" "sequence" "none" "none" "classification" "")
'("Graphviz sequence classification" "edge-style-by" "edge" "none" "sequence" "none" "none" "classification" "")
'("Graphviz sequence graph attributes" "colorscheme" "graph" "colorscheme" "sequence" "none" "none" "graph" "x11")
'("Graphviz sequence graph attributes" "bgcolor" "graph" "bgcolor" "sequence" "none" "none" "graph" "white")
'("Graphviz sequence graph attributes" "fontname" "graph" "fontname" "sequence" "none" "none" "graph" "Helvetica")
'("Graphviz sequence graph attributes" "fontsize" "graph" "fontsize" "sequence" "none" "none" "graph" "14.0")
'("Graphviz sequence graph attributes" "fontcolor" "graph" "fontcolor" "sequence" "none" "none" "graph" "black")
'("Graphviz sequence graph attributes" "label" "graph" "label" "sequence" "none" "none" "graph" "Sequence Diagram")
'("Graphviz sequence graph attributes" "labelloc" "graph" "labelloc" "sequence" "none" "none" "graph" "t")
'("Graphviz sequence graph attributes" "style" "graph" "style" "sequence" "none" "none" "graph" "filled")
'("Graphviz sequence graph attributes" "size" "graph" "size" "sequence" "none" "none" "graph" "6,4!")
'("Graphviz sequence graph attributes" "ratio" "graph" "ratio" "sequence" "none" "none" "graph" "auto")
'("Graphviz sequence graph attributes" "page" "graph" "page" "sequence" "none" "none" "graph" "7,5")
'("Graphviz sequence graph attributes" "dpi" "graph" "dpi" "sequence" "none" "none" "graph" "96")
'("Graphviz sequence graph attributes" "margin" "graph" "margin" "sequence" "none" "none" "graph" "0.5,0.5")
'("Graphviz sequence graph attributes" "label-break" "graph" "label-break" "sequence" "none" "none" "graph" "")
'("Graphviz sequence graph attributes" "fontsize-subscript" "graph" "fontsize-subscript" "sequence" "none" "none" "graph" "10")
'("Graphviz sequence graph attributes" "splines" "graph" "splines" "sequence" "none" "none" "graph" "ortho")
'("Graphviz sequence edge attributes" "edge-classify-by" "edge" "none" "sequence" "none" "none" "edge" "from")
'("Graphviz sequence edge attributes" "colorscheme" "edge" "colorscheme" "sequence" "none" "none" "edge" "x11")
'("Graphviz sequence edge attributes" "style" "edge" "style" "sequence" "none" "none" "edge" "solid")
'("Graphviz sequence edge attributes" "color" "edge" "color" "sequence" "none" "none" "edge" "black")
'("Graphviz sequence edge attributes" "fontname" "edge" "fontname" "sequence" "none" "none" "edge" "Helvetica")
'("Graphviz sequence edge attributes" "fontsize" "edge" "fontsize" "sequence" "none" "none" "edge" "14.0")
'("Graphviz sequence edge attributes" "fontsize-min" "edge" "fontsize-min" "sequence" "none" "none" "edge" "14.0")
'("Graphviz sequence edge attributes" "fontsize-max" "edge" "fontsize-max" "sequence" "none" "none" "edge" "14.0")
'("Graphviz sequence edge attributes" "fontcolor" "edge" "fontcolor" "sequence" "none" "none" "edge" "black")
'("Graphviz sequence edge attributes" "arrowhead" "edge" "arrowhead" "sequence" "none" "none" "edge" "normal")
'("Graphviz sequence edge attributes" "penwidth" "edge" "penwidth" "sequence" "none" "none" "edge" "1.0")
'("Graphviz sequence edge attributes" "penwidth-min" "edge" "penwidth-min" "sequence" "none" "none" "edge" "1.0")
'("Graphviz sequence edge attributes" "penwidth-max" "edge" "penwidth-max" "sequence" "none" "none" "edge" "1.0")
'("Graphviz sequence node attributes" "shape" "node" "shape" "sequence" "none" "none" "node" "box")
'("Graphviz sequence node attributes" "colorscheme" "node" "colorscheme" "sequence" "none" "none" "node" "x11")
'("Graphviz sequence node attributes" "style" "node" "style" "sequence" "none" "none" "node" "filled")
'("Graphviz sequence node attributes" "color" "node" "color" "sequence" "none" "none" "node" "black")
'("Graphviz sequence node attributes" "fontsize" "node" "fontsize" "sequence" "none" "none" "node" "14.0")
'("Graphviz sequence node attributes" "fontsize-min" "node" "fontsize-min" "sequence" "none" "none" "node" "14.0")
'("Graphviz sequence node attributes" "fontsize-max" "node" "fontsize-max" "sequence" "none" "none" "node" "14.0")
'("Graphviz sequence node attributes" "fontcolor" "node" "fontcolor" "sequence" "none" "none" "node" "black")
'("Graphviz sequence node attributes" "fillcolor" "node" "fillcolor" "sequence" "none" "none" "node" "white")
'("Graphviz sequence node attributes" "fontname" "node" "fontname" "sequence" "none" "none" "node" "Helvetica")
'("Graphviz sequence node attributes" "penwidth" "node" "penwidth" "sequence" "none" "none" "node" "1.0")
'("Graphviz sequence node attributes" "penwidth-min" "node" "penwidth-min" "sequence" "none" "none" "node" "1.0")
'("Graphviz sequence node attributes" "penwidth-max" "node" "penwidth-max" "sequence" "none" "none" "node" "1.0")
'("Graphviz sequence node attributes" "polygon-distortion" "node" "polygon-distortion" "sequence" "none" "none" "node" "0.0")
'("Graphviz sequence node attributes" "polygon-image" "node" "polygon-image" "sequence" "none" "none" "node" "")
'("Graphviz sequence node attributes" "polygon-orientation" "node" "polygon-orientation" "sequence" "none" "none" "node" "0")
'("Graphviz sequence node attributes" "polygon-sides" "node" "polygon-sides" "sequence" "none" "none" "node" "4")
'("Graphviz sequence node attributes" "polygon-skew" "node" "polygon-skew" "sequence" "none" "none" "node" "0.0")
'("Reachability configuration" "reachable-from" "node" "reachable-from" "sequence" "none" "reachable" "reachable" "")
'("Reachability configuration" "reachable-limit" "node" "reachable-limit" "sequence" "none" "reachable" "reachable" "")
'("Graphviz sequence reachability node colors" "origin" "node" "origin" "sequence" "node-color-by" "reachable" "reachable" "1")
'("Graphviz sequence reachability node colors" "reachable" "node" "reachable" "sequence" "node-color-by" "reachable" "reachable" "2")
'("Graphviz sequence reachability node colors" "not-reachable" "node" "not-reachable" "sequence" "node-color-by" "reachable" "reachable" "3")
'("Graphviz sequence reachability node colors" "colorscheme" "node" "colorscheme" "sequence" "node-color-by" "reachable" "reachable" "accent3")
'("Graphviz sequence reachability node fillcolors" "origin" "node" "origin" "sequence" "node-fill-by" "reachable" "reachable" "1")
'("Graphviz sequence reachability node fillcolors" "reachable" "node" "reachable" "sequence" "node-fill-by" "reachable" "reachable" "2")
'("Graphviz sequence reachability node fillcolors" "not-reachable" "node" "not-reachable" "sequence" "node-fill-by" "reachable" "reachable" "3")
'("Graphviz sequence reachability node fillcolors" "colorscheme" "node" "colorscheme" "sequence" "node-fill-by" "reachable" "reachable" "accent3")
'("Graphviz sequence reachability node shapes" "origin" "node" "origin" "sequence" "node-shape-by" "reachable" "reachable" "polygon")
'("Graphviz sequence reachability node shapes" "reachable" "node" "reachable" "sequence" "node-shape-by" "reachable" "reachable" "polygon")
'("Graphviz sequence reachability node shapes" "not-reachable" "node" "not-reachable" "sequence" "node-shape-by" "reachable" "reachable" "polygon")
'("Graphviz sequence reachability node styles" "origin" "node" "origin" "sequence" "node-style-by" "reachable" "reachable" "solid,filled")
'("Graphviz sequence reachability node styles" "reachable" "node" "reachable" "sequence" "node-style-by" "reachable" "reachable" "dashed,filled")
'("Graphviz sequence reachability node styles" "not-reachable" "node" "not-reachable" "sequence" "node-style-by" "reachable" "reachable" "dotted,filled")
'("Graphviz sequence reachability node polygon distortion" "origin" "node" "origin" "sequence" "node-polygon-distortion-by" "reachable" "reachable" "0")
'("Graphviz sequence reachability node polygon distortion" "reachable" "node" "reachable" "sequence" "node-polygon-distortion-by" "reachable" "reachable" "50")
'("Graphviz sequence reachability node polygon distortion" "not-reachable" "node" "not-reachable" "sequence" "node-polygon-distortion-by" "reachable" "reachable" "-50")
'("Graphviz sequence reachability node polygon image" "origin" "node" "origin" "sequence" "node-polygon-image-by" "reachable" "reachable" "")
'("Graphviz sequence reachability node polygon image" "reachable" "node" "reachable" "sequence" "node-polygon-image-by" "reachable" "reachable" "")
'("Graphviz sequence reachability node polygon image" "not-reachable" "node" "not-reachable" "sequence" "node-polygon-image-by" "reachable" "reachable" "")
'("Graphviz sequence reachability node polygon orientation" "origin" "node" "origin" "sequence" "node-polygon-orientation-by" "reachable" "reachable" "0")
'("Graphviz sequence reachability node polygon orientation" "reachable" "node" "reachable" "sequence" "node-polygon-orientation-by" "reachable" "reachable" "45")
'("Graphviz sequence reachability node polygon orientation" "not-reachable" "node" "not-reachable" "sequence" "node-polygon-orientation-by" "reachable" "reachable" "90")
'("Graphviz sequence reachability node polygon sides" "origin" "node" "origin" "sequence" "node-polygon-sides-by" "reachable" "reachable" "4")
'("Graphviz sequence reachability node polygon sides" "reachable" "node" "reachable" "sequence" "node-polygon-sides-by" "reachable" "reachable" "4")
'("Graphviz sequence reachability node polygon sides" "not-reachable" "node" "not-reachable" "sequence" "node-polygon-sides-by" "reachable" "reachable" "4")
'("Graphviz sequence reachability node polygon skew" "origin" "node" "origin" "sequence" "node-polygon-skew-by" "reachable" "reachable" "0")
'("Graphviz sequence reachability node polygon skew" "reachable" "node" "reachable" "sequence" "node-polygon-skew-by" "reachable" "reachable" "50")
'("Graphviz sequence reachability node polygon skew" "not-reachable" "node" "not-reachable" "sequence" "node-polygon-skew-by" "reachable" "reachable" "-50")
'("Graphviz sequence reachability node penwidths" "origin" "node" "origin" "sequence" "node-penwidth-by" "reachable" "reachable" "3")
'("Graphviz sequence reachability node penwidths" "reachable" "node" "reachable" "sequence" "node-penwidth-by" "reachable" "reachable" "2")
'("Graphviz sequence reachability node penwidths" "not-reachable" "node" "not-reachable" "sequence" "node-penwidth-by" "reachable" "reachable" "1")
'("Graphviz sequence reachability edge penwidths" "origin" "edge" "origin" "sequence" "edge-penwidth-by" "reachable" "reachable" "3")
'("Graphviz sequence reachability edge penwidths" "reachable" "edge" "reachable" "sequence" "edge-penwidth-by" "reachable" "reachable" "2")
'("Graphviz sequence reachability edge penwidths" "not-reachable" "edge" "not-reachable" "sequence" "edge-penwidth-by" "reachable" "reachable" "1")
'("Graphviz sequence reachability edge colors" "origin" "edge" "origin" "sequence" "edge-color-by" "reachable" "reachable" "1")
'("Graphviz sequence reachability edge colors" "reachable" "edge" "reachable" "sequence" "edge-color-by" "reachable" "reachable" "2")
'("Graphviz sequence reachability edge colors" "not-reachable" "edge" "not-reachable" "sequence" "edge-color-by" "reachable" "reachable" "3")
'("Graphviz sequence reachability edge colors" "colorscheme" "edge" "colorscheme" "sequence" "edge-color-by" "reachable" "reachable" "accent3")
'("Graphviz sequence reachability edge styles" "origin" "edge" "origin" "sequence" "edge-style-by" "reachable" "reachable" "solid")
'("Graphviz sequence reachability edge styles" "reachable" "edge" "reachable" "sequence" "edge-style-by" "reachable" "reachable" "dashed")
'("Graphviz sequence reachability edge styles" "not-reachable" "edge" "not-reachable" "sequence" "edge-style-by" "reachable" "reachable" "dotted")
'("Graphviz sequence reachability edge fontcolors" "origin" "edge" "origin" "sequence" "edge-fontcolor-by" "reachable" "reachable" "1")
'("Graphviz sequence reachability edge fontcolors" "reachable" "edge" "reachable" "sequence" "edge-fontcolor-by" "reachable" "reachable" "2")
'("Graphviz sequence reachability edge fontcolors" "not-reachable" "edge" "not-reachable" "sequence" "edge-fontcolor-by" "reachable" "reachable" "3")
'("Graphviz sequence reachability edge fontcolors" "colorscheme" "edge" "colorscheme" "sequence" "edge-fontcolor-by" "reachable" "reachable" "accent3")
'("Graphviz sequence reachability edge fontsize" "origin" "edge" "origin" "sequence" "edge-fontsize-by" "reachable" "reachable" "14.0")
'("Graphviz sequence reachability edge fontsize" "reachable" "edge" "reachable" "sequence" "edge-fontsize-by" "reachable" "reachable" "14.0")
'("Graphviz sequence reachability edge fontsize" "not-reachable" "edge" "not-reachable" "sequence" "edge-fontsize-by" "reachable" "reachable" "14.0")
'("Adjacency configuration" "origin" "edge" "origin" "sequence" "none" "adjacent" "adjacent" "")
'("Graphviz sequence adjacent node colors" "origin" "node" "origin" "sequence" "node-color-by" "adjacent" "adjacent" "1")
'("Graphviz sequence adjacent node colors" "adjacent" "node" "adjacent" "sequence" "node-color-by" "adjacent" "adjacent" "2")
'("Graphviz sequence adjacent node colors" "not-adjacent" "node" "not-adjacent" "sequence" "node-color-by" "adjacent" "adjacent" "3")
'("Graphviz sequence adjacent node colors" "colorscheme" "node" "colorscheme" "sequence" "node-color-by" "adjacent" "adjacent" "rdylbu3")
'("Graphviz sequence adjacent node fillcolors" "origin" "node" "origin" "sequence" "node-fill-by" "adjacent" "adjacent" "1")
'("Graphviz sequence adjacent node fillcolors" "adjacent" "node" "adjacent" "sequence" "node-fill-by" "adjacent" "adjacent" "2")
'("Graphviz sequence adjacent node fillcolors" "not-adjacent" "node" "not-adjacent" "sequence" "node-fill-by" "adjacent" "adjacent" "3")
'("Graphviz sequence adjacent node fillcolors" "colorscheme" "node" "colorscheme" "sequence" "node-fill-by" "adjacent" "adjacent" "rdylbu3")
'("Graphviz sequence adjacent node shapes" "adjacent" "node" "adjacent" "sequence" "node-shape-by" "adjacent" "adjacent" "polygon")
'("Graphviz sequence adjacent node shapes" "not-adjacent" "node" "not-adjacent" "sequence" "node-shape-by" "adjacent" "adjacent" "polygon")
'("Graphviz sequence adjacent node shapes" "origin" "node" "origin" "sequence" "node-shape-by" "adjacent" "adjacent" "polygon")
'("Graphviz sequence adjacent node styles" "adjacent" "node" "adjacent" "sequence" "node-style-by" "adjacent" "adjacent" "solid,filled")
'("Graphviz sequence adjacent node styles" "not-adjacent" "node" "not-adjacent" "sequence" "node-style-by" "adjacent" "adjacent" "dashed,filled")
'("Graphviz sequence adjacent node styles" "origin" "node" "origin" "sequence" "node-style-by" "adjacent" "adjacent" "dotted,filled")
'("Graphviz sequence adjacent node penwidths" "origin" "node" "origin" "sequence" "node-penwidth-by" "adjacent" "adjacent" "3")
'("Graphviz sequence adjacent node penwidths" "adjacent" "node" "adjacent" "sequence" "node-penwidth-by" "adjacent" "adjacent" "2")
'("Graphviz sequence adjacent node penwidths" "not-adjacent" "node" "not-adjacent" "sequence" "node-penwidth-by" "adjacent" "adjacent" "1")
'("Graphviz sequence adjacent node polygon distortion" "adjacent" "node" "adjacent" "sequence" "node-polygon-distortion-by" "adjacent" "adjacent" "0")
'("Graphviz sequence adjacent node polygon distortion" "not-adjacent" "node" "not-adjacent" "sequence" "node-polygon-distortion-by" "adjacent" "adjacent" "50")
'("Graphviz sequence adjacent node polygon distortion" "origin" "node" "origin" "sequence" "node-polygon-distortion-by" "adjacent" "adjacent" "-50")
'("Graphviz sequence adjacent node polygon image" "adjacent" "node" "adjacent" "sequence" "node-polygon-image-by" "adjacent" "adjacent" "")
'("Graphviz sequence adjacent node polygon image" "not-adjacent" "node" "not-adjacent" "sequence" "node-polygon-image-by" "adjacent" "adjacent" "")
'("Graphviz sequence adjacent node polygon image" "origin" "node" "origin" "sequence" "node-polygon-image-by" "adjacent" "adjacent" "")
'("Graphviz sequence adjacent node polygon orientation" "adjacent" "node" "adjacent" "sequence" "node-polygon-orientation-by" "adjacent" "adjacent" "0")
'("Graphviz sequence adjacent node polygon orientation" "not-adjacent" "node" "not-adjacent" "sequence" "node-polygon-orientation-by" "adjacent" "adjacent" "45")
'("Graphviz sequence adjacent node polygon orientation" "origin" "node" "origin" "sequence" "node-polygon-orientation-by" "adjacent" "adjacent" "90")
'("Graphviz sequence adjacent node polygon sides" "origin" "node" "origin" "sequence" "node-polygon-side-by" "adjacent" "adjacent" "4")
'("Graphviz sequence adjacent node polygon sides" "adjacent" "node" "adjacent" "sequence" "node-polygon-side-by" "adjacent" "adjacent" "4")
'("Graphviz sequence adjacent node polygon sides" "not-adjacent" "node" "not-adjacent" "sequence" "node-polygon-side-by" "adjacent" "adjacent" "4")
'("Graphviz sequence adjacent node polygon skew" "origin" "node" "origin" "sequence" "node-polygon-skew-by" "adjacent" "adjacent" "0")
'("Graphviz sequence adjacent node polygon skew" "adjacent" "node" "adjacent" "sequence" "node-polygon-skew-by" "adjacent" "adjacent" "50")
'("Graphviz sequence adjacent node polygon skew" "not-adjacent" "node" "not-adjacent" "sequence" "node-polygon-skew-by" "adjacent" "adjacent" "-50")
'("Graphviz sequence adjacent edge penwidths" "origin" "edge" "origin" "sequence" "edge-penwidth-by" "adjacent" "adjacent" "3")
'("Graphviz sequence adjacent edge penwidths" "adjacent" "edge" "adjacent" "sequence" "edge-penwidth-by" "adjacent" "adjacent" "2")
'("Graphviz sequence adjacent edge penwidths" "not-adjacent" "edge" "not-adjacent" "sequence" "edge-penwidth-by" "adjacent" "adjacent" "1")
'("Graphviz sequence adjacent edge colors" "origin" "edge" "origin" "sequence" "edge-color-by" "adjacent" "adjacent" "1")
'("Graphviz sequence adjacent edge colors" "adjacent" "edge" "adjacent" "sequence" "edge-color-by" "adjacent" "adjacent" "2")
'("Graphviz sequence adjacent edge colors" "not-adjacent" "edge" "not-adjacent" "sequence" "edge-color-by" "adjacent" "adjacent" "3")
'("Graphviz sequence adjacent edge colors" "colorscheme" "edge" "colorscheme" "sequence" "edge-color-by" "adjacent" "adjacent" "rdylbu3")
'("Graphviz sequence adjacent edge styles" "origin" "edge" "origin" "sequence" "edge-style-by" "adjacent" "adjacent" "solid")
'("Graphviz sequence adjacent edge styles" "adjacent" "edge" "adjacent" "sequence" "edge-style-by" "adjacent" "adjacent" "dashed")
'("Graphviz sequence adjacent edge styles" "not-adjacent" "edge" "not-adjacent" "sequence" "edge-style-by" "adjacent" "adjacent" "dotted")
'("Graphviz sequence adjacent edge fontcolors" "origin" "edge" "origin" "sequence" "edge-fontcolor-by" "adjacent" "adjacent" "1")
'("Graphviz sequence adjacent edge fontcolors" "adjacent" "edge" "adjacent" "sequence" "edge-fontcolor-by" "adjacent" "adjacent" "2")
'("Graphviz sequence adjacent edge fontcolors" "not-adjacent" "edge" "not-adjacent" "sequence" "edge-fontcolor-by" "adjacent" "adjacent" "3")
'("Graphviz sequence adjacent edge fontcolors" "colorscheme" "edge" "colorscheme" "sequence" "edge-fontcolor-by" "adjacent" "adjacent" "rdylbu3")
'("Graphviz sequence adjacent edge fontsize" "origin" "edge" "origin" "sequence" "edge-fontsize-by" "adjacent" "adjacent" "14.0")
'("Graphviz sequence adjacent edge fontsize" "adjacent" "edge" "adjacent" "sequence" "edge-fontsize-by" "adjacent" "adjacent" "14.0")
'("Graphviz sequence adjacent edge fontsize" "not-adjacent" "edge" "not-adjacent" "sequence" "edge-fontsize-by" "adjacent" "adjacent" "14.0")
'("Graphviz sequence unit node shape" "deposit" "node" "deposit" "sequence" "node-shape-by" "units" "units" "box")
'("Graphviz sequence unit node shape" "interface" "node" "interface" "sequence" "node-shape-by" "units" "units" "trapezium")
'("Graphviz sequence unit node color" "deposit" "node" "deposit" "sequence" "node-color-by" "units" "units" "green")
'("Graphviz sequence unit node color" "interface" "node" "interface" "sequence" "node-color-by" "units" "units" "red")
'("Graphviz sequence unit node color" "colorscheme" "node" "colorscheme" "sequence" "node-color-by" "units" "units" "x11")
'("Graphviz sequence unit node fillcolor" "deposit" "node" "deposit" "sequence" "node-fill-by" "units" "units" "green")
'("Graphviz sequence unit node fillcolor" "interface" "node" "interface" "sequence" "node-fill-by" "units" "units" "red")
'("Graphviz sequence unit node fillcolor" "colorscheme" "node" "colorscheme" "sequence" "node-fill-by" "units" "units" "x11")
'("Graphviz sequence unit node penwidth" "deposit" "node" "deposit" "sequence" "node-penwidth-by" "units" "units" "2")
'("Graphviz sequence unit node penwidth" "interface" "node" "interface" "sequence" "node-penwidth-by" "units" "units" "1")
'("Graphviz sequence unit node style" "deposit" "node" "deposit" "sequence" "node-style-by" "units" "units" "solid,filled")
'("Graphviz sequence unit node style" "interface" "node" "interface" "sequence" "node-style-by" "units" "units" "dashed,filled")
'("Graphviz sequence unit node polygon distortion" "deposit" "node" "deposit" "sequence" "node-polygon-distortion-by" "units" "units" "")
'("Graphviz sequence unit node polygon distortion" "interface" "node" "interface" "sequence" "node-polygon-distortion-by" "units" "units" "")
'("Graphviz sequence unit node polygon image" "deposit" "node" "deposit" "sequence" "node-polygon-image-by" "units" "units" "")
'("Graphviz sequence unit node polygon image" "interface" "node" "interface" "sequence" "node-polygon-image-by" "units" "units" "")
'("Graphviz sequence unit node polygon orientation" "deposit" "node" "deposit" "sequence" "node-polygon-orientation-by" "units" "units" "")
'("Graphviz sequence unit node polygon orientation" "interface" "node" "interface" "sequence" "node-polygon-orientation-by" "units" "units" "")
'("Graphviz sequence unit node polygon sides" "deposit" "node" "deposit" "sequence" "node-polygon-sides-by" "units" "units" "")
'("Graphviz sequence unit node polygon sides" "interface" "node" "interface" "sequence" "node-polygon-sides-by" "units" "units" "")
'("Graphviz sequence unit node polygon skew" "deposit" "node" "deposit" "sequence" "node-polygon-skew-by" "units" "units" "")
'("Graphviz sequence unit node polygon skew" "interface" "node" "interface" "sequence" "node-polygon-skew-by" "units" "units" "")
'("Graphviz sequence unit edge color" "deposit" "edge" "deposit" "sequence" "edge-color-by" "units" "units" "green")
'("Graphviz sequence unit edge color" "interface" "edge" "interface" "sequence" "edge-color-by" "units" "units" "red")
'("Graphviz sequence unit edge color" "colorscheme" "edge" "colorscheme" "sequence" "edge-color-by" "units" "units" "x11")
'("Graphviz sequence unit edge fontcolor" "deposit" "edge" "deposit" "sequence" "edge-fontcolor-by" "units" "units" "green")
'("Graphviz sequence unit edge fontcolor" "interface" "edge" "interface" "sequence" "edge-fontcolor-by" "units" "units" "red")
'("Graphviz sequence unit edge fontcolor" "colorscheme" "edge" "colorscheme" "sequence" "edge-fontcolor-by" "units" "units" "x11")
'("Graphviz sequence unit edge penwidth" "deposit" "edge" "deposit" "sequence" "edge-penwidth-by" "units" "units" "2")
'("Graphviz sequence unit edge penwidth" "interface" "edge" "interface" "sequence" "edge-penwidth-by" "units" "units" "1")
'("Graphviz sequence unit edge style" "deposit" "edge" "deposit" "sequence" "edge-style-by" "units" "units" "solid")
'("Graphviz sequence unit edge style" "interface" "edge" "interface" "sequence" "edge-style-by" "units" "units" "dashed")
'("Graphviz chronology graph attributes" "colorscheme" "graph" "colorscheme" "chronology" "none" "none" "" "x11")
'("Graphviz chronology graph attributes" "fontname" "graph" "fontname" "chronology" "none" "none" "" "Time-Roman")
'("Graphviz chronology graph attributes" "fontsize" "graph" "fontsize" "chronology" "none" "none" "" "14.0")
'("Graphviz chronology graph attributes" "fontcolor" "graph" "fontcolor" "chronology" "none" "none" "" "black")
'("Graphviz chronology graph attributes" "label" "graph" "label" "chronology" "none" "none" "" "Chronology Graph")
'("Graphviz chronology graph attributes" "labelloc" "graph" "labelloc" "chronology" "none" "none" "" "t")
'("Graphviz chronology graph attributes" "style" "graph" "style" "chronology" "none" "none" "" "filled")
'("Graphviz chronology graph attributes" "size" "graph" "size" "chronology" "none" "none" """")
'("Graphviz chronology graph attributes" "ratio" "graph" "ratio" "chronology" "none" "none" """")
'("Graphviz chronology graph attributes" "page" "graph" "page" "chronology" "none" "none" """")
'("Graphviz chronology graph attributes" "dpi" "graph" "dpi" "chronology" "none" "none" "" "0.0")
'("Graphviz chronology graph attributes" "margin" "graph" "margin" "chronology" "none" "none" """")
'("Graphviz chronology graph attributes" "bgcolor" "graph" "bgcolor" "chronology" "none" "none" "" "white")
'("Graphviz chronology graph attributes" "label-break" "graph" "label-break" "chronology" "none" "none" """")
'("Graphviz chronology graph attributes" "fontsize-subscript" "graph" "fontsize-subscript" "chronology" "none" "none" """")
'("Graphviz chronology graph attributes" "splines" "graph" "splines" "chronology" "none" "none" "" "ortho")
'("Graphviz chronology node attributes" "colorscheme" "node" "colorscheme" "chronology" "none" "none" """")
'("Graphviz chronology node attributes" "style" "node" "style" "chronology" "none" "none" "" "filled")
'("Graphviz chronology node attributes" "fontname" "node" "fontname" "chronology" "none" "none" "" "Helvetica")
'("Graphviz chronology node attributes" "fontsize" "node" "fontsize" "chronology" "none" "none" "" "14.0")
'("Graphviz chronology node attributes" "fontcolor" "node" "fontcolor" "chronology" "none" "none" "" "black")
'("Graphviz chronology node attributes" "color" "node" "color" "chronology" "none" "none" "" "black")
'("Graphviz chronology node attributes" "fillcolor" "node" "fillcolor" "chronology" "none" "none" "" "white")
'("Graphviz chronology edge attributes" "fontname" "edge" "fontname" "chronology" "none" "none" "" "Helvetica")
'("Graphviz chronology edge attributes" "colorscheme" "edge" "colorscheme" "chronology" "none" "none" """")
'("Graphviz chronology edge attributes" "fontsize" "edge" "fontsize" "chronology" "none" "none" "" "14.0")
'("Graphviz chronology edge attributes" "fontcolor" "edge" "fontcolor" "chronology" "none" "none" "" "black")
'("Graphviz chronology edge attributes" "arrowhead" "edge" "arrowhead" "chronology" "none" "none" "" "normal")
'("Graphviz chronology edge attributes" "sequential" "edge" "sequential" "chronology" "none" "none" "" "solid")
'("Graphviz chronology edge attributes" "abutting" "edge" "abutting" "chronology" "none" "none" "" "dashed")
'("Graphviz chronology edge attributes" "separated" "edge" "separated" "chronology" "none" "none" "" "dotted")
'("Graphviz chronology edge attributes" "color" "edge" "color" "chronology" "none" "none" "" "black")
'("Graphviz chronology node shapes" "phase" "node" "phase" "chronology" "none" "none" "" "box")
'("Graphviz chronology node shapes" "event" "node" "event" "chronology" "none" "none" "" "ellipse")
'("Graphviz colors" "label-dark" "node" "label-dark" "all" "none" "none" "" "black")
'("Graphviz colors" "label-light" "node" "label-light" "all" "none" "none" "" "white")
'("Graphviz legend node attributes" "color" "node" "color" "legend" "none" "none" "" "black")
'("Graphviz legend node attributes" "fillcolor" "node" "fillcolor" "legend" "none" "none" "" "white")
'("Graphviz legend node attributes" "shape" "node" "shape" "legend" "none" "none" "" "box")))

;;API
(defun boolean-strings ()
  (fset:set "1" "yes" "true" "on" "0" "no" "false" "off"))

(defun penwidth-min (cfg element)
  "Returns penwidth-min for the ELEMENT from the user's configuration, CFG.
  ELEMENT is one of `edge', `node'."
  (let ((section (if (string= element "edge")
                     "Graphviz sequence edge attributes"
                     "Graphviz sequence node attributes")))
     (get-option cfg section "penwidth-min")))

(defun penwidth-max (cfg element)
  "Returns penwidth-max for the ELEMENT from the user's configuration, CFG.
  ELEMENT is one of `edge', `node'."
  (let ((section (if (string= element "edge")
                     "Graphviz sequence edge attributes"
                     "Graphviz sequence node attributes")))
    (get-option cfg section "penwidth-max")))

(defun fontsize-min (cfg element)
  "Returns fontsize-min for the ELEMENT from the user's configuration, CFG.
  ELEMENT is one of `edge', `node'."
  (let ((section (if (string= element "edge")
                     "Graphviz sequence edge attributes"
                     "Graphviz sequence node attributes")))
    (get-option cfg section "fontsize-min")))

(defun fontsize-max (cfg element)
  "Returns fontsize-max for the ELEMENT from the user's configuration, CFG.
  ELEMENT is one of `edge', `node'."
  (let ((section (if (string= element "edge")
                     "Graphviz sequence edge attributes"
                     "Graphviz sequence node attributes")))
    (get-option cfg section "fontsize-max")))

(defun fast-matrix-p (cfg)
  "Returns the boolean value for fast-matrix in the user's configuration, CFG,
or nil if CFG contains a value not interpreted by py-configparser as a boolean."
  (let ((value (get-option cfg "General configuration" "fast-matrix")))
    (if (fset:contains? (boolean-strings) value)
        (get-option cfg "General configuration" "fast-matrix" :type :boolean)
        (unless (emptyp value)
          (error "Error: ~s is not valid for fast-matrix." value)))))

;;API
(defun assume-correlations-p (cfg)
  "Returns the boolean value for assume-correlations in the user's
  configuration, CFG, or an error if CFG contains a value not interpreted by
  py-configparser as a boolean."
  (let ((value (get-option cfg "General configuration" "assume-correlations")))
    (if (fset:contains? (boolean-strings) value)
        (get-option cfg "General configuration" "assume-correlations"
                    :type :boolean)
        (unless (emptyp value)
          (error "Error: ~s is not valid for assume-correlations." value)))))

(defun project-directory (cfg)
  "Return a string with the user's project directory, or nil if the option is empty."
  (let ((value (get-option cfg "General configuration" "project-directory")))
    (if (emptyp value) nil value)))

(defun input-file-name-p (cfg content)
  "Return a boolean indicating whether or not the user's configuration, CFG,
  includes a file name for CONTENT. CONTENT is a string, one of `contexts',
  `observations', `inferences', `periods', `phases', `events', or
  `event-order'."
  (not (emptyp (get-option cfg "Input files" content))))

(defun file-header-p (cfg content)
  "Return the boolean value for CONTENT from the `Input file headers' section of
  the user's configuration, CFG, or nil if the value is not a valid boolean
  string. CONTENT is a string, one of `contexts', `observations', `inferences',
  `periods', `phases', `events', or `event-order'."
  (let ((value (get-option cfg "Input file headers" content)))
    (if (fset:contains? (boolean-strings) value)
        (get-option cfg "Input file headers" content :type :boolean)
        (unless (emptyp value)
          (error "Error: ~s is not valid for ~s file header." value content)))))

(defun missing-interfaces-p (cfg)
  "Return the boolean value of `add-missing-interfaces'."
  (let ((value (get-option cfg "General configuration" "add-interfaces")))
    (if (fset:contains? (boolean-strings) value)
        (get-option cfg "General configuration" "add-interfaces" :type :boolean)
        (unless (emptyp value)
          (error "Error: ~s is not valid for add-interfaces." value)))))

(defun graphviz-sequence-graph-attribute (cfg attribute)
  "Return the sequence graph attribute from the user's configuration, CFG."
  (get-option cfg "Graphviz sequence graph attributes" attribute))

(defun graphviz-sequence-edge-attribute (cfg attribute)
  "Return a function that returns the sequence graph edge attribute from the
user's configuration, CFG."
  (let ((attr (get-option cfg "Graphviz sequence edge attributes" attribute)))
    (constantly attr)))

(defun graphviz-sequence-node-attribute (cfg attribute)
  "Return a function that returns the sequence graph node attribute from the
user's configuration, CFG."
  (let ((attr (get-option cfg "Graphviz sequence node attributes" attribute)))
    (constantly attr)))

(defun graphviz-sequence-graph-color (cfg attribute)
  "Return a graphviz color string for the sequence graph ATTRIBUTE from the
  user's configuration, CFG."
  (let ((name (graphviz-sequence-graph-attribute cfg attribute))
        (scheme (graphviz-sequence-graph-attribute cfg "colorscheme")))
    (graphviz-color-string name scheme)))

(defun graphviz-classification (seq element attribute)
  "Return the user-value of the classification if it is set, nil otherwise."
  (let* ((cfg (archaeological-sequence-configuration seq))
         (section "Graphviz sequence classification")
         (option (concatenate 'string element "-" attribute "-by"))
         (user-val (get-option cfg section option)))
    (if (emptyp user-val) nil user-val)))

(defun reachable-limit (cfg)
  "Return the numeric value of `reachable-limit' from the user's configuration,
CFG."
  (get-option cfg "Reachability configuration" "reachable-limit" :type :number))

(defun reachable-from-node (cfg)
  "Returns a symbol from the user's configuration, CFG."
  (symbolicate (get-option cfg "Reachability configuration" "reachable-from")))

(defun chronology-graph-p (cfg)
  "Return a boolean value read from the user's configuration indicating whether
  or not to draw the chronology graph."
  (let ((value (get-option cfg "General configuration" "chronology-graph-draw")))
    (if (fset:contains? (boolean-strings) value)
        (get-option cfg "General configuration" "chronology-graph-draw" :type :boolean)
        (unless (emptyp value)
          (error "Error: ~s is not valid for chronology-graph-draw." value)))))

(defun include-url-p (cfg)
  "Return a boolean value read from the user's configuration, CFG, indicating whether or not to include an URL in the output."
  (let ((value (get-option cfg "General configuration" "url-include")))
    (if (fset:contains? (boolean-strings) value)
        (get-option cfg "General configuration" "url-include" :type :boolean)
        (unless (emptyp value)
          (error "Error: ~s is not valid for url-include." value)))))

(defun default-url (cfg)
  "Return the default URL from the user's configuration, CFG, as a string"
  (get-option cfg "General configuration" "url-default"))

(defun user-color (cfg section option)
  "Given a user's configuration, CFG, a configuration file SECTION and OPTION,
  return a valid Graphviz color string, or the empty string if the user's CFG
  does not indicate a valid color string."
  (let ((color (get-option cfg section option))
        (scheme (get-option cfg section "colorscheme")))
    (if (and (not (emptyp color)) (not (emptyp scheme)))
        (graphviz-color-string color scheme) "")))

(defun sequence-classifier (cfg option)
  "Get the sequence classification, OPTION, from the user's configuration, CFG."
  (get-option cfg "Graphviz sequence classification" option))

;; API

(defun default-configuration ()
  "Convenience function returns the default configuration."
  (make-default-or-empty-configuration (master-table)))

(defun empty-configuration ()
  "Convenience function returns an empty configuration."
  (make-default-or-empty-configuration (master-table) t))

(defun make-default-or-empty-configuration (master &optional empty)
  "Returns a configuration from the values in MASTER. By default, returns the
default configuration. If EMPTY is non-nil, then returns an empty
configuration."
  (let ((cfg (make-config))
        (sections))
    (dolist (row master)
      (let ((section (nth 0 row))
            (option (nth 1 row))
            (default (nth 8 row)))
        (when (not (member section sections))
          (push section sections)
          (add-section cfg section))
        (set-option cfg section option (if empty "" default))))
    cfg))

(defun make-lookup-table (master)
  (let ((map (fset:empty-map)))
    (dolist (row master)
      (let ((key (fset:empty-set)))
        (dolist (column '(1 2 3 4 5))
          (setq key (fset:with key (nth column row))))
        (setq map (fset:with map key (nth 0 row)))))
    map))

(defun make-lookup-map (master)
  "Return an fset map whose key is the set (element dot-attr graph-type
graph-classification graph-attribute) and whose value is a two element list with the
configuration section and option."
  (let ((map (fset:empty-map)))
    (dolist (row master)
      (let ((key (fset:empty-set)))
        (dolist (col '(2 3 4 5 6))
          (setq key (fset:with key (nth col row))))
        (setq map (fset:with map key (list (nth 0 row) (nth 1 row))))))
    map))

(defun section-name (element dot-attr graph-type classification graph-attr)
  (let ((map (make-lookup-map (master-table))))
    (nth 0 (fset:lookup
            map (fset:set element dot-attr graph-type classification graph-attr)))))

(defun option-name (element dot-attr graph-type classification graph-attr)
  (let ((map (make-lookup-map (master-table))))
    (nth 1 (fset:lookup
            map (fset:set element dot-attr graph-type classification graph-attr)))))

(defun lookup-graphviz-option
    (cfg element dot-attr graph-type
     &optional (classification "none") (graph-attr "none"))
  "Returns the value from the user's configuration, CFG, of the attribute,
  DOT-ATTR, associated with graphviz ELEMENT and the hm graph of GRAPH-TYPE.
  GRAPH-TYPE is one of `sequence' `chronology', `legend', or `'. ELEMENT
  is one of `edge', `node', `graph', or `'."
  (get-option cfg (section-name element dot-attr graph-type classification graph-attr)
              (option-name element dot-attr graph-type classification graph-attr)))

(defun lookup-option (cfg option graph-type graph-element domain
                      &optional (classification ""))
  "Returns the value of OPTION from the user configuration CFG associated with a
  key specified by GRAPH-TYPE, GRAPH-ELEMENT, DOMAIN, and CLASSIFICATION.
  GRAPH-TYPE is one of `sequence' `chronology', `legend', or `'. GRAPH-ELEMENT
  is one of `edge', `node', `graph', or `'. DOMAIN is one of `output', `input',
  `header', `general', `classification', `graph', `edge', `node', `reachable',
  `adjacent', `units', or `'. CLASSIFICATION is one of `node-color-by',
  `node-fill-by', `node-shape-by', `node-style-by',
  `node-polygon-distortion-by', `node-polygon-image-by',
  `node-polygon-orientation-by', `node-polygon-sides-by',
  `node-polygon-skew-by', `node-penwidth-by', `edge-penwidth-by',
  `edge-color-by', `edge-style-by', `edge-fontcolor-by', or `'."
  (let ((lookup-table (make-lookup-table (master-table)))
        (key (fset:empty-set))
        (param-list (list option graph-type graph-element domain classification))
        (section))
    (dolist (param param-list)
      (setq key (fset:with key param)))
    (setq section (fset:lookup lookup-table key))
    (get-option cfg section option)))

(defun Graphviz-section-p (section)
  "Given a section name string, SECTION, return true if the section
  contains options for Graphviz configuration.  Function depends on
  the convention of starting such sections with \"Graphviz\"."
  (eq 0 (search "Graphviz" section)))

(defun Graphviz-node-fill-problem? (cfg)
  "If the user wants nodes filled by some graph function, then
  Graphviz requires that node-style-by be empty and the node style
  attribute be `filled'.  Returns nil if all is well, non-nil
  otherwise."
  (if (not (emptyp (get-option cfg "Graphviz sequence classification"
                               "node-fill-by")))
      (progn
        (unless (and
                 (emptyp (get-option cfg "Graphviz sequence classification"
                                     "node-style-by"))
                 (string= (get-option cfg "Graphviz sequence node attributes"
                                      "style") "filled")))
        t)
      nil))

(defun Graphviz-node-polygon-shape-problem? (cfg)
  "Given a configuration, CFG, return nil if the user is set up to
  classify by manipulations of polygon node shape, non-nil otherwise"
  (and (not (or (emptyp (get-option cfg "Graphviz sequence classification"
                                    "node-polygon-distortion-by"))
                (emptyp (get-option cfg "Graphviz sequence classification"
                                    "node-polygon-image-by"))
                (emptyp (get-option cfg "Graphviz sequence classification"
                                    "node-polygon-orientation-by"))
                (emptyp (get-option cfg "Graphviz sequence classification"
                                    "node-polygon-sides-by"))
                (emptyp (get-option cfg "Graphviz sequence classification"
                                    "node-polygon-skew-by"))))
       (emptyp (get-option cfg "Graphviz sequence classification"
                           "node-shape-by"))
       (string= (get-option cfg "Graphviz sequence node attributes"
                            "shape") "polygon")))

(defun origin-node-missing? (cfg)
  "Returns non-nil if an origin node is needed by, but is missing from,
the configuration CFG, nil otherwise."
  (let ((options '("distance" "reachable" "adjacent")))
    (and (emptyp (get-option cfg "Reachability configuration" "reachable-from"))
         (or
          (member (get-option cfg "Graphviz sequence classification"
                              "node-fill-by") options)
          (member (get-option cfg "Graphviz sequence classification"
                              "node-color-by") options)
          (member (get-option cfg "Graphviz sequence classification"
                              "node-shape-by") options)))))

(defun chronology-graph-restrictions-ignored? (cfg)
  "Returns non-nil if the configuration CFG ignores chronology graph
restrictions, nil otherwise."
  (and (get-option cfg "General configuration" "chronology-graph-draw" :type :boolean)
       (get-option cfg "General configuration"
                   "assume-correlations" :type :boolean)))

(defun missing-contexts? (cfg)
  "Returns non-nil if contexts are missing from the configuration CFG,
nil otherwise."
  (let ((option (get-option cfg "Input files" "contexts")))
    (alexandria:emptyp option)))

(defun missing-observations? (cfg)
  "Returns non-nil if observations are missing from the configuration
CFG, nil otherwise."
  (let ((option (get-option cfg "Input files" "observations")))
    (alexandria:emptyp option)))

(defun unable-to-find-input-files? (cfg)
  "Returns non-nil if input files specified in the configuration CFG
can't be found, nil otherwise."
  (let ((option-list (options cfg "Input files"))
        (missing))
    (dolist (option option-list)
      (let ((file-name (get-option cfg "Input files" option))
            (dir (project-directory cfg)))
        (when file-name (unless (probe-file (uiop:merge-pathnames* dir file-name))
                          (push file-name missing)))))
    missing))

(defun reset-option (cfg section-name option-name value)
  "Ensure option names are not inadvertently created when setting an
option."
  (let ((section-list (sections cfg)))
    (assert (member section-name section-list :test #'equal)
            (section-name) "Error: \"~a\" is not one of ~a."
            section-name section-list))
  (let ((option-list (options cfg section-name)))
    (assert (member option-name option-list :test #'equal)
            (option-name) "Error: \"~a\" in not one of ~a."
            option-name option-list))
  (set-option cfg section-name option-name value))

(defun configuration-errors? (cfg)
  "Checks for problems in the configuration CFG, errors out if it
finds one, otherwise returns nil.  Problems include missing input
files, missing configuration values, and contradictory configuration
settings."
  (let ((missing-files (unable-to-find-input-files? cfg)))
    (when missing-files (error "Error: Unable to find ~a" missing-files)))
  (when (missing-contexts? cfg)
    (error "Error: Contexts file not specified."))
  (when (missing-observations? cfg)
    (error "Error: Observations file not specified."))
  (when (chronology-graph-restrictions-ignored? cfg)
    (error "Error: Cannot draw chronology graph when correlations assumed true."))
  (when (origin-node-missing? cfg)
    (error "Error: Origin node must be specified in \"reachable-from\"."))
  (when (Graphviz-node-fill-problem? cfg)
    (error "Error: Graphviz node fills are classified.  Check that \"node-style-by\" is not set and that node \"style\" is \"filled\"."))
  (when (Graphviz-node-polygon-shape-problem? cfg)
    (error "Error: Graphviz node polygons are classified.  Check that \"node-shape-by\" is not set and that node \"shape\" is \"polygon\".")))

(defun set-input-file (cfg option file-name header)
  "If OPTION is recognized, then FILE-NAME and HEADER are registered with the
configuration, CFG.  HEADER is interpreted as a boolean."
  (let ((option-list (options cfg "Input files")))
    (assert (member option option-list :test #'equal)
            (option) "Error: \"~a\" is not one of ~a"
            option option-list)
    (set-option cfg "Input files" option file-name)
    (set-option cfg "Input file headers" option (if header "yes" "no"))))

(defun set-dot-file (cfg option name &optional (verbose t))
  "Registers the chronology output file, NAME, with the OPTION in the
configuration CFG.  Checks if OPTION is known and errors out if not.
If NAME exists and VERBOSE is non-nil, then asks about overwriting it."
  (let ((option-list (options cfg "Output files")))
    (assert (member option option-list :test #'equal)
            (option) "Error: \"~a\" is not one of ~a"
            option option-list)
    (when (and (probe-file name) verbose)
      (unless (yes-or-no-p "Overwrite ~a?" name))
      (return-from set-dot-file))
    (set-option cfg "Output files" option name)))

(defun show-configuration-options (cfg section-name)
  "Print the options in section SECTION-NAME of configuration CFG.
Error out if CFG is not a configuration.  If SECTION-NAME isn't found
in CFG, then let the user try another SECTION-NAME."
  (unless (typep cfg 'config) (error "Error: ~a is not a configuration." cfg))
  (assert (has-section-p cfg section-name) (section-name)
          "Error: Unable to find ~a." section-name)
  (let ((key-val-list (items cfg section-name)))
    (dolist (pair key-val-list)
      (format t "~a = ~a~&" (car pair) (cdr pair)))))

(defun get-configuration-sections (cfg &optional (sort nil))
  "Return a list of sections in the configuration CFG, sorted alphabetically if
SORT is non-nil, or unsorted otherwise.  Assumes that CFG is a configuration."
  (let ((section-list (sections cfg)))
    (when sort (setf section-list (sort section-list #'string<)))
    section-list))

(defun show-configuration-sections (cfg &optional (sort t))
  "Print out the sections in configuration CFG, by default in sorted
order.  If SORT is nil, then print out the unsorted section list.
Errors out if CFG is not a configuration."
  (unless (typep cfg 'config) (error "Error: ~a is not a configuration." cfg))
  (let ((section-list (get-configuration-sections cfg sort)))
    (dolist (section section-list)
      (format t "~a~&" section))))

(defun get-all-configuration-options (cfg)
  "Return a list of the options in all sections of the configuration, CFG.  Assumest that CFG is a configuration."
  (let ((sections (get-configuration-sections cfg))
        (options nil))
    (dolist (section sections)
      (setf options (append options (items cfg section))))
    options))
