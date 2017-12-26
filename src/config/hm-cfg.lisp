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
;; 7 = default-value
;; use columns 2 and 3 to lookup .ini file section and option in columns 0 and 1
(defun master-table ()
(list '("Output files" "sequence-dot" :none :none :sequence :none :none "")
'("Output files" "chronology-dot" :none :none :chronology :none :none "")
'("Input files" "contexts" :none :none :sequence :none :none "")
'("Input files" "observations" :none :none :sequence :none :none "")
'("Input files" "inferences" :none :none :sequence :none :none "")
'("Input files" "periods" :none :none :sequence :none :none "")
'("Input files" "phases" :none :none :sequence :none :none "")
'("Input files" "events" :none :none :chronology :none :none "")
'("Input files" "event-order" :none :none :chronology :none :none "")
'("Input file headers" "contexts" :none :none :sequence :none :none "")
'("Input file headers" "observations" :none :none :sequence :none :none "")
'("Input file headers" "inferences" :none :none :sequence :none :none "")
'("Input file headers" "periods" :none :none :sequence :none :none "")
'("Input file headers" "phases" :none :none :sequence :none :none "")
'("Input file headers" "events" :none :none :chronology :none :none "")
'("Input file headers" "event-order" :none :none :chronology :none :none "")
'("General configuration" "chronology-graph-draw" :none :none :chronology :none :none "off")
'("General configuration" "project-directory" :none :none :none :none :none "")
'("General configuration" "legend" :none :none :all :none :none "off")
'("General configuration" "assume-correlations" :none :none :sequence :none :none "no")
'("General configuration" "fast-matrix" :none :none :sequence :none :none "on")
'("Graph analysis configuration" "distance-from" :node :distance-from :sequence :none :distance "")
'("Graph analysis configuration" "adjacent-from" :edge :origin :sequence :none :adjacent "")
'("Graph analysis configuration" "reachable-from" :node :reachable-from :sequence :none :reachable "")
'("Graph analysis configuration" "reachable-limit" :node :reachable-limit :sequence :none :reachable "")
'("Graphviz sequence classification" "node-fillcolor-by" :node :none :sequence :none :none "")
'("Graphviz sequence classification" "node-fontcolor-by" :node :none :sequence :none :none "")
'("Graphviz sequence classification" "node-shape-by" :node :none :sequence :none :none "units")
'("Graphviz sequence classification" "node-color-by" :node :none :sequence :none :none "")
'("Graphviz sequence classification" "node-penwidth-by" :node :none :sequence :none :none "")
'("Graphviz sequence classification" "node-style-by" :node :none :sequence :none :none "")
'("Graphviz sequence classification" "node-polygon-distortion-by" :node :none :sequence :none :none "")
'("Graphviz sequence classification" "node-polygon-image-by" :node :none :sequence :none :none "")
'("Graphviz sequence classification" "node-polygon-orientation-by" :node :none :sequence :none :none "")
'("Graphviz sequence classification" "node-polygon-sides-by" :node :none :sequence :none :none "")
'("Graphviz sequence classification" "node-polygon-skew-by" :node :none :sequence :none :none "")
'("Graphviz sequence classification" "edge-color-by" :edge :none :sequence :none :none "")
'("Graphviz sequence classification" "edge-fontcolor-by" :edge :none :sequence :none :none "")
'("Graphviz sequence classification" "edge-fontsize-by" :edge :none :sequence :none :none "")
'("Graphviz sequence classification" "edge-penwidth-by" :edge :none :sequence :none :none "")
'("Graphviz sequence classification" "edge-arrowhead-by" :edge :none :sequence :none :none "")
'("Graphviz sequence classification" "edge-style-by" :edge :none :sequence :none :none "")
'("Graphviz sequence graph attributes" "colorscheme" :graph :colorscheme :sequence :none :none "x11")
'("Graphviz sequence graph attributes" "bgcolor" :graph :bgcolor :sequence :none :none "white")
'("Graphviz sequence graph attributes" "fontname" :graph :fontname :sequence :none :none "Helvetica")
'("Graphviz sequence graph attributes" "fontsize" :graph :fontsize :sequence :none :none "14.0")
'("Graphviz sequence graph attributes" "fontcolor" :graph :fontcolor :sequence :none :none "black")
'("Graphviz sequence graph attributes" "label" :graph :label :sequence :none :none "Sequence Diagram")
'("Graphviz sequence graph attributes" "labelloc" :graph :labelloc :sequence :none :none "t")
'("Graphviz sequence graph attributes" "style" :graph :style :sequence :none :none "filled")
'("Graphviz sequence graph attributes" "size" :graph :size :sequence :none :none "6,4!")
'("Graphviz sequence graph attributes" "ratio" :graph :ratio :sequence :none :none "auto")
'("Graphviz sequence graph attributes" "page" :graph :page :sequence :none :none "7,5")
'("Graphviz sequence graph attributes" "dpi" :graph :dpi :sequence :none :none "96")
'("Graphviz sequence graph attributes" "margin" :graph :margin :sequence :none :none "0.5,0.5")
'("Graphviz sequence graph attributes" "fontsize-subscript" :graph :fontsize-subscript :sequence :none :none "10")
'("Graphviz sequence graph attributes" "splines" :graph :splines :sequence :none :none "ortho")
'("Graphviz sequence graph attributes" "url" :graph :url :sequence :none :none "http://tsdye.github.io/harris-matrix/")
'("Graphviz sequence edge attributes" "edge-classify-by" :edge :none :sequence :none :none "from")
'("Graphviz sequence edge attributes" "colorscheme" :edge :colorscheme :sequence :none :none "x11")
'("Graphviz sequence edge attributes" "style" :edge :style :sequence :none :none "solid")
'("Graphviz sequence edge attributes" "color" :edge :color :sequence :none :none "black")
'("Graphviz sequence edge attributes" "fontname" :edge :fontname :sequence :none :none "Helvetica")
'("Graphviz sequence edge attributes" "fontsize" :edge :fontsize :sequence :none :none "14.0")
'("Graphviz sequence edge attributes" "fontsize-min" :edge :fontsize-min :sequence :none :none "14.0")
'("Graphviz sequence edge attributes" "fontsize-max" :edge :fontsize-max :sequence :none :none "14.0")
'("Graphviz sequence edge attributes" "fontcolor" :edge :fontcolor :sequence :none :none "black")
'("Graphviz sequence edge attributes" "arrowhead" :edge :arrowhead :sequence :none :none "normal")
'("Graphviz sequence edge attributes" "penwidth" :edge :penwidth :sequence :none :none "1.0")
'("Graphviz sequence edge attributes" "penwidth-min" :edge :penwidth-min :sequence :none :none "1.0")
'("Graphviz sequence edge attributes" "penwidth-max" :edge :penwidth-max :sequence :none :none "1.0")
'("Graphviz sequence edge attributes" "url" :edge :url :sequence :none :none "http://tsdye.github.io/harris-matrix/")
'("Graphviz sequence edge font color schemes" "levels" :edge :colorscheme :sequence :edge-fontcolor-by :levels "accent")
'("Graphviz sequence edge font color schemes" "distance" :edge :colorscheme :sequence :edge-fontcolor-by :distance "accent")
'("Graphviz sequence edge font color schemes" "periods" :edge :colorscheme :sequence :edge-fontcolor-by :periods "set1")
'("Graphviz sequence edge font color schemes" "phases" :edge :colorscheme :sequence :edge-fontcolor-by :phases "set2")
'("Graphviz sequence edge color schemes" "levels" :edge :colorscheme :sequence :edge-color-by :levels "accent")
'("Graphviz sequence edge color schemes" "distance" :edge :colorscheme :sequence :edge-color-by :distance "accent")
'("Graphviz sequence edge color schemes" "periods" :edge :colorscheme :sequence :edge-color-by :periods "set1")
'("Graphviz sequence edge color schemes" "phases" :edge :colorscheme :sequence :edge-color-by :phases "set2")
'("Graphviz sequence node attributes" "shape" :node :shape :sequence :none :none "box")
'("Graphviz sequence node attributes" "colorscheme" :node :colorscheme :sequence :none :none "x11")
'("Graphviz sequence node attributes" "style" :node :style :sequence :none :none "filled")
'("Graphviz sequence node attributes" "color" :node :color :sequence :none :none "black")
'("Graphviz sequence node attributes" "fontsize" :node :fontsize :sequence :none :none "14.0")
'("Graphviz sequence node attributes" "fontsize-min" :node :fontsize-min :sequence :none :none "6.0")
'("Graphviz sequence node attributes" "fontsize-max" :node :fontsize-max :sequence :none :none "22.0")
'("Graphviz sequence node attributes" "fontcolor" :node :fontcolor :sequence :none :none "black")
'("Graphviz sequence node attributes" "fillcolor" :node :fillcolor :sequence :none :none "white")
'("Graphviz sequence node attributes" "fontname" :node :fontname :sequence :none :none "Helvetica")
'("Graphviz sequence node attributes" "penwidth" :node :penwidth :sequence :none :none "1.0")
'("Graphviz sequence node attributes" "penwidth-min" :node :penwidth-min :sequence :none :none "1.0")
'("Graphviz sequence node attributes" "penwidth-max" :node :penwidth-max :sequence :none :none "1.0")
'("Graphviz sequence node attributes" "polygon-distortion" :node :polygon-distortion :sequence :none :none "0.0")
'("Graphviz sequence node attributes" "polygon-distortion-min" :node :polygon-distortion-min :sequence :none :none "0.0")
'("Graphviz sequence node attributes" "polygon-distortion-max" :node :polygon-distortion-max :sequence :none :none "1.0")
'("Graphviz sequence node attributes" "polygon-image" :node :polygon-image :sequence :none :none "")
'("Graphviz sequence node attributes" "polygon-orientation" :node :polygon-orientation :sequence :none :none "0.0")
'("Graphviz sequence node attributes" "polygon-orientation-min" :node :polygon-orientation-min :sequence :none :none "-10.0")
'("Graphviz sequence node attributes" "polygon-orientation-max" :node :polygon-orientation-max :sequence :none :none "10.0")
'("Graphviz sequence node attributes" "polygon-sides" :node :polygon-sides :sequence :none :none "4")
'("Graphviz sequence node attributes" "polygon-sides-min" :node :polygon-sides-min :sequence :none :none "3")
'("Graphviz sequence node attributes" "polygon-sides-max" :node :polygon-sides-max :sequence :none :none "16")
'("Graphviz sequence node attributes" "polygon-skew" :node :polygon-skew :sequence :none :none "0.0")
'("Graphviz sequence node attributes" "polygon-skew-min" :node :polygon-skew-min :sequence :none :none "-1.0")
'("Graphviz sequence node attributes" "polygon-skew-max" :node :polygon-skew-max :sequence :none :none "1.0")
'("Graphviz sequence node attributes" "url" :node :url :sequence :none :none "http://tsdye.github.io/harris-matrix/")
'("Graphviz sequence node fill color schemes" "levels" :node :colorscheme :sequence :node-fillcolor-by :levels "accent")
'("Graphviz sequence node fill color schemes" "distance" :node :colorscheme :sequence :node-fillcolor-by :distance "accent")
'("Graphviz sequence node fill color schemes" "periods" :node :colorscheme :sequence :node-fillcolor-by :periods "set1")
'("Graphviz sequence node fill color schemes" "phases" :node :colorscheme :sequence :node-fillcolor-by :phases "set2")
'("Graphviz sequence node font color schemes" "levels" :node :colorscheme :sequence :node-fontcolor-by :levels "accent")
'("Graphviz sequence node font color schemes" "distance" :node :colorscheme :sequence :node-fontcolor-by :distance "accent")
'("Graphviz sequence node font color schemes" "periods" :node :colorscheme :sequence :node-fontcolor-by :periods "set1")
'("Graphviz sequence node font color schemes" "phases" :node :colorscheme :sequence :node-fontcolor-by :phases "set2")
'("Graphviz sequence node color schemes" "levels" :node :colorscheme :sequence :node-color-by :levels "accent")
'("Graphviz sequence node color schemes" "distance" :node :colorscheme :sequence :node-color-by :distance "accent")
'("Graphviz sequence node color schemes" "periods" :node :colorscheme :sequence :node-color-by :periods "set1")
'("Graphviz sequence node color schemes" "phases" :node :colorscheme :sequence :node-color-by :phases "set2")
'("Graphviz sequence reachability node colors" "origin" :node :origin :sequence :node-color-by :reachable "0")
'("Graphviz sequence reachability node colors" "reachable" :node :reachable :sequence :node-color-by :reachable "1")
'("Graphviz sequence reachability node colors" "not-reachable" :node :not-reachable :sequence :node-color-by :reachable "2")
'("Graphviz sequence reachability node colors" "colorscheme" :node :colorscheme :sequence :node-color-by :reachable "accent")
'("Graphviz sequence reachability node fillcolors" "origin" :node :origin :sequence :node-fillcolor-by :reachable "0")
'("Graphviz sequence reachability node fillcolors" "reachable" :node :reachable :sequence :node-fillcolor-by :reachable "1")
'("Graphviz sequence reachability node fillcolors" "not-reachable" :node :not-reachable :sequence :node-fillcolor-by :reachable "2")
'("Graphviz sequence reachability node fillcolors" "colorscheme" :node :colorscheme :sequence :node-fillcolor-by :reachable "accent")
'("Graphviz sequence reachability node fontcolors" "origin" :node :origin :sequence :node-fontcolor-by :reachable "0")
'("Graphviz sequence reachability node fontcolors" "reachable" :node :reachable :sequence :node-fontcolor-by :reachable "1")
'("Graphviz sequence reachability node fontcolors" "not-reachable" :node :not-reachable :sequence :node-fontcolor-by :reachable "2")
'("Graphviz sequence reachability node fontcolors" "colorscheme" :node :colorscheme :sequence :node-fontcolor-by :reachable "accent")
'("Graphviz sequence reachability node shapes" "origin" :node :origin :sequence :node-shape-by :reachable "polygon")
'("Graphviz sequence reachability node shapes" "reachable" :node :reachable :sequence :node-shape-by :reachable "polygon")
'("Graphviz sequence reachability node shapes" "not-reachable" :node :not-reachable :sequence :node-shape-by :reachable "polygon")
'("Graphviz sequence reachability node styles" "origin" :node :origin :sequence :node-style-by :reachable "solid,filled")
'("Graphviz sequence reachability node styles" "reachable" :node :reachable :sequence :node-style-by :reachable "dashed,filled")
'("Graphviz sequence reachability node styles" "not-reachable" :node :not-reachable :sequence :node-style-by :reachable "dotted,filled")
'("Graphviz sequence reachability node polygon distortion" "origin" :node :origin :sequence :node-polygon-distortion-by :reachable "0")
'("Graphviz sequence reachability node polygon distortion" "reachable" :node :reachable :sequence :node-polygon-distortion-by :reachable "50")
'("Graphviz sequence reachability node polygon distortion" "not-reachable" :node :not-reachable :sequence :node-polygon-distortion-by :reachable "-50")
'("Graphviz sequence reachability node polygon image" "origin" :node :origin :sequence :node-polygon-image-by :reachable "")
'("Graphviz sequence reachability node polygon image" "reachable" :node :reachable :sequence :node-polygon-image-by :reachable "")
'("Graphviz sequence reachability node polygon image" "not-reachable" :node :not-reachable :sequence :node-polygon-image-by :reachable "")
'("Graphviz sequence reachability node polygon orientation" "origin" :node :origin :sequence :node-polygon-orientation-by :reachable "0")
'("Graphviz sequence reachability node polygon orientation" "reachable" :node :reachable :sequence :node-polygon-orientation-by :reachable "45")
'("Graphviz sequence reachability node polygon orientation" "not-reachable" :node :not-reachable :sequence :node-polygon-orientation-by :reachable "90")
'("Graphviz sequence reachability node polygon sides" "origin" :node :origin :sequence :node-polygon-sides-by :reachable "4")
'("Graphviz sequence reachability node polygon sides" "reachable" :node :reachable :sequence :node-polygon-sides-by :reachable "4")
'("Graphviz sequence reachability node polygon sides" "not-reachable" :node :not-reachable :sequence :node-polygon-sides-by :reachable "4")
'("Graphviz sequence reachability node polygon skew" "origin" :node :origin :sequence :node-polygon-skew-by :reachable "0")
'("Graphviz sequence reachability node polygon skew" "reachable" :node :reachable :sequence :node-polygon-skew-by :reachable "50")
'("Graphviz sequence reachability node polygon skew" "not-reachable" :node :not-reachable :sequence :node-polygon-skew-by :reachable "-50")
'("Graphviz sequence reachability node penwidths" "origin" :node :origin :sequence :node-penwidth-by :reachable "3")
'("Graphviz sequence reachability node penwidths" "reachable" :node :reachable :sequence :node-penwidth-by :reachable "2")
'("Graphviz sequence reachability node penwidths" "not-reachable" :node :not-reachable :sequence :node-penwidth-by :reachable "1")
'("Graphviz sequence reachability edge penwidths" "origin" :edge :origin :sequence :edge-penwidth-by :reachable "3")
'("Graphviz sequence reachability edge penwidths" "reachable" :edge :reachable :sequence :edge-penwidth-by :reachable "2")
'("Graphviz sequence reachability edge penwidths" "not-reachable" :edge :not-reachable :sequence :edge-penwidth-by :reachable "1")
'("Graphviz sequence reachability edge colors" "origin" :edge :origin :sequence :edge-color-by :reachable "0")
'("Graphviz sequence reachability edge colors" "reachable" :edge :reachable :sequence :edge-color-by :reachable "1")
'("Graphviz sequence reachability edge colors" "not-reachable" :edge :not-reachable :sequence :edge-color-by :reachable "2")
'("Graphviz sequence reachability edge colors" "colorscheme" :edge :colorscheme :sequence :edge-color-by :reachable "accent")
'("Graphviz sequence reachability edge styles" "origin" :edge :origin :sequence :edge-style-by :reachable "solid")
'("Graphviz sequence reachability edge styles" "reachable" :edge :reachable :sequence :edge-style-by :reachable "dashed")
'("Graphviz sequence reachability edge styles" "not-reachable" :edge :not-reachable :sequence :edge-style-by :reachable "dotted")
'("Graphviz sequence reachability edge fontcolors" "origin" :edge :origin :sequence :edge-fontcolor-by :reachable "0")
'("Graphviz sequence reachability edge fontcolors" "reachable" :edge :reachable :sequence :edge-fontcolor-by :reachable "1")
'("Graphviz sequence reachability edge fontcolors" "not-reachable" :edge :not-reachable :sequence :edge-fontcolor-by :reachable "2")
'("Graphviz sequence reachability edge fontcolors" "colorscheme" :edge :colorscheme :sequence :edge-fontcolor-by :reachable "accent")
'("Graphviz sequence reachability edge fontsize" "origin" :edge :origin :sequence :edge-fontsize-by :reachable "14.0")
'("Graphviz sequence reachability edge fontsize" "reachable" :edge :reachable :sequence :edge-fontsize-by :reachable "14.0")
'("Graphviz sequence reachability edge fontsize" "not-reachable" :edge :not-reachable :sequence :edge-fontsize-by :reachable "14.0")
'("Graphviz sequence reachability edge arrowhead" "origin" :edge :origin :sequence :edge-arrowhead-by :reachable "none")
'("Graphviz sequence reachability edge arrowhead" "reachable" :edge :reachable :sequence :edge-arrowhead-by :reachable "normal")
'("Graphviz sequence reachability edge arrowhead" "not-reachable" :edge :not-reachable :sequence :edge-arrowhead-by :reachable "open")
'("Graphviz sequence adjacent node colors" "origin" :node :origin :sequence :node-color-by :adjacent "0")
'("Graphviz sequence adjacent node colors" "adjacent" :node :adjacent :sequence :node-color-by :adjacent "1")
'("Graphviz sequence adjacent node colors" "not-adjacent" :node :not-adjacent :sequence :node-color-by :adjacent "2")
'("Graphviz sequence adjacent node colors" "colorscheme" :node :colorscheme :sequence :node-color-by :adjacent "rdylbu")
'("Graphviz sequence adjacent node fillcolors" "origin" :node :origin :sequence :node-fillcolor-by :adjacent "0")
'("Graphviz sequence adjacent node fillcolors" "adjacent" :node :adjacent :sequence :node-fillcolor-by :adjacent "1")
'("Graphviz sequence adjacent node fillcolors" "not-adjacent" :node :not-adjacent :sequence :node-fillcolor-by :adjacent "2")
'("Graphviz sequence adjacent node fillcolors" "colorscheme" :node :colorscheme :sequence :node-fillcolor-by :adjacent "rdylbu")
'("Graphviz sequence adjacent node fontcolors" "origin" :node :origin :sequence :node-fontcolor-by :adjacent "0")
'("Graphviz sequence adjacent node fontcolors" "adjacent" :node :adjacent :sequence :node-fontcolor-by :adjacent "1")
'("Graphviz sequence adjacent node fontcolors" "not-adjacent" :node :not-adjacent :sequence :node-fontcolor-by :adjacent "2")
'("Graphviz sequence adjacent node fontcolors" "colorscheme" :node :colorscheme :sequence :node-fontcolor-by :adjacent "dark2")
'("Graphviz sequence adjacent node shapes" "adjacent" :node :adjacent :sequence :node-shape-by :adjacent "polygon")
'("Graphviz sequence adjacent node shapes" "not-adjacent" :node :not-adjacent :sequence :node-shape-by :adjacent "polygon")
'("Graphviz sequence adjacent node shapes" "origin" :node :origin :sequence :node-shape-by :adjacent "polygon")
'("Graphviz sequence adjacent node styles" "adjacent" :node :adjacent :sequence :node-style-by :adjacent "solid,filled")
'("Graphviz sequence adjacent node styles" "not-adjacent" :node :not-adjacent :sequence :node-style-by :adjacent "dashed,filled")
'("Graphviz sequence adjacent node styles" "origin" :node :origin :sequence :node-style-by :adjacent "dotted,filled")
'("Graphviz sequence adjacent node penwidths" "origin" :node :origin :sequence :node-penwidth-by :adjacent "3")
'("Graphviz sequence adjacent node penwidths" "adjacent" :node :adjacent :sequence :node-penwidth-by :adjacent "2")
'("Graphviz sequence adjacent node penwidths" "not-adjacent" :node :not-adjacent :sequence :node-penwidth-by :adjacent "1")
'("Graphviz sequence adjacent node polygon distortion" "adjacent" :node :adjacent :sequence :node-polygon-distortion-by :adjacent "0")
'("Graphviz sequence adjacent node polygon distortion" "not-adjacent" :node :not-adjacent :sequence :node-polygon-distortion-by :adjacent "50")
'("Graphviz sequence adjacent node polygon distortion" "origin" :node :origin :sequence :node-polygon-distortion-by :adjacent "-50")
'("Graphviz sequence adjacent node polygon image" "adjacent" :node :adjacent :sequence :node-polygon-image-by :adjacent "")
'("Graphviz sequence adjacent node polygon image" "not-adjacent" :node :not-adjacent :sequence :node-polygon-image-by :adjacent "")
'("Graphviz sequence adjacent node polygon image" "origin" :node :origin :sequence :node-polygon-image-by :adjacent "")
'("Graphviz sequence adjacent node polygon orientation" "adjacent" :node :adjacent :sequence :node-polygon-orientation-by :adjacent "0")
'("Graphviz sequence adjacent node polygon orientation" "not-adjacent" :node :not-adjacent :sequence :node-polygon-orientation-by :adjacent "45")
'("Graphviz sequence adjacent node polygon orientation" "origin" :node :origin :sequence :node-polygon-orientation-by :adjacent "90")
'("Graphviz sequence adjacent node polygon sides" "origin" :node :origin :sequence :node-polygon-side-by :adjacent "4")
'("Graphviz sequence adjacent node polygon sides" "adjacent" :node :adjacent :sequence :node-polygon-side-by :adjacent "4")
'("Graphviz sequence adjacent node polygon sides" "not-adjacent" :node :not-adjacent :sequence :node-polygon-side-by :adjacent "4")
'("Graphviz sequence adjacent node polygon skew" "origin" :node :origin :sequence :node-polygon-skew-by :adjacent "0")
'("Graphviz sequence adjacent node polygon skew" "adjacent" :node :adjacent :sequence :node-polygon-skew-by :adjacent "50")
'("Graphviz sequence adjacent node polygon skew" "not-adjacent" :node :not-adjacent :sequence :node-polygon-skew-by :adjacent "-50")
'("Graphviz sequence adjacent edge penwidths" "origin" :edge :origin :sequence :edge-penwidth-by :adjacent "3")
'("Graphviz sequence adjacent edge penwidths" "adjacent" :edge :adjacent :sequence :edge-penwidth-by :adjacent "2")
'("Graphviz sequence adjacent edge penwidths" "not-adjacent" :edge :not-adjacent :sequence :edge-penwidth-by :adjacent "1")
'("Graphviz sequence adjacent edge colors" "origin" :edge :origin :sequence :edge-color-by :adjacent "0")
'("Graphviz sequence adjacent edge colors" "adjacent" :edge :adjacent :sequence :edge-color-by :adjacent "1")
'("Graphviz sequence adjacent edge colors" "not-adjacent" :edge :not-adjacent :sequence :edge-color-by :adjacent "2")
'("Graphviz sequence adjacent edge colors" "colorscheme" :edge :colorscheme :sequence :edge-color-by :adjacent "rdylbu")
'("Graphviz sequence adjacent edge styles" "origin" :edge :origin :sequence :edge-style-by :adjacent "solid")
'("Graphviz sequence adjacent edge styles" "adjacent" :edge :adjacent :sequence :edge-style-by :adjacent "dashed")
'("Graphviz sequence adjacent edge styles" "not-adjacent" :edge :not-adjacent :sequence :edge-style-by :adjacent "dotted")
'("Graphviz sequence adjacent edge fontcolor" "origin" :edge :origin :sequence :edge-fontcolor-by :adjacent "0")
'("Graphviz sequence adjacent edge fontcolor" "adjacent" :edge :adjacent :sequence :edge-fontcolor-by :adjacent "1")
'("Graphviz sequence adjacent edge fontcolor" "not-adjacent" :edge :not-adjacent :sequence :edge-fontcolor-by :adjacent "2")
'("Graphviz sequence adjacent edge fontcolor" "colorscheme" :edge :colorscheme :sequence :edge-fontcolor-by :adjacent "rdylbu")
'("Graphviz sequence adjacent edge fontsize" "origin" :edge :origin :sequence :edge-fontsize-by :adjacent "14.0")
'("Graphviz sequence adjacent edge fontsize" "adjacent" :edge :adjacent :sequence :edge-fontsize-by :adjacent "14.0")
'("Graphviz sequence adjacent edge fontsize" "not-adjacent" :edge :not-adjacent :sequence :edge-fontsize-by :adjacent "14.0")
'("Graphviz sequence adjacent edge arrowhead" "origin" :edge :origin :sequence :edge-arrowhead-by :adjacent "none")
'("Graphviz sequence adjacent edge arrowhead" "adjacent" :edge :adjacent :sequence :edge-arrowhead-by :adjacent "normal")
'("Graphviz sequence adjacent edge arrowhead" "not-adjacent" :edge :not-adjacent :sequence :edge-arrowhead-by :adjacent "open")
'("Graphviz sequence unit node shape" "deposit" :node :deposit :sequence :node-shape-by :units "box")
'("Graphviz sequence unit node shape" "interface" :node :interface :sequence :node-shape-by :units "trapezium")
'("Graphviz sequence unit node color" "deposit" :node :deposit :sequence :node-color-by :units "green")
'("Graphviz sequence unit node color" "interface" :node :interface :sequence :node-color-by :units "red")
'("Graphviz sequence unit node color" "colorscheme" :node :colorscheme :sequence :node-color-by :units "x11")
'("Graphviz sequence unit node fillcolor" "deposit" :node :deposit :sequence :node-fillcolor-by :units "green")
'("Graphviz sequence unit node fillcolor" "interface" :node :interface :sequence :node-fillcolor-by :units "red")
'("Graphviz sequence unit node fillcolor" "colorscheme" :node :colorscheme :sequence :node-fillcolor-by :units "x11")
'("Graphviz sequence unit node fontcolor" "deposit" :node :deposit :sequence :node-fontcolor-by :units "green")
'("Graphviz sequence unit node fontcolor" "interface" :node :interface :sequence :node-fontcolor-by :units "red")
'("Graphviz sequence unit node fontcolor" "colorscheme" :node :colorscheme :sequence :node-fontcolor-by :units "x11")
'("Graphviz sequence unit node penwidth" "deposit" :node :deposit :sequence :node-penwidth-by :units "2")
'("Graphviz sequence unit node penwidth" "interface" :node :interface :sequence :node-penwidth-by :units "1")
'("Graphviz sequence unit node style" "deposit" :node :deposit :sequence :node-style-by :units "solid,filled")
'("Graphviz sequence unit node style" "interface" :node :interface :sequence :node-style-by :units "dashed,filled")
'("Graphviz sequence unit node polygon distortion" "deposit" :node :deposit :sequence :node-polygon-distortion-by :units "")
'("Graphviz sequence unit node polygon distortion" "interface" :node :interface :sequence :node-polygon-distortion-by :units "")
'("Graphviz sequence unit node polygon image" "deposit" :node :deposit :sequence :node-polygon-image-by :units "")
'("Graphviz sequence unit node polygon image" "interface" :node :interface :sequence :node-polygon-image-by :units "")
'("Graphviz sequence unit node polygon orientation" "deposit" :node :deposit :sequence :node-polygon-orientation-by :units "")
'("Graphviz sequence unit node polygon orientation" "interface" :node :interface :sequence :node-polygon-orientation-by :units "")
'("Graphviz sequence unit node polygon sides" "deposit" :node :deposit :sequence :node-polygon-sides-by :units "")
'("Graphviz sequence unit node polygon sides" "interface" :node :interface :sequence :node-polygon-sides-by :units "")
'("Graphviz sequence unit node polygon skew" "deposit" :node :deposit :sequence :node-polygon-skew-by :units "")
'("Graphviz sequence unit node polygon skew" "interface" :node :interface :sequence :node-polygon-skew-by :units "")
'("Graphviz sequence unit edge color" "deposit" :edge :deposit :sequence :edge-color-by :units "green")
'("Graphviz sequence unit edge color" "interface" :edge :interface :sequence :edge-color-by :units "red")
'("Graphviz sequence unit edge color" "colorscheme" :edge :colorscheme :sequence :edge-color-by :units "x11")
'("Graphviz sequence unit edge fontcolor" "deposit" :edge :deposit :sequence :edge-fontcolor-by :units "green")
'("Graphviz sequence unit edge fontcolor" "interface" :edge :interface :sequence :edge-fontcolor-by :units "red")
'("Graphviz sequence unit edge fontcolor" "colorscheme" :edge :colorscheme :sequence :edge-fontcolor-by :units "x11")
'("Graphviz sequence unit edge penwidth" "deposit" :edge :deposit :sequence :edge-penwidth-by :units "2")
'("Graphviz sequence unit edge penwidth" "interface" :edge :interface :sequence :edge-penwidth-by :units "1")
'("Graphviz sequence unit edge style" "deposit" :edge :deposit :sequence :edge-style-by :units "solid")
'("Graphviz sequence unit edge style" "interface" :edge :interface :sequence :edge-style-by :units "dashed")
'("Graphviz sequence unit edge arrowhead" "deposit" :edge :deposit :sequence :edge-arrowhead-by :units "none")
'("Graphviz sequence unit edge arrowhead" "interface" :edge :interface :sequence :edge-arrowhead-by :units "normal")
'("Graphviz sequence unit edge fontsize" "deposit" :edge :deposit :sequence :edge-fontsize-by :units "14.0")
'("Graphviz sequence unit edge fontsize" "interface" :edge :interface :sequence :edge-fontsize-by :units "10.0")
'("Graphviz chronology graph attributes" "colorscheme" :graph :colorscheme :chronology :none :none "x11")
'("Graphviz chronology graph attributes" "fontname" :graph :fontname :chronology :none :none "Time-Roman")
'("Graphviz chronology graph attributes" "fontsize" :graph :fontsize :chronology :none :none "14.0")
'("Graphviz chronology graph attributes" "fontcolor" :graph :fontcolor :chronology :none :none "black")
'("Graphviz chronology graph attributes" "label" :graph :label :chronology :none :none "Chronology Graph")
'("Graphviz chronology graph attributes" "labelloc" :graph :labelloc :chronology :none :none "t")
'("Graphviz chronology graph attributes" "style" :graph :style :chronology :none :none "filled")
'("Graphviz chronology graph attributes" "size" :graph :size :chronology :none :none "6,4!")
'("Graphviz chronology graph attributes" "ratio" :graph :ratio :chronology :none :none "auto")
'("Graphviz chronology graph attributes" "page" :graph :page :chronology :none :none "7,5")
'("Graphviz chronology graph attributes" "dpi" :graph :dpi :chronology :none :none "0.0")
'("Graphviz chronology graph attributes" "margin" :graph :margin :chronology :none :none "0.5,0.5")
'("Graphviz chronology graph attributes" "bgcolor" :graph :bgcolor :chronology :none :none "white")
'("Graphviz chronology graph attributes" "fontsize-subscript" :graph :fontsize-subscript :chronology :none :none "10.0")
'("Graphviz chronology graph attributes" "splines" :graph :splines :chronology :none :none "ortho")
'("Graphviz chronology node attributes" "colorscheme" :node :colorscheme :chronology :none :none "x11")
'("Graphviz chronology node attributes" "style" :node :style :chronology :none :none "filled")
'("Graphviz chronology node attributes" "fontname" :node :fontname :chronology :none :none "Helvetica")
'("Graphviz chronology node attributes" "fontsize" :node :fontsize :chronology :none :none "14.0")
'("Graphviz chronology node attributes" "fontcolor" :node :fontcolor :chronology :none :none "black")
'("Graphviz chronology node attributes" "color" :node :color :chronology :none :none "black")
'("Graphviz chronology node attributes" "fillcolor" :node :fillcolor :chronology :none :none "white")
'("Graphviz chronology node attributes" "phase" :node :phase :chronology :none :none "box")
'("Graphviz chronology node attributes" "event" :node :event :chronology :none :none "ellipse")
'("Graphviz chronology edge attributes" "fontname" :edge :fontname :chronology :none :none "Helvetica")
'("Graphviz chronology edge attributes" "colorscheme" :edge :colorscheme :chronology :none :none "x11")
'("Graphviz chronology edge attributes" "fontsize" :edge :fontsize :chronology :none :none "14.0")
'("Graphviz chronology edge attributes" "fontcolor" :edge :fontcolor :chronology :none :none "black")
'("Graphviz chronology edge attributes" "arrowhead" :edge :arrowhead :chronology :none :none "normal")
'("Graphviz chronology edge attributes" "sequential" :edge :sequential :chronology :none :none "solid")
'("Graphviz chronology edge attributes" "abutting" :edge :abutting :chronology :none :none "dashed")
'("Graphviz chronology edge attributes" "separated" :edge :separated :chronology :none :none "dotted")
'("Graphviz chronology edge attributes" "color" :edge :color :chronology :none :none "black")
'("Graphviz legend node attributes" "color" :node :color :legend :none :none "black")
'("Graphviz legend node attributes" "fillcolor" :node :fillcolor :legend :none :none "white")
'("Graphviz legend node attributes" "shape" :node :shape :legend :none :none "box")))

(defun boolean-strings ()
  (fset:set "1" "yes" "true" "on" "0" "no" "false" "off"))

(defun boolean-string-p (candidate)
  (fset:contains? (boolean-strings) candidate))

(defun penwidth-min (cfg element)
  "Returns penwidth-min for the ELEMENT from the user's configuration, CFG.
  ELEMENT is one of `edge', `node'."
  (let ((section (if (eq element :edge)
                     "Graphviz sequence edge attributes"
                     "Graphviz sequence node attributes")))
     (get-option cfg section "penwidth-min" :type :number)))

(defun penwidth-max (cfg element)
  "Returns penwidth-max for the ELEMENT from the user's configuration, CFG.
  ELEMENT is one of `edge', `node'."
  (let ((section (if (eq element :edge)
                     "Graphviz sequence edge attributes"
                     "Graphviz sequence node attributes")))
    (get-option cfg section "penwidth-max" :type :number)))

(defun fontsize-min (cfg element)
  "Returns fontsize-min for the ELEMENT from the user's configuration, CFG.
  ELEMENT is one of `edge', `node'."
  (let ((section (if (eq element :edge)
                     "Graphviz sequence edge attributes"
                     "Graphviz sequence node attributes")))
    (get-option cfg section "fontsize-min" :type :number)))

(defun fontsize-max (cfg element)
  "Returns FONTSIZE-MAX for the ELEMENT from the user's configuration, CFG.
  ELEMENT is one of `edge', `node'."
  (let ((section (if (eq element :edge)
                     "Graphviz sequence edge attributes"
                     "Graphviz sequence node attributes")))
    (get-option cfg section "fontsize-max" :type :number)))

(defun polygon-skew-max (cfg)
  "Returns POLYGON-SKEW-MAX from the user's configuration, CFG."
  (get-option cfg "Graphviz sequence node attributes" "polygon-skew-max" :type :number))

(defun polygon-skew-min (cfg)
  "Returns POLYGON-SKEW-MIN from the user's configuration, CFG."
  (get-option cfg "Graphviz sequence node attributes" "polygon-skew-min" :type :number))

(defun polygon-sides-max (cfg)
  "Returns POLYGON-SIDES-MAX from the user's configuration, CFG."
  (get-option cfg "Graphviz sequence node attributes" "polygon-sides-max" :type :number))

(defun polygon-sides-min (cfg)
  "Returns POLYGON-SIDES-MIN from the user's configuration, CFG."
  (get-option cfg "Graphviz sequence node attributes" "polygon-sides-min" :type :number))

(defun polygon-orientation-max (cfg)
  "Returns POLYGON-ORIENTATION-MAX from the user's configuration, CFG."
  (get-option cfg "Graphviz sequence node attributes" "polygon-orientation-max"
              :type :number))

(defun polygon-orientation-min (cfg)
  "Returns POLYGON-ORIENTATION-MIN from the user's configuration, CFG."
  (get-option cfg "Graphviz sequence node attributes" "polygon-orientation-min"
              :type :number))

(defun polygon-distortion-max (cfg)
  "Returns POLYGON-DISTORTION-MAX from the user's configuration, CFG."
  (get-option cfg "Graphviz sequence node attributes" "polygon-distortion-max"
              :type :number))

(defun polygon-distortion-min (cfg)
  "Returns POLYGON-DISTORTION-MIN from the user's configuration, CFG."
  (get-option cfg "Graphviz sequence node attributes" "polygon-distortion-min"
              :type :number))

(defun edge-classify-by (seq)
  "Returns the value of EDGE-CLASSIFY-BY from the user's configuration in SEQ."
  (let ((cfg (archaeological-sequence-configuration seq)))
    (classification-to-keyword
     (get-option cfg "Graphviz sequence edge attributes" "edge-classify-by"))))

(defun fast-matrix-p (cfg)
  "Returns the boolean value for fast-matrix in the user's configuration, CFG,
or nil if CFG contains a value not interpreted by py-configparser as a boolean."
  (let ((value (get-option cfg "General configuration" "fast-matrix")))
    (if (fset:contains? (boolean-strings) value)
        (get-option cfg "General configuration" "fast-matrix" :type :boolean)
        (unless (emptyp value)
          (error "Error: ~s is not valid for fast-matrix." value)))))

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
  (let ((val (get-option cfg "Input files" content)))
    (if (emptyp val) nil val)))

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

(defun graphviz-chronology-graph-attribute (cfg attribute)
  "Return the sequence graph attribute from the user's configuration, CFG."
  (get-option cfg "Graphviz chronology graph attributes" attribute))

(defun graphviz-chronology-graph-color (cfg attribute)
  "Return a graphviz color string for the sequence graph ATTRIBUTE from the
  user's configuration, CFG."
  (let ((name (graphviz-chronology-graph-attribute cfg attribute))
        (scheme (graphviz-chronology-graph-attribute cfg "colorscheme")))
    (graphviz-color-string name scheme)))

(defun graphviz-chronology-node-attribute (cfg attribute)
  "Return a function that returns the sequence graph node attribute from the
user's configuration, CFG."
  (let ((attr (quotes-around
               (get-option cfg "Graphviz chronology node attributes" attribute))))
    (constantly attr)))

(defun graphviz-chronology-label-attribute ()
  #'(lambda (x)
      (let ((s (ppcre:split "-" (string-downcase x))))
        (format nil "<&~a;<sub>~a</sub>>" (nth 0 s) (nth 1 s)))))

(defun graphviz-chronology-edge-attribute (cfg attribute)
  "Return a function that returns the sequence graph edge attribute from the
user's configuration, CFG."
  (let ((attr (quotes-around
               (get-option cfg "Graphviz chronology edge attributes" attribute))))
    (constantly attr)))

(defun graphviz-sequence-graph-attribute (cfg attribute)
  "Return the sequence graph attribute from the user's configuration, CFG."
  (get-option cfg "Graphviz sequence graph attributes" attribute))

(defun make-edge-url-map (cfg &optional (verbose t))
  (let ((url-list (read-table (input-file-name cfg "observations")
                              (file-header-p cfg "observations") verbose))
        (map (fset:empty-map)))
    (dolist (edge url-list)
      (setf map (fset:with map (list (ensure-symbol (nth 0 edge))
                                     (ensure-symbol (nth 1 edge))) (nth 2 edge))))
    map))

(defun make-node-url-map (cfg &optional (verbose t))
  (let ((url-list (read-table (input-file-name cfg "contexts")
                              (file-header-p cfg "contexts") verbose))
        (map (fset:empty-map)))
    (dolist (node url-list)
      (setf map (fset:with map (ensure-symbol (nth 0 node)) (nth 5 node))))
    map))

(defun graphviz-sequence-edge-attribute (cfg attribute &optional (verbose t))
  "Return a function that returns the sequence graph edge attribute from the
user's configuration, CFG, or, in the case of an URL, from the URL field of the
observations file."
  (let ((attr (quotes-around (get-option cfg "Graphviz sequence edge attributes"
                                         attribute))))
    (if (eq attribute :url)
        (let ((map (make-edge-url-map cfg verbose)))
          #'(lambda (x)
              (let ((url (fset:@ map x)))
                (if (emptyp url) attr (quotes-around url)))))
        (constantly attr))))

(defun graphviz-sequence-node-attribute (cfg attribute &optional (verbose t))
  "Return a function that returns the sequence graph node attribute from the
user's configuration, CFG, or, in the case of an URL, from the URL field of the
contexts file."
  (let ((attr (quotes-around (get-option cfg "Graphviz sequence node attributes"
                                         attribute))))
    (if (eq attribute :url)
        (let ((map (make-node-url-map cfg verbose)))
          #'(lambda (x)
              (let ((url (fset:@ map x)))
                (if (emptyp url) attr (quotes-around url)))))
        (constantly attr))))

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
         (option (concatenate 'string (string-downcase element) "-"
                              (string-downcase attribute) "-by"))
         (user-val (classification-to-keyword (get-option cfg section option))))
     user-val))

(defun classification-to-keyword (cls)
  "Given a valid classification string, CLS, return the corresponding keyword,
  or else error out."
  (if (or (fset:contains? (classifiers) cls)
            (fset:contains? (fset:set "levels" "adjacent" "reachable" "distance" "units" "periods" "phases" "to" "from") cls)
            (emptyp cls))
      (if (emptyp cls) nil
          (cond
            ((equal cls "levels") :levels)
            ((equal cls "adjacent") :adjacent)
            ((equal cls "reachable") :reachable)
            ((equal cls "distance") :distance)
            ((equal cls "units") :units)
            ((equal cls "periods") :periods)
            ((equal cls "phases") :phases)
            ((equal cls "edge-style-by") :edge-style-by)
            ((equal cls "node-color-by") :node-color-by)
            ((equal cls "node-shape-by") :node-shape-by)
            ((equal cls "node-style-by") :node-style-by)
            ((equal cls "edge-fontsize-by") :edge-fontsize-by)
            ((equal cls "edge-penwidth-by") :edge-penwidth-by)
            ((equal cls "node-penwidth-by") :node-penwidth-by)
            ((equal cls "edge-arrowhead-by") :edge-arrowhead-by)
            ((equal cls "edge-fontcolor-by") :edge-fontcolor-by)
            ((equal cls "node-fillcolor-by") :node-fillcolor-by)
            ((equal cls "node-fontcolor-by") :node-fontcolor-by)
            ((equal cls "node-polygon-skew-by") :node-polygon-skew-by)
            ((equal cls "node-polygon-image-by") :node-polygon-image-by)
            ((equal cls "node-polygon-sides-by") :node-polygon-sides-by)
            ((equal cls "node-polygon-distortion-by") :node-polygon-distortion-by)
            ((equal cls "node-polygon-orientation-by") :node-polygon-orientation-by)
            ((equal cls "edge-color-by") :edge-color-by)
            ((equal cls "from") :from)
            ((equal cls "to") :to)))
      (error "Error: Unable to convert classification to keyword.")))

(defun reachable-limit (cfg)
  "Return the numeric value of `reachable-limit' from the user's configuration,
CFG, or nil if the option is not set."
  (let ((val (get-option cfg "Graph analysis configuration" "reachable-limit")))
    (if (emptyp val) nil
        (get-option cfg "Graph analysis configuration" "reachable-limit" :type :number))))

(defun reachable-from-node (cfg)
  "Returns a symbol from the user's configuration, CFG, or nil if the option is
not set."
  (let ((val (get-option cfg "Graph analysis configuration" "reachable-from")))
    (if (emptyp val) nil (symbolicate val))))

(defun adjacent-from-node (cfg)
  "Returns a symbol from the user's configuration, CFG, or nil if the option is
  not set."
  (let ((val (get-option cfg "Graph analysis configuration" "adjacent-from")))
    (if (emptyp val) nil) (symbolicate val)))

(defun distance-from-node (cfg)
  "Returns a symbol from the user's configuration, CFG, or nil if the option is
  not set."
  (let ((val (get-option cfg "Graph analysis configuration" "distance-from")))
    (if (emptyp val) nil) (symbolicate val)))

(defun chronology-graph-p (cfg)
  "Return a boolean value read from the user's configuration indicating whether
  or not to draw the chronology graph."
  (let ((value (get-option cfg "General configuration" "chronology-graph-draw")))
    (if (fset:contains? (boolean-strings) value)
        (get-option cfg "General configuration" "chronology-graph-draw" :type :boolean)
        (unless (emptyp value)
          (error "Error: ~s is not valid for chronology-graph-draw." value)))))

(defun sequence-classifier (cfg option)
  "Get the sequence classification, OPTION, from the user's configuration, CFG,
or nil if the option is not set."
  (let ((val (classification-to-keyword
              (get-option cfg "Graphviz sequence classification" option))))
    val))

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
    (dolist (row (reverse master))
      (let ((section (nth 0 row))
            (option (nth 1 row))
            (default (nth 7 row)))
        (when (not (member section sections))
          (push section sections)
          (add-section cfg section))
        (set-option cfg section option (if empty "" default))))
    cfg))

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
     &optional (classification :none) (graph-attr :none))
  "Returns the value from the user's configuration, CFG, of the attribute,
  DOT-ATTR, associated with graphviz ELEMENT and the hm graph of GRAPH-TYPE.
  GRAPH-TYPE is one of :sequence, :chronology, :legend, or `'. ELEMENT
  is one of :edge, :node, :graph, or `'."
  (get-option cfg (section-name element dot-attr graph-type classification graph-attr)
              (option-name element dot-attr graph-type classification graph-attr)))

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
  (when (sequence-classifier cfg :node-fillcolor-by)
    (not (and (not (sequence-classifier cfg :node-style-by))
              (equal (get-option cfg "Graphviz sequence node attributes"
                                 "style") "filled")))))

(defun Graphviz-node-polygon-shape-problem? (cfg)
  "Given a configuration, CFG, return non-nil if the user is incorrectly set up
  to classify by manipulations of polygon node shape, nil otherwise"
  (let ((polygon-shaper-p (or (sequence-classifier cfg :node-polygon-distortion-by)
                              (sequence-classifier cfg :node-polygon-image-by)
                              (sequence-classifier cfg :node-polygon-orientation-by)
                              (sequence-classifier cfg :node-polygon-sides-by)
                              (sequence-classifier cfg :node-polygon-skew-by)))
        (node-shape-by-p (sequence-classifier cfg :node-shape-by))
        (polygon-shape-p (equal (get-option cfg "Graphviz sequence node attributes"
                                            "shape") "polygon")))
    (when polygon-shaper-p
      (or node-shape-by-p (not polygon-shape-p)))))

;; Needs work, not exhaustive
(defun origin-node-missing? (cfg)
  "Returns non-nil if an origin node is needed by, but is missing from,
the configuration CFG, nil otherwise."
  (let ((options (options cfg "Graphviz sequence classification"))
        (classifiers (fset:empty-set)))
    (dolist (option options)
      (let ((val (get-option cfg "Graphviz sequence classification" option)))
        (unless (emptyp val) (setf classifiers (fset:with classifiers val)))))
    (when (and (fset:contains? classifiers "reachable") (not (reachable-from-node cfg)))
      (error "Error: Configuration lacks reachable from node."))
    (when (and (fset:contains? classifiers "adjacent") (not (adjacent-from-node cfg)))
      (error "Error: Configuration lacks adjacent from node."))
    (when (and (fset:contains? classifiers "distance") (not (distance-from-node cfg)))
      (error "Error: Configuration lacks distance-from node."))))

(defun chronology-graph-restrictions-ignored? (cfg)
  "Returns non-nil if the configuration CFG ignores chronology graph
restrictions, nil otherwise."
  (and (chronology-graph-p cfg) (assume-correlations-p cfg)))

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
  (when-let ((missing-files (unable-to-find-input-files? cfg)))
    (error "Error: Unable to find ~a" missing-files))
  (unless (input-file-name-p cfg :contexts)
    (error "Error: Contexts file not specified."))
  (unless (input-file-name-p cfg :observations)
    (error "Error: Observations file not specified."))
  (when (chronology-graph-restrictions-ignored? cfg)
    (error "Error: Cannot draw chronology graph when correlations assumed true."))
  (when (origin-node-missing? cfg)
    (error "Error: Origin node must be specified in \"reachable-from\"."))
  (when (Graphviz-node-fill-problem? cfg)
    (error "Error: Graphviz node fills are classified.  Check that \"node-style-by\" is not set and that node \"style\" is \"filled\"."))
  (when (Graphviz-node-polygon-shape-problem? cfg)
    (error "Error: Graphviz node polygons are classified.  Check that \"node-shape-by\" is not set and that node \"shape\" is \"polygon\"."))
  (when (assume-correlations-p cfg) (correlation-problems? cfg)))

(defun correlation-problems? (cfg)
  "Raises an error if correlated nodes are assigned to different unit types,
periods, or phases."
  (let ((correlations (read-table (input-file-name cfg "inferences")
                                  (file-header-p cfg "inferences") nil))
        (contexts (read-table (input-file-name cfg "contexts")
                              (file-header-p cfg "contexts") nil))
        (period-map (fset:empty-map))
        (phase-map (fset:empty-map))
        (unit-map (fset:empty-map)))
    (dolist (context contexts)
      (setf period-map (fset:with period-map (nth 0 context) (nth 3 context)))
      (setf phase-map (fset:with phase-map (nth 0 context) (nth 4 context)))
      (setf unit-map (fset:with unit-map (nth 0 context) (nth 1 context))))
    (dolist (correlation correlations)
      (unless (equal (fset:@ unit-map (nth 0 correlation))
                     (fset:@ unit-map (nth 1 correlation)))
        (error "Error: Contexts ~s and ~s must be assigned to the same unit type."
               (nth 0 correlation) (nth 1 correlation)))
      (unless (equal (fset:@ period-map (nth 0 correlation))
                     (fset:@ period-map (nth 1 correlation)))
        (error "Error: Contexts ~s and ~s must be assigned to the same period."
               (nth 0 correlation) (nth 1 correlation)))
      (unless (equal (fset:@ phase-map (nth 0 correlation))
                     (fset:@ phase-map (nth 1 correlation)))
        (error "Error: Contexts ~s and ~s must be assigned to the same phase."
               (nth 0 correlation) (nth 1 correlation))))))

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

(defun toggle-chronology-graph (cfg &optional (verbose t))
  "Toggle configuration option `chronology-graph-draw.'"
  (let* ((section "General configuration")
         (option "chronology-graph-draw")
         (value (get-option cfg section option)))
    (unless (boolean-string-p value)
      (error "Error: ~s is an invalid setting for ~s.~&" value option))
    (if (get-option cfg section option :type :boolean)
        (progn
          (set-option cfg section option "no")
          (when verbose (format t "Option ~s set to `no'.~&" option)))
        (progn
          (set-option cfg section option "yes")
          (when verbose (format t "Option ~s set to `yes'.~&" option))))))

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
