;;; hm-cfg.lisp

;; Copyright (C) Thomas Dye 2017

;; Licensed under the Gnu Public License Version 3 or later

(in-package #:hm)

;; API

(defun master-table ()
  '(("Output files" "sequence-dot" "sequence" "" "output" "" "" )
    ("Output files" "chronology-dot" "chronology" "" "output" "" "" )
    ("Input files" "contexts" "" "" "input" "" "" )
    ("Input files" "observations" "" "" "input" "" "" )
    ("Input files" "inferences" "" "" "input" "" "" )
    ("Input files" "periods" "" "" "input" "" "" )
    ("Input files" "phases" "" "" "input" "" "" )
    ("Input files" "events" "" "" "input" "" "" )
    ("Input files" "event-order" "" "" "input" "" "" )
    ("Input file headers" "contexts" "" "" "header" "" "" )
    ("Input file headers" "observations" "" "" "header" "" "" )
    ("Input file headers" "inferences" "" "" "header" "" "" )
    ("Input file headers" "periods" "" "" "header" "" "" )
    ("Input file headers" "phases" "" "" "header" "" "" )
    ("Input file headers" "events" "" "" "header" "" "" )
    ("Input file headers" "event-order" "" "" "header" "" "" )
    ("General configuration" "chronology-graph-draw" "chronology" "" "general" "" "off" )
    ("General configuration" "url-include" "" "" "general" "" "off" )
    ("General configuration" "url-default" "" "" "general" "" "http://tsdye.github.io/harris-matrix/" )
    ("General configuration" "legend" "" "" "general" "" "off" )
    ("General configuration" "assume-correlations" "sequence" "" "general" "" "no" )
    ("General configuration" "fast-matrix" "sequence" "" "general" "" "on" )
    ("General configuration" "add-interfaces" "sequence" "" "general" "" "no" )
    ("Graphviz sequence classification" "node-fill-by" "sequence" "node" "classification" "" "" )
    ("Graphviz sequence classification" "node-shape-by" "sequence" "node" "classification" "" "units" )
    ("Graphviz sequence classification" "node-color-by" "sequence" "node" "classification" "" "" )
    ("Graphviz sequence classification" "node-penwidth-by" "sequence" "node" "classification" "" "" )
    ("Graphviz sequence classification" "node-style-by" "sequence" "node" "classification" "" "" )
    ("Graphviz sequence classification" "node-polygon-distortion-by" "sequence" "node" "classification" "" "" )
    ("Graphviz sequence classification" "node-polygon-image-by" "sequence" "node" "classification" "" "" )
    ("Graphviz sequence classification" "node-polygon-orientation-by" "sequence" "node" "classification" "" "" )
    ("Graphviz sequence classification" "node-polygon-sides-by" "sequence" "node" "classification" "" "" )
    ("Graphviz sequence classification" "node-polygon-skew-by" "sequence" "node" "classification" "" "" )
    ("Graphviz sequence classification" "edge-color-by" "sequence" "edge" "classification" "" "" )
    ("Graphviz sequence classification" "edge-fontcolor-by" "sequence" "edge" "classification" "" "" )
    ("Graphviz sequence classification" "edge-penwidth-by" "sequence" "edge" "classification" "" "" )
    ("Graphviz sequence classification" "edge-style-by" "sequence" "edge" "classification" "" "" )
    ("Graphviz sequence classification" "edge-classify-by" "sequence" "edge" "classification" "" "from" )
    ("Graphviz sequence graph attributes" "colorscheme" "sequence" "graph" "graph" "" "x11" )
    ("Graphviz sequence graph attributes" "bgcolor" "sequence" "graph" "graph" "" "white" )
    ("Graphviz sequence graph attributes" "fontname" "sequence" "graph" "graph" "" "Helvetica" )
    ("Graphviz sequence graph attributes" "fontsize" "sequence" "graph" "graph" "" "14.0" )
    ("Graphviz sequence graph attributes" "fontcolor" "sequence" "graph" "graph" "" "black" )
    ("Graphviz sequence graph attributes" "label" "sequence" "graph" "graph" "" "Sequence Diagram" )
    ("Graphviz sequence graph attributes" "labelloc" "sequence" "graph" "graph" "" "t" )
    ("Graphviz sequence graph attributes" "style" "sequence" "graph" "graph" "" "filled" )
    ("Graphviz sequence graph attributes" "size" "sequence" "graph" "graph" "" "6,4!" )
    ("Graphviz sequence graph attributes" "ratio" "sequence" "graph" "graph" "" "auto" )
    ("Graphviz sequence graph attributes" "page" "sequence" "graph" "graph" "" "7,5" )
    ("Graphviz sequence graph attributes" "dpi" "sequence" "graph" "graph" "" "96" )
    ("Graphviz sequence graph attributes" "margin" "sequence" "graph" "graph" "" "0.5,0.5" )
    ("Graphviz sequence graph attributes" "label-break" "sequence" "graph" "graph" "" "" )
    ("Graphviz sequence graph attributes" "fontsize-subscript" "sequence" "graph" "graph" "" "10" )
    ("Graphviz sequence graph attributes" "splines" "sequence" "graph" "graph" "" "ortho" )
    ("Graphviz sequence edge attributes" "colorscheme" "sequence" "edge" "edge" "" "x11" )
    ("Graphviz sequence edge attributes" "style" "sequence" "edge" "edge" "" "solid" )
    ("Graphviz sequence edge attributes" "color" "sequence" "edge" "edge" "" "black" )
    ("Graphviz sequence edge attributes" "fontname" "sequence" "edge" "edge" "" "Helvetica" )
    ("Graphviz sequence edge attributes" "fontsize" "sequence" "edge" "edge" "" "14.0" )
    ("Graphviz sequence edge attributes" "fontcolor" "sequence" "edge" "edge" "" "black" )
    ("Graphviz sequence edge attributes" "arrowhead" "sequence" "edge" "edge" "" "normal" )
    ("Graphviz sequence edge attributes" "penwidth" "sequence" "edge" "edge" "" "1.0" )
    ("Graphviz sequence edge attributes" "penwidth-min" "sequence" "edge" "edge" "" "1.0" )
    ("Graphviz sequence edge attributes" "penwidth-max" "sequence" "edge" "edge" "" "1.0" )
    ("Graphviz sequence node attributes" "shape" "sequence" "node" "node" "" "box" )
    ("Graphviz sequence node attributes" "colorscheme" "sequence" "node" "node" "" "x11" )
    ("Graphviz sequence node attributes" "style" "sequence" "node" "node" "" "filled" )
    ("Graphviz sequence node attributes" "color" "sequence" "node" "node" "" "black" )
    ("Graphviz sequence node attributes" "fontsize" "sequence" "node" "node" "" "14.0" )
    ("Graphviz sequence node attributes" "fontcolor" "sequence" "node" "node" "" "black" )
    ("Graphviz sequence node attributes" "fillcolor" "sequence" "node" "node" "" "white" )
    ("Graphviz sequence node attributes" "fontname" "sequence" "node" "node" "" "Helvetica" )
    ("Graphviz sequence node attributes" "penwidth" "sequence" "node" "node" "" "1.0" )
    ("Graphviz sequence node attributes" "penwidth-min" "sequence" "node" "node" "" "1.0" )
    ("Graphviz sequence node attributes" "penwidth-max" "sequence" "node" "node" "" "1.0" )
    ("Graphviz sequence node attributes" "polygon-distortion" "sequence" "node" "node" "" "0.0" )
    ("Graphviz sequence node attributes" "polygon-image" "sequence" "node" "node" "" "" )
    ("Graphviz sequence node attributes" "polygon-orientation" "sequence" "node" "node" "" "0" )
    ("Graphviz sequence node attributes" "polygon-sides" "sequence" "node" "node" "" "4" )
    ("Graphviz sequence node attributes" "polygon-skew" "sequence" "node" "node" "" "0.0" )
    ("Reachability configuration" "reachable-from" "sequence" "node" "reachable" "" "" )
    ("Reachability configuration" "reachable-limit" "sequence" "node" "reachable" "" "" )
    ("Graphviz sequence reachability node colors" "origin" "sequence" "node" "reachable" "node-color-by" "1" )
    ("Graphviz sequence reachability node colors" "reachable" "sequence" "node" "reachable" "node-color-by" "2" )
    ("Graphviz sequence reachability node colors" "not-reachable" "sequence" "node" "reachable" "node-color-by" "3" )
    ("Graphviz sequence reachability node colors" "colorscheme" "sequence" "node" "reachable" "node-color-by" "accent3" )
    ("Graphviz sequence reachability node fillcolors" "origin" "sequence" "node" "reachable" "node-fill-by" "1" )
    ("Graphviz sequence reachability node fillcolors" "reachable" "sequence" "node" "reachable" "node-fill-by" "2" )
    ("Graphviz sequence reachability node fillcolors" "not-reachable" "sequence" "node" "reachable" "node-fill-by" "3" )
    ("Graphviz sequence reachability node fillcolors" "colorscheme" "sequence" "node" "reachable" "node-fill-by" "accent3" )
    ("Graphviz sequence reachability node shapes" "origin" "sequence" "node" "reachable" "node-shape-by" "polygon" )
    ("Graphviz sequence reachability node shapes" "reachable" "sequence" "node" "reachable" "node-shape-by" "polygon" )
    ("Graphviz sequence reachability node shapes" "not-reachable" "sequence" "node" "reachable" "node-shape-by" "polygon" )
    ("Graphviz sequence reachability node styles" "origin" "sequence" "node" "reachable" "node-style-by" "solid,filled" )
    ("Graphviz sequence reachability node styles" "reachable" "sequence" "node" "reachable" "node-style-by" "dashed,filled" )
    ("Graphviz sequence reachability node styles" "not-reachable" "sequence" "node" "reachable" "node-style-by" "dotted,filled" )
    ("Graphviz sequence reachability node polygon distortion" "origin" "sequence" "node" "reachable" "node-polygon-distortion-by" "0" )
    ("Graphviz sequence reachability node polygon distortion" "reachable" "sequence" "node" "reachable" "node-polygon-distortion-by" "50" )
    ("Graphviz sequence reachability node polygon distortion" "not-reachable" "sequence" "node" "reachable" "node-polygon-distortion-by" "-50" )
    ("Graphviz sequence reachability node polygon image" "origin" "sequence" "node" "reachable" "node-polygon-image-by" "" )
    ("Graphviz sequence reachability node polygon image" "reachable" "sequence" "node" "reachable" "node-polygon-image-by" "" )
    ("Graphviz sequence reachability node polygon image" "not-reachable" "sequence" "node" "reachable" "node-polygon-image-by" "" )
    ("Graphviz sequence reachability node polygon orientation" "origin" "sequence" "node" "reachable" "node-polygon-orientation-by" "0" )
    ("Graphviz sequence reachability node polygon orientation" "reachable" "sequence" "node" "reachable" "node-polygon-orientation-by" "45" )
    ("Graphviz sequence reachability node polygon orientation" "not-reachable" "sequence" "node" "reachable" "node-polygon-orientation-by" "90" )
    ("Graphviz sequence reachability node polygon sides" "origin" "sequence" "node" "reachable" "node-polygon-sides-by" "4" )
    ("Graphviz sequence reachability node polygon sides" "reachable" "sequence" "node" "reachable" "node-polygon-sides-by" "4" )
    ("Graphviz sequence reachability node polygon sides" "not-reachable" "sequence" "node" "reachable" "node-polygon-sides-by" "4" )
    ("Graphviz sequence reachability node polygon skew" "origin" "sequence" "node" "reachable" "node-polygon-skew-by" "0" )
    ("Graphviz sequence reachability node polygon skew" "reachable" "sequence" "node" "reachable" "node-polygon-skew-by" "50" )
    ("Graphviz sequence reachability node polygon skew" "not-reachable" "sequence" "node" "reachable" "node-polygon-skew-by" "-50" )
    ("Graphviz sequence reachability node penwidths" "origin" "sequence" "node" "reachable" "node-penwidth-by" "3" )
    ("Graphviz sequence reachability node penwidths" "reachable" "sequence" "node" "reachable" "node-penwidth-by" "2" )
    ("Graphviz sequence reachability node penwidths" "not-reachable" "sequence" "node" "reachable" "node-penwidth-by" "1" )
    ("Graphviz sequence reachability edge penwidths" "origin" "sequence" "edge" "reachable" "edge-penwidth-by" "3" )
    ("Graphviz sequence reachability edge penwidths" "reachable" "sequence" "edge" "reachable" "edge-penwidth-by" "2" )
    ("Graphviz sequence reachability edge penwidths" "not-reachable" "sequence" "edge" "reachable" "edge-penwidth-by" "1" )
    ("Graphviz sequence reachability edge colors" "origin" "sequence" "edge" "reachable" "edge-color-by" "1" )
    ("Graphviz sequence reachability edge colors" "reachable" "sequence" "edge" "reachable" "edge-color-by" "2" )
    ("Graphviz sequence reachability edge colors" "not-reachable" "sequence" "edge" "reachable" "edge-color-by" "3" )
    ("Graphviz sequence reachability edge colors" "colorscheme" "sequence" "edge" "reachable" "edge-color-by" "accent3" )
    ("Graphviz sequence reachability edge styles" "origin" "sequence" "edge" "reachable" "edge-style-by" "solid" )
    ("Graphviz sequence reachability edge styles" "reachable" "sequence" "edge" "reachable" "edge-style-by" "dashed" )
    ("Graphviz sequence reachability edge styles" "not-reachable" "sequence" "edge" "reachable" "edge-style-by" "dotted" )
    ("Graphviz sequence reachability edge fontcolors" "origin" "sequence" "edge" "reachable" "edge-fontcolor-by" "1" )
    ("Graphviz sequence reachability edge fontcolors" "reachable" "sequence" "edge" "reachable" "edge-fontcolor-by" "2" )
    ("Graphviz sequence reachability edge fontcolors" "not-reachable" "sequence" "edge" "reachable" "edge-fontcolor-by" "3" )
    ("Graphviz sequence reachability edge fontcolors" "colorscheme" "sequence" "edge" "reachable" "edge-fontcolor-by" "accent3" )
    ("Adjacency configuration" "origin" "sequence" "edge" "adjacent" "" "" )
    ("Graphviz sequence adjacent node colors" "origin" "sequence" "node" "adjacent" "node-color-by" "1" )
    ("Graphviz sequence adjacent node colors" "adjacent" "sequence" "node" "adjacent" "node-color-by" "2" )
    ("Graphviz sequence adjacent node colors" "not-adjacent" "sequence" "node" "adjacent" "node-color-by" "3" )
    ("Graphviz sequence adjacent node colors" "colorscheme" "sequence" "node" "adjacent" "node-color-by" "rdylbu3" )
    ("Graphviz sequence adjacent node fillcolors" "origin" "sequence" "node" "adjacent" "node-fill-by" "1" )
    ("Graphviz sequence adjacent node fillcolors" "adjacent" "sequence" "node" "adjacent" "node-fill-by" "2" )
    ("Graphviz sequence adjacent node fillcolors" "not-adjacent" "sequence" "node" "adjacent" "node-fill-by" "3" )
    ("Graphviz sequence adjacent node fillcolors" "colorscheme" "sequence" "node" "adjacent" "node-fill-by" "rdylbu3" )
    ("Graphviz sequence adjacent node shapes" "adjacent" "sequence" "node" "adjacent" "node-shape-by" "polygon" )
    ("Graphviz sequence adjacent node shapes" "not-adjacent" "sequence" "node" "adjacent" "node-shape-by" "polygon" )
    ("Graphviz sequence adjacent node shapes" "origin" "sequence" "node" "adjacent" "node-shape-by" "polygon" )
    ("Graphviz sequence adjacent node styles" "adjacent" "sequence" "node" "adjacent" "node-style-by" "solid,filled" )
    ("Graphviz sequence adjacent node styles" "not-adjacent" "sequence" "node" "adjacent" "node-style-by" "dashed,filled" )
    ("Graphviz sequence adjacent node styles" "origin" "sequence" "node" "adjacent" "node-style-by" "dotted,filled" )
    ("Graphviz sequence adjacent node penwidths" "origin" "sequence" "node" "adjacent" "node-penwidth-by" "3" )
    ("Graphviz sequence adjacent node penwidths" "adjacent" "sequence" "node" "adjacent" "node-penwidth-by" "2" )
    ("Graphviz sequence adjacent node penwidths" "not-adjacent" "sequence" "node" "adjacent" "node-penwidth-by" "1" )
    ("Graphviz sequence adjacent node polygon distortion" "adjacent" "sequence" "node" "adjacent" "node-polygon-distortion-by" "0" )
    ("Graphviz sequence adjacent node polygon distortion" "not-adjacent" "sequence" "node" "adjacent" "node-polygon-distortion-by" "50" )
    ("Graphviz sequence adjacent node polygon distortion" "origin" "sequence" "node" "adjacent" "node-polygon-distortion-by" "-50" )
    ("Graphviz sequence adjacent node polygon image" "adjacent" "sequence" "node" "adjacent" "node-polygon-image-by" "" )
    ("Graphviz sequence adjacent node polygon image" "not-adjacent" "sequence" "node" "adjacent" "node-polygon-image-by" "" )
    ("Graphviz sequence adjacent node polygon image" "origin" "sequence" "node" "adjacent" "node-polygon-image-by" "" )
    ("Graphviz sequence adjacent node polygon orientation" "adjacent" "sequence" "node" "adjacent" "node-polygon-orientation-by" "0" )
    ("Graphviz sequence adjacent node polygon orientation" "not-adjacent" "sequence" "node" "adjacent" "node-polygon-orientation-by" "45" )
    ("Graphviz sequence adjacent node polygon orientation" "origin" "sequence" "node" "adjacent" "node-polygon-orientation-by" "90" )
    ("Graphviz sequence adjacent node polygon sides" "origin" "sequence" "node" "adjacent" "node-polygon-side-by" "4" )
    ("Graphviz sequence adjacent node polygon sides" "adjacent" "sequence" "node" "adjacent" "node-polygon-side-by" "4" )
    ("Graphviz sequence adjacent node polygon sides" "not-adjacent" "sequence" "node" "adjacent" "node-polygon-side-by" "4" )
    ("Graphviz sequence adjacent node polygon skew" "origin" "sequence" "node" "adjacent" "node-polygon-skew-by" "0" )
    ("Graphviz sequence adjacent node polygon skew" "adjacent" "sequence" "node" "adjacent" "node-polygon-skew-by" "50" )
    ("Graphviz sequence adjacent node polygon skew" "not-adjacent" "sequence" "node" "adjacent" "node-polygon-skew-by" "-50" )
    ("Graphviz sequence adjacent edge penwidths" "origin" "sequence" "edge" "adjacent" "edge-penwidth-by" "3" )
    ("Graphviz sequence adjacent edge penwidths" "adjacent" "sequence" "edge" "adjacent" "edge-penwidth-by" "2" )
    ("Graphviz sequence adjacent edge penwidths" "not-adjacent" "sequence" "edge" "adjacent" "edge-penwidth-by" "1" )
    ("Graphviz sequence adjacent edge colors" "origin" "sequence" "edge" "adjacent" "edge-color-by" "1" )
    ("Graphviz sequence adjacent edge colors" "adjacent" "sequence" "edge" "adjacent" "edge-color-by" "2" )
    ("Graphviz sequence adjacent edge colors" "not-adjacent" "sequence" "edge" "adjacent" "edge-color-by" "3" )
    ("Graphviz sequence adjacent edge colors" "colorscheme" "sequence" "edge" "adjacent" "edge-color-by" "rdylbu3" )
    ("Graphviz sequence adjacent edge styles" "origin" "sequence" "edge" "adjacent" "edge-style-by" "solid" )
    ("Graphviz sequence adjacent edge styles" "adjacent" "sequence" "edge" "adjacent" "edge-style-by" "dashed" )
    ("Graphviz sequence adjacent edge styles" "not-adjacent" "sequence" "edge" "adjacent" "edge-style-by" "dotted" )
    ("Graphviz sequence adjacent edge fontcolors" "origin" "sequence" "edge" "adjacent" "edge-fontcolor-by" "1" )
    ("Graphviz sequence adjacent edge fontcolors" "adjacent" "sequence" "edge" "adjacent" "edge-fontcolor-by" "2" )
    ("Graphviz sequence adjacent edge fontcolors" "not-adjacent" "sequence" "edge" "adjacent" "edge-fontcolor-by" "3" )
    ("Graphviz sequence adjacent edge fontcolors" "colorscheme" "sequence" "edge" "adjacent" "edge-fontcolor-by" "rdylbu3" )
    ("Graphviz sequence unit node shape" "deposit" "sequence" "node" "units" "node-shape-by" "box" )
    ("Graphviz sequence unit node shape" "interface" "sequence" "node" "units" "node-shape-by" "trapezium" )
    ("Graphviz sequence unit node color" "deposit" "sequence" "node" "units" "node-color-by" "green" )
    ("Graphviz sequence unit node color" "interface" "sequence" "node" "units" "node-color-by" "red" )
    ("Graphviz sequence unit node color" "colorscheme" "sequence" "node" "units" "node-color-by" "x11" )
    ("Graphviz sequence unit node fillcolor" "deposit" "sequence" "node" "units" "node-fill-by" "green" )
    ("Graphviz sequence unit node fillcolor" "interface" "sequence" "node" "units" "node-fill-by" "red" )
    ("Graphviz sequence unit node fillcolor" "colorscheme" "sequence" "node" "units" "node-fill-by" "x11" )
    ("Graphviz sequence unit node penwidth" "deposit" "sequence" "node" "units" "node-penwidth-by" "2" )
    ("Graphviz sequence unit node penwidth" "interface" "sequence" "node" "units" "node-penwidth-by" "1" )
    ("Graphviz sequence unit node style" "deposit" "sequence" "node" "units" "node-style-by" "solid,filled" )
    ("Graphviz sequence unit node style" "interface" "sequence" "node" "units" "node-style-by" "dashed,filled" )
    ("Graphviz sequence unit node polygon distortion" "deposit" "sequence" "node" "units" "node-polygon-distortion-by" "" )
    ("Graphviz sequence unit node polygon distortion" "interface" "sequence" "node" "units" "node-polygon-distortion-by" "" )
    ("Graphviz sequence unit node polygon image" "deposit" "sequence" "node" "units" "node-polygon-image-by" "" )
    ("Graphviz sequence unit node polygon image" "interface" "sequence" "node" "units" "node-polygon-image-by" "" )
    ("Graphviz sequence unit node polygon orientation" "deposit" "sequence" "node" "units" "node-polygon-orientation-by" "" )
    ("Graphviz sequence unit node polygon orientation" "interface" "sequence" "node" "units" "node-polygon-orientation-by" "" )
    ("Graphviz sequence unit node polygon sides" "deposit" "sequence" "node" "units" "node-polygon-sides-by" "" )
    ("Graphviz sequence unit node polygon sides" "interface" "sequence" "node" "units" "node-polygon-sides-by" "" )
    ("Graphviz sequence unit node polygon skew" "deposit" "sequence" "node" "units" "node-polygon-skew-by" "" )
    ("Graphviz sequence unit node polygon skew" "interface" "sequence" "node" "units" "node-polygon-skew-by" "" )
    ("Graphviz sequence unit edge color" "deposit" "sequence" "edge" "units" "edge-color-by" "green" )
    ("Graphviz sequence unit edge color" "interface" "sequence" "edge" "units" "edge-color-by" "red" )
    ("Graphviz sequence unit edge color" "colorscheme" "sequence" "edge" "units" "edge-color-by" "x11" )
    ("Graphviz sequence unit edge fontcolor" "deposit" "sequence" "edge" "units" "edge-fontcolor-by" "green" )
    ("Graphviz sequence unit edge fontcolor" "interface" "sequence" "edge" "units" "edge-fontcolor-by" "red" )
    ("Graphviz sequence unit edge fontcolor" "colorscheme" "sequence" "edge" "units" "edge-fontcolor-by" "x11" )
    ("Graphviz sequence unit edge penwidth" "deposit" "sequence" "edge" "units" "edge-penwidth-by" "2" )
    ("Graphviz sequence unit edge penwidth" "interface" "sequence" "edge" "units" "edge-penwidth-by" "1" )
    ("Graphviz sequence unit edge style" "deposit" "sequence" "edge" "units" "edge-style-by" "solid" )
    ("Graphviz sequence unit edge style" "interface" "sequence" "edge" "units" "edge-style-by" "dashed" )
    ("Graphviz chronology graph attributes" "colorscheme" "chronology" "graph" "" "" "x11" )
    ("Graphviz chronology graph attributes" "fontname" "chronology" "graph" "" "" "Time-Roman" )
    ("Graphviz chronology graph attributes" "fontsize" "chronology" "graph" "" "" "14.0" )
    ("Graphviz chronology graph attributes" "fontcolor" "chronology" "graph" "" "" "black" )
    ("Graphviz chronology graph attributes" "label" "chronology" "graph" "" "" "Chronology Graph" )
    ("Graphviz chronology graph attributes" "labelloc" "chronology" "graph" "" "" "t" )
    ("Graphviz chronology graph attributes" "style" "chronology" "graph" "" "" "filled" )
    ("Graphviz chronology graph attributes" "size" "chronology" "graph" "" "" "" )
    ("Graphviz chronology graph attributes" "ratio" "chronology" "graph" "" "" "" )
    ("Graphviz chronology graph attributes" "page" "chronology" "graph" "" "" "" )
    ("Graphviz chronology graph attributes" "dpi" "chronology" "graph" "" "" "0.0" )
    ("Graphviz chronology graph attributes" "margin" "chronology" "graph" "" "" "" )
    ("Graphviz chronology graph attributes" "bgcolor" "chronology" "graph" "" "" "white" )
    ("Graphviz chronology graph attributes" "label-break" "chronology" "graph" "" "" "" )
    ("Graphviz chronology graph attributes" "fontsize-subscript" "chronology" "graph" "" "" "" )
    ("Graphviz chronology graph attributes" "splines" "chronology" "graph" "" "" "ortho" )
    ("Graphviz chronology node attributes" "colorscheme" "chronology" "node" "" "" "" )
    ("Graphviz chronology node attributes" "style" "chronology" "node" "" "" "filled" )
    ("Graphviz chronology node attributes" "fontname" "chronology" "node" "" "" "Helvetica" )
    ("Graphviz chronology node attributes" "fontsize" "chronology" "node" "" "" "14.0" )
    ("Graphviz chronology node attributes" "fontcolor" "chronology" "node" "" "" "black" )
    ("Graphviz chronology node attributes" "color" "chronology" "node" "" "" "black" )
    ("Graphviz chronology node attributes" "fillcolor" "chronology" "node" "" "" "white" )
    ("Graphviz chronology edge attributes" "fontname" "chronology" "edge" "" "" "Helvetica" )
    ("Graphviz chronology edge attributes" "colorscheme" "chronology" "edge" "" "" "" )
    ("Graphviz chronology edge attributes" "fontsize" "chronology" "edge" "" "" "14.0" )
    ("Graphviz chronology edge attributes" "fontcolor" "chronology" "edge" "" "" "black" )
    ("Graphviz chronology edge attributes" "arrowhead" "chronology" "edge" "" "" "normal" )
    ("Graphviz chronology edge attributes" "sequential" "chronology" "edge" "" "" "solid" )
    ("Graphviz chronology edge attributes" "abutting" "chronology" "edge" "" "" "dashed" )
    ("Graphviz chronology edge attributes" "separated" "chronology" "edge" "" "" "dotted" )
    ("Graphviz chronology edge attributes" "color" "chronology" "edge" "" "" "black" )
    ("Graphviz chronology node shapes" "phase" "chronology" "node" "" "" "box" )
    ("Graphviz chronology node shapes" "event" "chronology" "node" "" "" "ellipse" )
    ("Graphviz colors" "label-dark" "" "" "" "" "black" )
    ("Graphviz colors" "label-light" "" "" "" "" "white" )
    ("Graphviz legend node attributes" "color" "legend" "node" "" "" "black" )
    ("Graphviz legend node attributes" "fillcolor" "legend" "node" "" "" "white" )
    ("Graphviz legend node attributes" "shape" "legend" "node" "" "" "box" )
    ))


;;API
(defun fast-matrix-p (cfg)
  "Returns the boolean value for fast-matrix in the user's configuration, CFG,
or nil if CFG contains a value not interpreted by py-configparser as a boolean."
  (let ((value (get-option cfg "General configuration" "fast-matrix")))
    (if (member value '("1" "yes" "true" "on" "0" "no" "false" "off")
                :test #'string=)
        (get-option cfg "General configuration" "fast-matrix" :type :boolean)
        nil)))

;;API
(defun assume-correlations-p (cfg)
  "Returns the boolean value for assume-correlations in the user's
  configuration, CFG, or nil if CFG contains a value not interpreted by
  py-configparser as a boolean."
  (let ((value (get-option cfg "General configuration" "assume-correlations")))
    (if (member value '("1" "yes" "true" "on" "0" "no" "false" "off")
                :test #'string=)
        (get-option cfg "General configuration" "assume-correlations"
                    :type :boolean)
        nil)))

;;API
(defun input-file-name (cfg content)
  "Return the file name for CONTENT from the user's configuration, CFG. CONTENT
  is a string, one of `contexts', `observations', `inferences', `periods',
  `phases', `events', or `event-order'."
  (get-option cfg "Input files" content))

;;API
(defun input-file-name-p (cfg content)
  "Return a boolean indicating whether or not the user's configuration, CFG,
  includes a file name for CONTENT. CONTENT is a string, one of `contexts',
  `observations', `inferences', `periods', `phases', `events', or
  `event-order'."
  (not (emptyp (get-option cfg "Input files" content))))

;; API
(defun file-header-p (cfg content)
  "Return the boolean value for CONTENT from the `Input file headers' section of
  the user's configuration, CFG. CONTENT is a string, one of `contexts',
  `observations', `inferences', `periods', `phases', `events', or
  `event-order'."
  (get-option cfg "Input file headers" content :type :boolean))

;; API
(defun output-file-name (cfg content)
  "Return the file name for CONTENT from the user's configuration, CFG. CONTENT
  is a string, one of `sequence-dot' or `chronology-dot'. CONTENT is a string,
  one of `contexts', `observations', `inferences', `periods', `phases',
  `events', or `event-order'."
  (get-option cfg "Output files" content))

(defun missing-interfaces-p (cfg)
  "Return the boolean value of `add-missing-interfaces'."
  (get-option cfg "General configuration" "add-missing-interfaces" :type :boolean))

(defun graphviz-sequence-graph-attribute (cfg attribute)
  "Return the sequence graph attribute from the user's configuration, CFG."
  (get-option cfg "Graphviz sequence graph attributes" attribute))

(defun graphviz-sequence-graph-color (cfg attribute)
  "Return a graphviz color string for the sequence graph ATTRIBUTE from the
  user's configuration, CFG."
  (let ((name (graphviz-sequence-graph-attribute cfg attribute))
        (scheme (graphviz-sequence-graph-attribute cfg "colorscheme")))
    (graphviz-color-string name scheme)))

(defun graphviz-node-classification (cfg attribute)
  "Return the value from the user's configuration, CFG, for the node ATTRIBUTE
classification. ATTRIBUTE is a string, one of `fill', `shape', `color',
`penwidth', `style', `polygon-distortion', `polygon-image',
`polygon-orientation', `polygon-sides', or `polygon-skew'."
  (let ((section "Graphviz sequence classification")
        (option (concatenate 'string "node-" attribute "-by")))
    (get-option cfg section option)))

(defun graphviz-edge-classification (cfg attribute)
  "Return the value from the user's configuration, CFG, for the edge ATTRIBUTE
  classification. ATTRIBUTE is a string, one of `color', `fontcolor',
  `penwidth', `style', or `classify'."
  (let ((section "Graphviz sequence classification")
        (option (concatenate 'string "edge-" attribute "-by")))
    (get-option cfg section option)))

(defun graphviz-classification (cfg element attribute)
  "Return the user-value of the classification if it is set, nil otherwise."
  (let ((user-val (if (string= element "node")
                      (graphviz-node-classification cfg attribute)
                      (graphviz-edge-classification cfg attribute))))
    (if (emptyp user-val) nil user-val)))

(defun reachable-limit (cfg)
  "Return the numeric value of `reachable-limit' from the user's configuration,
CFG."
  (get-option cfg "Reachability configuration" "reachable-limit" :type :number))

(defun reachable-from-node (cfg)
  "Returns a symbol from the user's configuration, CFG."
  (symbolicate (get-option cfg "General configuration" "reachable-from")))

(defun chronology-graph-p (cfg)
  "Return a boolean value read from the user's configuration indicating whether
  or not to draw the chronology graph."
  (get-option cfg "Chronology graph" "draw" :type :boolean))

(defun include-url-p (cfg)
  "Return a boolean value read from the user's configuration, CFG, indicating whether or not to include an URL in the output."
  (get-option cfg "General configuration" "url-include" :type :boolean))

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
            (default (nth 6 row)))
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

;; API
(defun convert-csv-config-to-ini (csv-file)
  "Augments the default configuration with the information in the csv
file, CSV-FILE, and returns a possibly modified configuration.  This
function can be used to convert the old style csv initialization files
to the new ini style."
  (let ((cfg (make-default-or-empty-configuration (master-table)))
        (ht (make-hash-table :test 'equal)))
    (cl-csv:read-csv csv-file
                     :map-fn #'(lambda (row)
                                 (setf (gethash (nth 0 row) ht) (nth 1 row))))

;    (add-section cfg "Output files")
    (set-option cfg "Output files" "sequence-dot"
                (gethash "*output-file-sequence*" ht))
    (set-option cfg "Output files" "chronology-dot"
                (gethash "*output-file-chronology*" ht))

;    (add-section cfg "Input files")
    (set-option cfg "Input files" "contexts"
                (gethash "*context-table-name*" ht))
    (set-option cfg "Input files" "observations"
                (gethash "*observation-table-name*" ht))
    (set-option cfg "Input files" "inferences"
                (gethash "*inference-table-name*" ht))
    (set-option cfg "Input files" "periods"
                (gethash "*period-table-name*" ht))
    (set-option cfg "Input files" "phases"
                (gethash "*phase-table-name*" ht))
    (set-option cfg "Input files" "events"
                (gethash "*radiocarbon-table-name*" ht))
    (set-option cfg "Input files" "event-order"
                (gethash "*date-order-table-name*" ht))

;    (add-section cfg "Input file headers")
    (set-option cfg "Input file headers" "contexts"
                (if (equal (gethash "*context-table-header*" ht) "t") "yes" "no"))
    (set-option cfg "Input file headers" "observations"
                (if (equal (gethash "*observation-table-header*" ht) "t")
                    "yes" "no"))
    (set-option cfg "Input file headers" "inferences"
                (if (equal (gethash "*inference-table-header*" ht) "t") "yes" "no"))
    (set-option cfg "Input file headers" "periods"
                (if (equal (gethash "*period-table-header*" ht) "t") "yes" "no"))
    (set-option cfg "Input file headers" "phases"
                (if (equal (gethash "*phase-table-header*" ht) "t") "yes" "no"))
    (set-option cfg "Input file headers" "events"
                (if (equal (gethash "*radiocarbon-table-header*" ht) "t")
                    "yes" "no"))
    (set-option cfg "Input file headers" "event-order"
                (if (equal (gethash "*date-order-table-header*" ht) "t")
                    "yes" "no"))

;    (add-section cfg "Chronology graph")
    (set-option cfg "Chronology graph" "draw"
                (if (equal (gethash "*create-chronology-graph*" ht) "t")
                    "on" "off"))

;    (add-section cfg "General configuration")
    (set-option cfg "General configuration" "reachable-from"
                (gethash "*reachable-from*" ht))
    (set-option cfg "General configuration" "reachable-limit"
                (gethash "*reachable-limit*" ht))
    (set-option cfg "General configuration" "url-include"
                (if (equal (gethash "*ulr-include*" ht) "nil") "off" "on"))
    (set-option cfg "General configuration" "url-default"
                (if (equal (gethash "*url-default*" ht) "nil") ""
                    (gethash "*url-default*" ht)))
    (set-option cfg "General configuration" "legend"
                (if (equal (gethash "*legend*" ht) "t") "on" "off"))
    (set-option cfg "General configuration" "assume-correlations"
                (if (equal (gethash "*assume-correlations-true*" ht) "t")
                    "yes" "no"))
    (set-option cfg "General configuration" "fast-matrix"
                (if (equal (gethash "*use-fast-matrix*" ht) "t") "on" "off"))

;    (add-section cfg "Graphviz sequence graph attributes")
    (set-option cfg "Graphviz sequence graph attributes" "colorscheme"
                (if (equal (gethash "*color-scheme-sequence*" ht) "nil") ""
                    (gethash "*color-scheme-sequence*" ht)))
    (set-option cfg "Graphviz sequence graph attributes" "bgcolor"
                (gethash "*color-fill-graph-sequence*" ht))
    (set-option cfg "Graphviz sequence graph attributes" "fontname"
                (gethash "*font-name-graph-sequence*" ht))
    (set-option cfg "Graphviz sequence graph attributes" "fontsize"
                (gethash "*font-size-graph-sequence*" ht))
    (set-option cfg "Graphviz sequence graph attributes" "fontcolor"
                (gethash "*font-color-graph-sequence*" ht))
    (set-option cfg "Graphviz sequence graph attributes" "label"
                (gethash "*graph-title-sequence*" ht))
    (set-option cfg "Graphviz sequence graph attributes" "labelloc"
                (gethash "*graph-labelloc-sequence*" ht))
    (set-option cfg "Graphviz sequence graph attributes" "style"
                (if (equal (gethash "*graph-style-sequence*" ht) "nil") ""
                    (gethash  "*graph-style-sequence*" ht)))
    (set-option cfg "Graphviz sequence graph attributes" "size"
                (if (equal (gethash "*graph-size-sequence*" ht) "nil") ""
                    (gethash  "*graph-size-sequence*" ht)))
    (set-option cfg "Graphviz sequence graph attributes" "ratio"
                (if (equal (gethash "*graph-ratio-sequence*" ht) "nil") ""
                    (gethash  "*graph-ratio-sequence*" ht)))
    (set-option cfg "Graphviz sequence graph attributes" "page"
                (if (equal (gethash "*graph-page-sequence*" ht) "nil") ""
                    (gethash  "*graph-page-sequence*" ht)))
    (set-option cfg "Graphviz sequence graph attributes" "dpi"
                (if (equal (gethash "*graph-dpi-sequence*" ht) "nil") ""
                    (gethash  "*graph-dpi-sequence*" ht)))
    (set-option cfg "Graphviz sequence graph attributes" "margin"
                (if (equal (gethash "*graph-margin-sequence*" ht) "nil") ""
                    (gethash  "*graph-margin-sequence*" ht)))
    ;; color-space???
    ;; (set-option cfg "Graphviz sequence graph attributes" "color-space"
    ;;             (gethash "*color-space*" ht))
    (set-option cfg "Graphviz sequence graph attributes" "label-break"
                (if (equal (gethash "*font-color-label-break*" ht) "nil") ""
                    (gethash "*font-color-label-break*" ht)))
    (set-option
     cfg "Graphviz sequence graph attributes" "fontsize-subscript"
     (if (or (not (gethash "*font-size-subscript*" ht))
             (equal (gethash "*font-size-subscript*" ht) "nil")) ""
             (gethash "*font-size-subscript*" ht)))
    (set-option cfg "Graphviz sequence graph attributes" "splines"
                (gethash "*graph-splines-sequence*" ht))

;    (add-section cfg "Graphviz sequence edge attributes")
    (set-option cfg "Graphviz sequence edge attributes" "style"
                (gethash "*style-edge-sequence*" ht))
    (set-option cfg "Graphviz sequence edge attributes" "color"
                (gethash "*color-edge-sequence*" ht))
    (set-option cfg "Graphviz sequence edge attributes" "fontname"
                (gethash "*font-name-edge-sequence*" ht))
    (set-option cfg "Graphviz sequence edge attributes" "fontsize"
                (gethash "*font-size-edge-sequence*" ht))
    (set-option cfg "Graphviz sequence edge attributes" "fontcolor"
                (gethash "*font-color-edge-sequence*" ht))
    (set-option cfg "Graphviz sequence edge attributes" "arrowhead"
                (gethash "*edge-arrowhead-sequence*" ht))
    (set-option cfg "Graphviz sequence edge attributes" "penwidth" "1.0")

;    (add-section cfg "Graphviz sequence node attributes")
    (set-option cfg "Graphviz sequence classification" "node-shape-by"
                (if (equal (gethash "*symbolize-unit-type*" ht) "t")
                    "units" ""))
    (set-option cfg "Graphviz sequence classification" "node-fill-by"
                (gethash "*node-fill-by*" ht))
    (set-option cfg "Graphviz sequence classification" "node-shape-by"
                (gethash "*node-shape-by*" ht))
    (set-option cfg "Graphviz sequence classification" "node-color-by"
                (gethash "*node-color-by*" ht))
    (set-option cfg "Graphviz sequence node attributes" "style"
                (gethash "*style-node-sequence*" ht))
    (set-option cfg "Graphviz sequence node attributes" "color"
                (gethash "*color-node-sequence*" ht))
    (set-option cfg "Graphviz sequence node attributes" "fontsize"
                (gethash "*font-size-node-sequence*" ht))
    (set-option cfg "Graphviz sequence node attributes" "fontcolor"
                (gethash "*font-color-node-sequence*" ht))
    (set-option cfg "Graphviz sequence node attributes" "fillcolor"
                (gethash "*color-fill-node-sequence*" ht))
    (set-option cfg "Graphviz sequence node attributes" "fontname"
                (gethash "*font-name-node-sequence*" ht))
    (set-option cfg "Graphviz sequence node attributes" "penwidth" "1.0")

;    (add-section cfg "Graphviz sequence reachability colors")
    (set-option cfg "Graphviz sequence reachability node colors"
                "reachable"
                (gethash "*color-reachable*" ht))
    (set-option cfg "Graphviz sequence reachability node colors"
                "not-reachable"
                (gethash "*color-not-reachable*" ht))
    (set-option cfg "Graphviz sequence reachability node colors"
                "origin"
                (gethash "*color-origin*" ht))
    (set-option cfg "Graphviz sequence reachability node colors"
                "adjacent"
                (gethash "*color-adjacent*" ht))

;    (add-section cfg "Graphviz sequence reachability shapes")
    (set-option cfg "Graphviz sequence reachability node shapes"
                "reachable"
                (if (equal (gethash "*shape-reachable*" ht) "nil") ""
                    (gethash "*shape-reachable*" ht)))
    (set-option cfg "Graphviz sequence reachability node shapes"
                "not-reachable"
                (if (equal (gethash "*shape-not-reachable*" ht) "nil") ""
                    (gethash "*shape-not-reachable*" ht)))
    (set-option cfg "Graphviz sequence reachability node shapes"
                "origin"
                (if (equal (gethash "*shape-origin*" ht) "nil") ""
                    (gethash "*shape-origin*" ht)))
    (set-option cfg "Graphviz sequence reachability node shapes"
                "adjacent"
                (if (equal (gethash "*shape-adjacent*" ht) "nil") ""
                    (gethash "*shape-adjacent*" ht)))

;; Graphviz sequence unit attributes
    (set-option cfg "Graphviz sequence unit attributes" "deposit-node-shape"
                (gethash "*shape-node-deposit*" ht))
    (set-option cfg "Graphviz sequence unit attributes"
                "deposit-node-color" "black")
    (set-option cfg "Graphviz sequence unit attributes" "deposit-node-fill"
                (gethash "*color-fill-node-deposit*" ht))
    (set-option cfg "Graphviz sequence unit attributes" "interface-node-shape"
                (gethash "*shape-node-interface*" ht))
    (set-option cfg "Graphviz sequence unit attributes"
                "interface-node-color" "black")
    (set-option cfg "Graphviz sequence unit attributes" "interface-node-fill"
                (gethash "*color-fill-node-interface*" ht))

;    (add-section cfg "Graphviz chronology graph attributes")
    (set-option cfg "Graphviz chronology graph attributes" "colorscheme"
                (if (equal (gethash "*color-scheme-chronology*" ht) "nil") ""
                    (gethash "*color-scheme-chronology*" ht)))
    (set-option cfg "Graphviz chronology graph attributes" "fontname"
                (if (equal (gethash "*font-name-graph-chronology*" ht) "nil") ""
                    (gethash "*font-name-graph-chronology*" ht)))
    (set-option cfg "Graphviz chronology graph attributes" "fontsize"
                (if (equal (gethash "*font-size-graph-chronology*" ht) "nil") ""
                    (gethash "*font-size-graph-chronology*" ht)))
    (set-option cfg "Graphviz chronology graph attributes" "fontcolor"
                (if (equal (gethash "*font-color-graph-chronology*" ht) "nil") ""
                    (gethash "*font-color-graph-chronology*" ht)))
    (set-option
     cfg "Graphviz chronology graph attributes" "label"
     (gethash "*graph-title-chronology*" ht))
    (set-option cfg "Graphviz chronology graph attributes" "labelloc"
                (gethash "*graph-labelloc-chronology*" ht))
    (set-option cfg "Graphviz chronology graph attributes" "style"
                (if (equal (gethash "*graph-style-chronology*" ht) "nil") ""
                    (gethash "*graph-style-chronology*" ht)))
    (set-option cfg "Graphviz chronology graph attributes" "size"
                (if (equal (gethash "*graph-size-chronology*" ht) "nil") ""
                    (gethash "*graph-size-chronology*" ht)))
    (set-option cfg "Graphviz chronology graph attributes" "ratio"
                (if (equal (gethash "*graph-ratio-chronology*" ht) "nil") ""
                    (gethash "*graph-ratio-chronology*" ht)))
    (set-option cfg "Graphviz chronology graph attributes" "page"
                (if (equal (gethash "*graph-page-chronology*" ht) "nil") ""
                    (gethash "*graph-page-chronology*" ht)))
    (set-option cfg "Graphviz chronology graph attributes" "dpi"
                (if (equal (gethash "*graph-dpi-chronology*" ht) "nil") ""
                    (gethash "*graph-dpi-chronology*" ht)))
    (set-option cfg "Graphviz chronology graph attributes" "margin"
                (if (equal (gethash "*graph-margin-chronology*" ht) "nil") ""
                    (gethash "*graph-margin-chronology*" ht)))
    (set-option cfg "Graphviz chronology graph attributes" "bgcolor"
                (if (equal (gethash "*color-fill-graph-chronology*" ht) "nil") ""
                    (gethash "*color-fill-graph-chronology*" ht)))
    (set-option cfg "Graphviz chronology graph attributes" "label-break"
                (if (equal (gethash "*font-color-label-break*" ht) "nil") ""
                    (gethash "*font-color-label-break*" ht)))
    (set-option
     cfg "Graphviz chronology graph attributes" "fontsize-subscript"
     (if (or (not (gethash "*font-size-subscript*" ht))
             (equal (gethash "*font-size-subscript*" ht) "nil")) ""
             (gethash "*font-size-subscript*" ht)))
    (set-option cfg "Graphviz chronology graph attributes" "splines"
                (gethash "*graph-splines-chronology*" ht))

;    (add-section cfg "Graphviz chronology node attributes")
    (set-option cfg "Graphviz chronology node attributes" "style"
                (gethash "*style-node-chronology*" ht))
    (set-option cfg "Graphviz chronology node attributes" "fontname"
                (gethash "*font-name-node-chronology*" ht))
    (set-option cfg "Graphviz chronology node attributes" "fontsize"
                (gethash "*font-size-node-chronology*" ht))
    (set-option cfg "Graphviz chronology node attributes" "fontcolor"
                (gethash "*font-color-node-chronology*" ht))
    (set-option cfg "Graphviz chronology node attributes" "color"
                (gethash "*color-node-chronology*" ht))
    (set-option cfg "Graphviz chronology node attributes" "fillcolor"
                (gethash "*color-fill-node-chronology*" ht))

;    (add-section cfg "Graphviz chronology edge attributes")
    (set-option cfg "Graphviz chronology edge attributes" "fontname"
                (gethash "*font-name-edge-chronology*" ht))
    (set-option cfg "Graphviz chronology edge attributes" "fontsize"
                (gethash "*font-size-edge-chronology*" ht))
    (set-option cfg "Graphviz chronology edge attributes" "fontcolor"
                (gethash "*font-color-edge-chronology*" ht))
    (set-option cfg "Graphviz chronology edge attributes" "arrowhead"
                (gethash "*edge-arrowhead-chronology*" ht))
    (set-option cfg "Graphviz chronology edge attributes" "sequential"
                (gethash "*edge-date-chronology*" ht))
    (set-option cfg "Graphviz chronology edge attributes" "abutting"
                (gethash "*edge-abutting-chronology*" ht))
    (set-option cfg "Graphviz chronology edge attributes" "separated"
                (gethash "*edge-separated-chronology*" ht))
    (set-option cfg "Graphviz chronology edge attributes" "color"
                (gethash "*color-edge-chronology*" ht))

;    (add-section cfg "Graphviz chronology node shapes")
    (set-option cfg "Graphviz chronology node shapes" "phase"
                (gethash "*shape-phase*" ht))
    (set-option cfg "Graphviz chronology node shapes" "event"
                (gethash "*shape-date*" ht))

;    (add-section cfg "Graphviz colors")
    (set-option cfg "Graphviz colors" "color-label-dark"
                (gethash "*color-label-dark*" ht))
    (set-option cfg "Graphviz colors" "color-label-light"
                (gethash "*color-label-light*" ht))

;    (add-section cfg "Graphviz legend node attributes")
    (set-option cfg "Graphviz legend node attributes" "color"
                (gethash "*color-node-legend*" ht))
    (set-option cfg "Graphviz legend node attributes" "fillcolor"
                (gethash "*color-fill-node-legend*" ht))
    (set-option cfg "Graphviz legend node attributes" "shape"
                (gethash "*shape-node-legend*" ht))
    cfg))

(defun Graphviz-node-fill-problem? (cfg)
  "If the user wants nodes filled by some graph function, then
  Graphviz requires that node-style-by be empty and the node style
  attribute be `filled'.  Returns nil if all is well, non-nil
  otherwise."
  (not
   (and (not (emptyp (get-option cfg "Graphviz sequence classification"
                                 "node-fill-by")))
        (emptyp (get-option cfg "Graphviz sequence classification"
                            "node-style-by"))
        (string= (get-option cfg "Graphviz sequence node attributes"
                             "style") "filled"))))

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
    (and (emptyp (get-option cfg "General configuration" "reachable-from"))
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
  (and (get-option cfg "Chronology graph" "draw" :type :boolean)
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
      (let ((file-name (get-option cfg "Input files" option)))
        (when file-name (unless (probe-file file-name) (push file-name missing)))))
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
configuration, CFG."
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

(defun read-configuration-from-files (verbose &rest file-names)
  "Reads the initialization files FILE-NAMES and returns a configuration. Errors
out if one or more initialization files were not read. Prints a status message."
  (let ((config (make-default-or-empty-configuration (master-table))))
    (dolist (file file-names)
      (when (null (probe-file file))
        (error "Error: unable to read file: ~s.~&" file)))
    (when verbose
      (format t "Read ~r initialization file~:p: ~{~a~^, ~}.~&"
              (length file-names) file-names))
    (dolist (file file-names)
      (read-files config (list file)))
    config))

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
