;;; hm-elements.lisp

;; Copyright (C) Thomas Dye 2017

;; Licensed under the Gnu Public License Version 3 or later

(in-package #:hm)

(defun graphviz-edge-style-map ()
  "Return an fset map where the keys are integers and the values are graphviz
dot edge style strings."
  (let ((map (-> (fset:empty-map)
                 (fset:with 0 "solid")
                 (fset:with 1 "dashed")
                 (fset:with 2 "dotted")
                 (fset:with 3 "bold"))))
    map))

(defun graphviz-edge-style ()
  "Return a closure that expects an integer argument and returns a valid Graphviz
edge style string."
  (let ((map (graphviz-edge-style-map)))
    #'(lambda (index)
        (fset:lookup map (mod index (fset:size map))))))

(defun graphviz-node-style-map ()
  "Return an fset map where the keys are integers and the values are graphviz
dot node style strings."
  (let ((map (-> (fset:empty-map)
                 (fset:with 0 "solid")
                 (fset:with 1 "dashed")
                 (fset:with 2 "dotted")
                 (fset:with 3 "bold")
                 (fset:with 4 "rounded")
                 (fset:with 5 "diagonals")
                 (fset:with 6 "filled")
                 (fset:with 7 "striped")
                 (fset:with 8 "wedged"))))
    map))

(defun graphviz-node-style ()
  "Return a closure that expects an integer argument and returns a valid
Graphviz dot node style."
  (let ((map (graphviz-node-style-map)))
    #'(lambda (index)
        (fset:lookup map (mod index (fset:size map))))))

(defun graphviz-node-shape-map ()
  "Returns an fset map where the keys are integers and the values are graphviz
dot node shape strings."
  (let ((map (-> (fset:empty-map)
                 (fset:with 0 "box")
                 (fset:with 1 "trapezium")
                 (fset:with 2 "ellipse")
                 (fset:with 3 "egg")
                 (fset:with 4 "triangle")
                 (fset:with 5 "diamond")
                 (fset:with 6 "oval")
                 (fset:with 7 "circle")
                 (fset:with 8 "house")
                 (fset:with 9 "pentagon")
                 (fset:with 10 "parallelogram")
                 (fset:with 11 "square")
                 (fset:with 12 "star")
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
                 (fset:with 25 "lpromoter")
                 (fset:with 26 "larrow")
                 (fset:with 27 "underline")
                 (fset:with 28 "note")
                 (fset:with 29 "tab")
                 (fset:with 30 "folder")
                 (fset:with 31 "box3d")
                 (fset:with 32 "component")
                 (fset:with 33 "cds")
                 (fset:with 34 "signature")
                 (fset:with 35 "rpromoter")
                 (fset:with 36 "rarrow"))))
    map))

(defun graphviz-node-shape ()
  "Return a closure that expects an integer argument and returns a valid
graphviz dot node shape string."
  (let ((map (graphviz-node-shape-map)))
    #'(lambda (index)
        (fset:@ map (mod index (fset:size map))))))

(defun graphviz-arrow-shape-map ()
  "Returns an fset map where the keys are integers and the values are graphviz
dot arrow type strings."
  (let ((map (-> (fset:empty-map)
                 (fset:with 0 "none")
                 (fset:with 1 "box")
                 (fset:with 2 "lbox")
                 (fset:with 3 "rbox")
                 (fset:with 4 "obox")
                 (fset:with 5 "olbox")
                 (fset:with 6 "orbox")
                 (fset:with 7 "crow")
                 (fset:with 8 "lcrow")
                 (fset:with 9 "rcrow")
                 (fset:with 10 "diamond")
                 (fset:with 11 "ldiamond")
                 (fset:with 12 "rdiamond")
                 (fset:with 13 "odiamond")
                 (fset:with 14 "oldiamond")
                 (fset:with 15 "ordiamond")
                 (fset:with 16 "dot")
                 (fset:with 17 "odot")
                 (fset:with 18 "inv")
                 (fset:with 19 "linv")
                 (fset:with 20 "rinv")
                 (fset:with 21 "oinv")
                 (fset:with 22 "olinv")
                 (fset:with 23 "orinv")
                 (fset:with 24 "normal")
                 (fset:with 25 "lnormal")
                 (fset:with 26 "rnormal")
                 (fset:with 27 "onormal")
                 (fset:with 28 "olnormal")
                 (fset:with 29 "ornormal")
                 (fset:with 30 "tee")
                 (fset:with 31 "ltee")
                 (fset:with 32 "rtee")
                 (fset:with 33 "vee")
                 (fset:with 34 "lvee")
                 (fset:with 35 "rvee")
                 (fset:with 36 "curve")
                 (fset:with 37 "lcurve")
                 (fset:with 38 "rcurve")
                 (fset:with 39 "icurve")
                 (fset:with 40 "licurve")
                 (fset:with 41 "ricurve") )))
    map))

(defun graphviz-arrow-shape ()
  "Return a closure that expects an integer argument and returns a valid
graphviz dot arrow type string."
  (let ((map (graphviz-arrow-shape-map)))
    #'(lambda (index)
        (fset:@ map (mod index (fset:size map))))))

(defun graphviz-polygon-sides-map (cfg)
  (let ((max (polygon-sides-max cfg))
        (min (polygon-sides-min cfg))
        (map (fset:empty-map)))
    (loop for i upto (- max min)
          do (setf map (fset:with map i (write-to-string (+ min i)))))
    map))
