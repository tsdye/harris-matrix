;;;; test/hm.lisp --- tests for hm application

;;; Copyright (C) Thomas S. Dye 2016

;;; Licensed under the Gnu Public License Version 3 or later

;;; Setup code:
(in-package :hm-test)

(defsuite test)
(in-suite test)

(defvar *transitive* nil
  "Variable for use in hm tests.")

(defvar *intransitive* nil
  "Variable for use in hm tests.")

(defvar *cfg* nil
  "Variable for use in hm tests.")

(defvar *hm-test-path* nil
  "Variable for use in hm tests.")

(defvar *hm-test-config-path* nil
  "Variable for use in hm tests.")

(defvar *roskams-h-structure* nil
  "Variable for use in hm tests.")

(defvar *roskams-h-seq* nil
  "Variable for use in hm tests.")

(defixture roskams-h-seq
  (:setup (setf *roskams-h-seq*
                (hm::configure-archaeological-sequence
                 (hm::make-archaeological-sequence)
                 (hm:read-configuration-from-files
                  nil (uiop:merge-pathnames*
                       "test/assets/examples/roskams-h-structure/roskams-h.ini"
                       (asdf:system-source-directory :hm-test)))
                 nil))))

(defixture roskams-h-structure
  (:setup (setf *roskams-h-structure*
                (hm:read-configuration-from-files
                 nil
                 (uiop:merge-pathnames*
                  "test/assets/examples/roskams-h-structure/roskams-h.ini"
                  (asdf:system-source-directory :hm-test)))))
  (:teardown (setf *roskams-h-structure* nil)))

(defixture hm-test-path
    (:setup
     (setf *hm-test-path*
           (uiop:merge-pathnames* "test/" (asdf:system-source-directory :hm-test))))
  (:teardown (setf *hm-test-path* nil)))

(defixture hm-test-config-path
    (:setup
     (setf *hm-test-config-path*
           (uiop:merge-pathnames* "test/assets/configurations/"
                                  (asdf:system-source-directory :hm-test))))
  (:teardown (setf *hm-test-config-path* nil)))

(defixture transitive-graph
  (:setup (setf *transitive* (graph:populate (make-instance 'graph:digraph)
                                             :nodes '(a b c)
                                             :edges '((a b)
                                                      (b c)
                                                      (a c)))
                *intransitive* (graph:populate (make-instance 'graph:digraph)
                                               :nodes '(a b c)
                                               :edges '((a b)
                                                        (b c)))
                *cfg* (hm:default-configuration)))
  (:teardown (setf *transitive* nil
                   *intransitive* nil
                   *cfg* nil)))

(defixture default-config
  (:setup (setf *cfg* (hm:default-configuration)))
  (:teardown (setf *cfg* nil)))

(defixture empty-config
  (:setup (setf *cfg* (hm:empty-configuration)))
  (:teardown (setf *cfg* nil)))


;;; Tests for utility functions, hm.lisp

(deftest quotes-around-test ()
  (let ((goal "\"abc\""))
    (is (string= (hm::quotes-around "abc") goal))
    (is (string= (hm::quotes-around "\"abc\"") goal))))


;;; Elements, hm-elements.lisp tests

(deftest graphviz-edge-style-test ()
  (is (equal (funcall (hm::graphviz-edge-style) 0) "solid"))
  (is (equal (funcall (hm::graphviz-edge-style) 1) "dashed"))
  (is (equal (funcall (hm::graphviz-edge-style) 2) "dotted"))
  (is (equal (funcall (hm::graphviz-edge-style) 3) "bold"))
  (is (equal (funcall (hm::graphviz-edge-style) 4) "solid")))

(deftest graphviz-node-style-test ()
  (is (equal (funcall (hm::graphviz-node-style) 0) "solid"))
  (is (equal (funcall (hm::graphviz-node-style) 1) "dashed"))
  (is (equal (funcall (hm::graphviz-node-style) 2) "dotted"))
  (is (equal (funcall (hm::graphviz-node-style) 3) "bold"))
  (is (equal (funcall (hm::graphviz-node-style) 4) "rounded"))
  (is (equal (funcall (hm::graphviz-node-style) 10) "solid")))

(deftest graphviz-node-shape-test ()
  (is (equal (funcall (hm::graphviz-node-shape) 0) "box"))
  (is (equal (funcall (hm::graphviz-node-shape) 1) "polygon"))
  (is (equal (funcall (hm::graphviz-node-shape) 2) "ellipse"))
  (is (equal (funcall (hm::graphviz-node-shape) 3) "egg"))
  (is (equal (funcall (hm::graphviz-node-shape) 4) "triangle"))
  (is (equal (funcall (hm::graphviz-node-shape) 39) "box")))

(deftest graphviz-arrow-shape-test ()
  (is (equal (funcall (hm::graphviz-arrow-shape) 0) "none"))
  (is (equal (funcall (hm::graphviz-arrow-shape) 1) "box"))
  (is (equal (funcall (hm::graphviz-arrow-shape) 2) "lbox"))
  (is (equal (funcall (hm::graphviz-arrow-shape) 42) "none")))

(deftest correlated-node-test ()
  (with-fixture transitive-graph
    (is (eq (hm::correlated-node (first (graph:nodes *transitive*))
                                 (second (graph:nodes *transitive*))
                                 nil)
            'a=b))
    (is (string= (hm::correlated-node (first (graph:nodes *transitive*))
                                 (second (graph:nodes *transitive*))
                                 t)
            "A=B"))))

;; graph tests
(deftest node-index-test ()
  (with-fixture transitive-graph
    (let ((i (hm::make-node-index *transitive*)))
      (is (eq (first (graph:nodes *transitive*))
              (fset:lookup i 0)))
      (is (eq (second (graph:nodes *transitive*))
              (fset:lookup i 1)))
      (is (eq (third (graph:nodes *transitive*))
              (fset:lookup i 2))))))

(deftest transitive-reduction ()
  (with-fixture transitive-graph
    (is (graph:graph-equal
         (hm::transitive-reduction *transitive*)
         *intransitive*))))

;;; Tests for configurations

(deftest test-penwidths ()
  "Test that edge and node penwidth min and max return the correct strings."
  (with-fixture default-config
    (let ((res "1.0"))
      (is (equal (hm::penwidth-min *cfg* :node) res))
      (is (equal (hm::penwidth-min *cfg* :edge) res))
      (is (equal (hm::penwidth-max *cfg* :node) res))
      (is (equal (hm::penwidth-max *cfg* :edge) res)))))

(deftest test-fontsizes ()
  "Test that edge and node fontsize min and max return correct strings."
  (with-fixture default-config
    (let ((res "14.0"))
      (is (equal (hm::fontsize-min *cfg* :node) res))
      (is (equal (hm::fontsize-min *cfg* :edge) res))
      (is (equal (hm::fontsize-max *cfg* :node) res))
      (is (equal (hm::fontsize-max *cfg* :edge) res)))))

(deftest test-lookup-fast-matrix ()
  "Test setting and getting the FAST-MATRIX option."
  (with-fixture default-config
    (is (hm:fast-matrix-p *cfg*))
    (hm::set-option *cfg* "General configuration" "fast-matrix" "off")
    (is (not (hm:fast-matrix-p *cfg*)))
    (hm::set-option *cfg* "General configuration" "fast-matrix" "foo")
    (with-expected-failures
      (is (not (hm:fast-matrix-p *cfg*))))))

(deftest test-assume-correlations-p ()
  "Test that the default configuration returns nil, that setting the option to `yes' returns non-nil, and that setting the option to `foo' returns an error."
  (with-fixture default-config
    (is (not (hm::assume-correlations-p *cfg*)))
    (hm::set-option *cfg* "General configuration" "assume-correlations" "yes")
    (is (hm::assume-correlations-p *cfg*))
    (hm::set-option *cfg* "General configuration" "assume-correlations" "foo")
    (with-expected-failures
      (is (not (hm::assume-correlations-p *cfg*))))))

(deftest test-project-directory ()
  "Test that PROJECT-DIRECTORY returns nil when called on the default configuration."
  (with-fixture default-config
    (is (not (hm::project-directory *cfg*)))))

(deftest test-input-file-name-p ()
  "Test that INPUT-FILE-NAME-P returns nil for all input files given the default
  configuration."
  (with-fixture default-config
    (is (not (hm:input-file-name-p *cfg* :contexts)))
    (is (not (hm:input-file-name-p *cfg* :observations)))
    (is (not (hm:input-file-name-p *cfg* :inferences)))
    (is (not (hm:input-file-name-p *cfg* :periods)))
    (is (not (hm:input-file-name-p *cfg* :phases)))
    (is (not (hm:input-file-name-p *cfg* :events)))
    (is (not (hm:input-file-name-p *cfg* :event-order)))))

(deftest test-file-header-p ()
  "Test that FILE-HEADER-P returns nil for all input files given the default
  configuration."
  (with-fixture default-config
    (is (not (hm:file-header-p *cfg* :contexts)))
    (is (not (hm:file-header-p *cfg* :observations)))
    (is (not (hm:file-header-p *cfg* :inferences)))
    (is (not (hm:file-header-p *cfg* :periods)))
    (is (not (hm:file-header-p *cfg* :phases)))
    (is (not (hm:file-header-p *cfg* :events)))
    (is (not (hm:file-header-p *cfg* :event-order)))))

(deftest test-missing-interfaces-p ()
  "Test that MISSING-INTERFACES-P returns nil for the default configuration."
  (with-fixture default-config
    (is (not (hm::missing-interfaces-p *cfg*)))))

(deftest test-graphviz-sequence-graph-attribute ()
  "Test that GRAPHVIZ-SEQUENCE-GRAPH-ATTRIBUTE returns correct values from the default configuration."
  (with-fixture default-config
    (is (equal (hm::graphviz-sequence-graph-attribute *cfg* :splines) "ortho"))
    (is (equal (hm::graphviz-sequence-graph-attribute *cfg* :fontsize-subscript) "10"))
    (is (equal (hm::graphviz-sequence-graph-attribute *cfg* :label-break) ""))
    (is (equal (hm::graphviz-sequence-graph-attribute *cfg* :margin) "0.5,0.5"))
    (is (equal (hm::graphviz-sequence-graph-attribute *cfg* :dpi) "96"))
    (is (equal (hm::graphviz-sequence-graph-attribute *cfg* :page) "7,5"))
    (is (equal (hm::graphviz-sequence-graph-attribute *cfg* :ratio) "auto"))
    (is (equal (hm::graphviz-sequence-graph-attribute *cfg* :size) "6,4!"))
    (is (equal (hm::graphviz-sequence-graph-attribute *cfg* :style) "filled"))
    (is (equal (hm::graphviz-sequence-graph-attribute *cfg* :labelloc) "t"))
    (is (equal (hm::graphviz-sequence-graph-attribute *cfg* :label) "Sequence Diagram"))
    (is (equal (hm::graphviz-sequence-graph-attribute *cfg* :fontcolor) "black"))
    (is (equal (hm::graphviz-sequence-graph-attribute *cfg* :fontsize) "14.0"))
    (is (equal (hm::graphviz-sequence-graph-attribute *cfg* :fontname) "Helvetica"))
    (is (equal (hm::graphviz-sequence-graph-attribute *cfg* :bgcolor) "white"))
    (is (equal (hm::graphviz-sequence-graph-attribute *cfg* :colorscheme) "x11"))))

(deftest test-graphviz-sequence-edge-attribute ()
  "Test GRAPHVIZ-SEQUENCE-EDGE-ATTRIBUTE returns correct values from the default configuration."
  (with-fixture default-config
    (is (equal
         (funcall (hm::graphviz-sequence-edge-attribute *cfg* :penwidth-max)) "1.0"))
    (is (equal
         (funcall (hm::graphviz-sequence-edge-attribute *cfg* :penwidth-min)) "1.0"))
    (is (equal
         (funcall (hm::graphviz-sequence-edge-attribute *cfg* :penwidth)) "1.0"))
    (is (equal
         (funcall (hm::graphviz-sequence-edge-attribute *cfg* :arrowhead)) "normal"))
    (is (equal
         (funcall (hm::graphviz-sequence-edge-attribute *cfg* :fontcolor)) "black"))
    (is (equal
         (funcall (hm::graphviz-sequence-edge-attribute *cfg* :fontsize-max)) "14.0"))
    (is (equal
         (funcall (hm::graphviz-sequence-edge-attribute *cfg* :fontsize-min)) "14.0"))
    (is (equal
         (funcall (hm::graphviz-sequence-edge-attribute *cfg* :fontsize)) "14.0"))
    (is (equal
         (funcall (hm::graphviz-sequence-edge-attribute *cfg* :fontname)) "Helvetica"))
    (is (equal
         (funcall (hm::graphviz-sequence-edge-attribute *cfg* :color)) "black"))
    (is (equal
         (funcall (hm::graphviz-sequence-edge-attribute *cfg* :style)) "solid"))
    (is (equal
         (funcall (hm::graphviz-sequence-edge-attribute *cfg* :colorscheme)) "x11"))
    (is (equal
         (funcall
          (hm::graphviz-sequence-edge-attribute *cfg* :edge-classify-by)) "from"))))

(deftest test-graphviz-sequence-node-attribute ()
  "Test GRAPHVIZ-SEQUENCE-NODE-ATTRIBUTE returns correct values from the default configuration."
  (with-fixture default-config
    (is (equal
         (funcall (hm::graphviz-sequence-node-attribute *cfg* :penwidth-max))
         "1.0"))
    (is (equal
         (funcall (hm::graphviz-sequence-node-attribute *cfg* :penwidth-min))
         "1.0"))
    (is (equal
         (funcall (hm::graphviz-sequence-node-attribute *cfg* :penwidth))
         "1.0"))
    (is (equal
         (funcall (hm::graphviz-sequence-node-attribute *cfg* :polygon-skew))
         "0.0"))
    (is (equal
         (funcall (hm::graphviz-sequence-node-attribute *cfg* :polygon-sides))
         "4"))
    (is (equal
         (funcall (hm::graphviz-sequence-node-attribute *cfg* :polygon-orientation))
         "0"))
    (is (equal
         (funcall (hm::graphviz-sequence-node-attribute *cfg* :polygon-image))
         ""))
    (is (equal
         (funcall (hm::graphviz-sequence-node-attribute *cfg* :polygon-distortion))
         "0.0"))
    (is (equal
         (funcall (hm::graphviz-sequence-node-attribute *cfg* :fontcolor))
         "black"))
    (is (equal
         (funcall (hm::graphviz-sequence-node-attribute *cfg* :fontsize-max))
         "14.0"))
    (is (equal
         (funcall (hm::graphviz-sequence-node-attribute *cfg* :fontsize-min))
         "14.0"))
    (is (equal
         (funcall (hm::graphviz-sequence-node-attribute *cfg* :fontsize))
         "14.0"))
    (is (equal
         (funcall (hm::graphviz-sequence-node-attribute *cfg* :fontname))
         "Helvetica"))
    (is (equal
         (funcall (hm::graphviz-sequence-node-attribute *cfg* :color))
         "black"))
    (is (equal
         (funcall (hm::graphviz-sequence-node-attribute *cfg* :style))
         "filled"))
    (is (equal
         (funcall (hm::graphviz-sequence-node-attribute *cfg* :colorscheme))
         "x11"))
    (is (equal
         (funcall
          (hm::graphviz-sequence-node-attribute *cfg* :shape))
         "box"))))

(deftest test-graphviz-sequence-graph-color ()
  (with-fixture default-config
    (is (equal "/x11/black" (hm::graphviz-sequence-graph-color *cfg* :fontcolor)))
    (is (equal "/x11/white" (hm::graphviz-sequence-graph-color *cfg* :bgcolor)))))

(deftest test-graphviz-classification ()
  "Test that classifications are read from a user configuration read from disk."
  (with-fixture roskams-h-seq
    (is (not (hm::graphviz-classification *roskams-h-seq* "node" "polygon-skew")))
    (is (not (hm::graphviz-classification *roskams-h-seq* "node" "polygon-sides")))
    (is (not (hm::graphviz-classification *roskams-h-seq* "node" "polygon-orientation")))
    (is (not (hm::graphviz-classification *roskams-h-seq* "node" "polygon-image")))
    (is (not (hm::graphviz-classification *roskams-h-seq* "node" "polygon-distortion")))
    (is (not (hm::graphviz-classification *roskams-h-seq* "node" "style")))
    (is (not (hm::graphviz-classification *roskams-h-seq* "node" "penwidth")))
    (is (not (hm::graphviz-classification *roskams-h-seq* "node" "color")))
    (is (equal "units" (hm::graphviz-classification *roskams-h-seq* "node" "shape")))
    (is (not (hm::graphviz-classification *roskams-h-seq* "node" "fontcolor")))
    (is (not (hm::graphviz-classification *roskams-h-seq* "node" "fill")))
    (is (not (hm::graphviz-classification *roskams-h-seq* "edge" "style")))
    (is (not (hm::graphviz-classification *roskams-h-seq* "edge" "arrowhead")))
    (is (not (hm::graphviz-classification *roskams-h-seq* "edge" "penwidth")))
    (is (not (hm::graphviz-classification *roskams-h-seq* "edge" "fontsize")))
    (is (not (hm::graphviz-classification *roskams-h-seq* "edge" "fontcolor")))
    (is (not (hm::graphviz-classification *roskams-h-seq* "edge" "color")))))

(deftest test-reachable-limit ()
  "Test that REACHABLE-LIMIT is not set in the default configuration."
  (with-fixture default-config
    (is (not (hm::reachable-limit *cfg*)))))

(deftest test-reachable-from-node ()
  "Test that REACHABLE-FROM-NODE is not set in the default configuration."
  (with-fixture default-config
    (is (not (hm::reachable-from-node *cfg*)))))

(deftest test-chronology-graph-p ()
  "Test that CHRONOLOGY-GRAPH-P returns nil given the default configuration, and
that it errors out when set to an invalid value."
  (with-fixture default-config
    (is (not (hm::chronology-graph-p *cfg*)))
    (set-option *cfg* "General configuration" "chronology-graph-draw" "foo")
    (with-expected-failures
      (is (hm::chronology-graph-p *cfg*)))))

(deftest test-include-url-p ()
  "Test that INCLUDE-URL-P returns nil given the default configuration, and
that it errors out when set to an invalid value."
  (with-fixture default-config
    (is (not (hm::include-url-p *cfg*)))
    (set-option *cfg* "General configuration" "url-include" "foo")
    (with-expected-failures
      (is (hm::include-url-p *cfg*)))))

(deftest test-default-url ()
  "Test that DEFAULT-URL returns the value given in the default configuration."
  (with-fixture default-config
    (is (equal (hm::default-url *cfg*) "http://tsdye.github.io/harris-matrix/"))))

(deftest test-sequence-classifier ()
  "Test that SEQUENCE-CLASSIFIER returns the values given in the default
  configuration."
  (with-fixture default-config
    (is (not (hm::sequence-classifier *cfg* :edge-style-by)))
    (is (not (hm::sequence-classifier *cfg* :edge-arrowhead-by)))
    (is (not (hm::sequence-classifier *cfg* :edge-penwidth-by)))
    (is (not (hm::sequence-classifier *cfg* :edge-fontsize-by)))
    (is (not (hm::sequence-classifier *cfg* :edge-fontcolor-by)))
    (is (not (hm::sequence-classifier *cfg* :edge-color-by)))
    (is (not (hm::sequence-classifier *cfg* :node-polygon-skew-by)))
    (is (not (hm::sequence-classifier *cfg* :node-polygon-sides-by)))
    (is (not (hm::sequence-classifier *cfg* :node-polygon-orientation-by)))
    (is (not (hm::sequence-classifier *cfg* :node-polygon-image-by)))
    (is (not (hm::sequence-classifier *cfg* :node-polygon-distortion-by)))
    (is (not (hm::sequence-classifier *cfg* :node-style-by)))
    (is (not (hm::sequence-classifier *cfg* :node-penwidth-by)))
    (is (not (hm::sequence-classifier *cfg* :node-color-by)))
    (is (equal "units" (hm::sequence-classifier *cfg* :node-shape-by)))
    (is (not (hm::sequence-classifier *cfg* :node-fontcolor-by)))
    (is (not (hm::sequence-classifier *cfg* :node-fill-by)))))

(deftest test-lookup-graphviz-option ()
  "Test that LOOKUP-GRAPHVIZ-OPTION returns values from the default configuration."
  (with-fixture default-config
    (is (equal "filled"
                 (hm::lookup-graphviz-option *cfg* "node" "style" "sequence")))
    (is (equal "solid"
                 (hm::lookup-graphviz-option *cfg* "edge" "style" "sequence")))
    (is (equal "x11"
                 (hm::lookup-graphviz-option *cfg* "edge" "colorscheme" "sequence")))
    (is (equal "x11"
                 (hm::lookup-graphviz-option *cfg* "node" "colorscheme" "sequence")))
    (is (equal "1"
                 (hm::lookup-graphviz-option *cfg* "node" "origin" "sequence"
                                             "node-color-by" "reachable")))
    (is (equal "2"
                 (hm::lookup-graphviz-option *cfg* "node" "reachable" "sequence"
                                             "node-color-by" "reachable")))
    (is (equal "3"
                 (hm::lookup-graphviz-option *cfg* "node" "not-reachable" "sequence"
                                             "node-color-by" "reachable")))))

(deftest test-reset-option ()
  "Test that GET-ALL-CONFIGURATION-OPTIONS works and that RESET-OPTION changes
the configuration."
  (let ((default-config (hm:default-configuration)))
    (with-fixture default-config
      (is (equal (get-all-configuration-options *cfg*)
                 (get-all-configuration-options default-config)))
      (reset-option default-config "General configuration" "legend" "on")
      (is (not (equal (get-all-configuration-options *cfg*)
                      (get-all-configuration-options default-config)))))))

(deftest test-set-input-file ()
  "Test whether SET-INPUT-FILE function correctly sets options in sections Input
files and Input file headers."
  (with-fixture default-config
    (let* ((file-name-string "test/assets/configurations/contexts-eg.csv"))
      (set-input-file *cfg* "contexts" file-name-string t)
      (set-input-file *cfg* "observations" file-name-string nil)
      (is (equal (hm::input-file-name-p *cfg* :contexts) file-name-string))
      (is (equal (hm::input-file-name-p *cfg* :observations) file-name-string))
      (is (hm::get-option *cfg* "Input file headers" "contexts" :type :boolean))
      (is (not (hm::get-option *cfg* "Input file headers" "observations"
                           :type :boolean))))))

(deftest test-set-dot-file ()
  "Test that TEST_SET_DOT_FILE works."
  (with-fixture default-config
    (set-dot-file *cfg* "chronology-dot" "foo.dot" nil)
    (set-dot-file *cfg* "sequence-dot" "bar.dot" nil)
    (is (equal (hm::get-option *cfg* "Output files" "chronology-dot") "foo.dot"))
    (is (equal (hm::get-option *cfg* "Output files" "sequence-dot") "bar.dot"))))

;;; Color functions

(deftest test-graphviz-color-string ()
  (is (equal (hm::graphviz-color-string 1 "reds" 3) "/reds3/2"))
  (is (equal (hm::graphviz-color-string "white" "x11") "/x11/white"))
  (is (equal (hm::graphviz-color-string "white" "x11" 3) "/x11/white"))
  (is (equal (hm::graphviz-color-string "blue" "solarized") "#268bd2"))
  (is (equal (hm::graphviz-color-string "base03" "solarized") "#002b36"))
  (is (equal (hm::graphviz-color-string 0 "cet-inferno" 256) "0.640 1.000 0.365")))

(deftest test-graphviz-hsv-string ()
  (is (equal (hm::graphviz-hsv-string "white") "0.000 0.000 1.000"))
  (is (equal (hm::graphviz-hsv-string "black") "0.000 0.000 0.000"))
  (is (equal (hm::graphviz-hsv-string "red") "0.000 1.000 1.000"))
  (is (equal (hm::graphviz-hsv-string "turquoise") "0.483 0.714 0.878"))
  (is (equal (hm::graphviz-hsv-string "sienna") "0.054 0.719 0.627")))

(deftest test-graphviz-color-from-ramp ()
  (is (equal (hm::graphviz-color-from-ramp 0 "black" "white" 3)
               "0.000 0.000 0.000"))
  (is (equal (hm::graphviz-color-from-ramp 1 "black" "white" 3)
               "0.000 0.000 0.333"))
  (is (equal (hm::graphviz-color-from-ramp 2 "black" "white" 3)
               "0.000 0.000 0.667"))
  (is (equal (hm::graphviz-color-from-ramp 3 "black" "white" 3)
               "0.000 0.000 1.000")))

(deftest test-cet-color ()
  "Test that CET-COLOR works with all the cet palettes."
  (is (equal (hm::cet-color "cet-inferno" 0 256) "0.640 1.000 0.365"))
  (is (equal (hm::cet-color "cet-inferno" 255 256) "0.171 0.687 0.976"))
  (is (equal (hm::cet-color "cet-inferno" 0 1) "0.943 0.780 0.910"))
  (is (equal (hm::cet-color "cet-bgyw" 0 1) "0.310 0.590 0.612"))
  (is (equal (hm::cet-color "cet-kbc" 0 1) "0.615 0.822 0.992"))
  (is (equal (hm::cet-color "cet-blues" 0 1) "0.589 0.332 0.863"))
  (is (equal (hm::cet-color "cet-bmw" 0 1) "0.784 0.890 0.996"))
  (is (equal (hm::cet-color "cet-kgy" 0 1) "0.307 0.920 0.541"))
  (is (equal (hm::cet-color "cet-gray" 0 1) "0.000 0.000 0.467"))
  (is (equal (hm::cet-color "cet-dimgray" 0 1) "0.000 0.000 0.494"))
  (is (equal (hm::cet-color "cet-fire" 0 1) "0.015 1.000 0.929"))
  (is (equal (hm::cet-color "cet-kb" 0 1) "0.623 0.902 0.518"))
  (is (equal (hm::cet-color "cet-kg" 0 1) "0.333 1.000 0.255"))
  (is (equal (hm::cet-color "cet-kr" 0 1) "0.031 1.000 0.463"))
  (is (equal (hm::cet-color "cet-rainbow" 0 1) "0.179 0.834 0.757")))

;; File functions

(deftest read-write-configuration ()
  "Test that configurations are written to file and read back in correctly"
  (with-fixture hm-test-config-path
    (let ((path-name
            (uiop:merge-pathnames* "test-config.ini" *hm-test-config-path*))
          (config-from-file nil))
      (with-fixture default-config
        (write-configuration *cfg* path-name)
        (setf config-from-file (read-configuration-from-files nil path-name))
        (is (equal (get-configuration-sections *cfg*)
                   (get-configuration-sections config-from-file)))
        (is (equal (get-all-configuration-options *cfg*)
                   (get-all-configuration-options config-from-file)))))))

(deftest read-write-split-configuration ()
  "Test that configurations written to two files are read back in correctly."
  (with-fixtures (hm-test-config-path default-config)
    (let ((general-path-name
            (uiop:merge-pathnames* "test-general-config.ini"
                                   *hm-test-config-path*))
          (graphviz-path-name
            (uiop:merge-pathnames* "test-graphviz-config.ini"
                                   *hm-test-config-path*))
          (config-from-file nil))
      (write-general-configuration *cfg* general-path-name)
      (write-Graphviz-style-configuration *cfg* graphviz-path-name)
      (setf config-from-file
            (read-configuration-from-files nil
                                           general-path-name graphviz-path-name))
      (is (equal (get-configuration-sections *cfg*)
                 (get-configuration-sections config-from-file)))
      (is (equal (get-all-configuration-options *cfg*)
                 (get-all-configuration-options config-from-file))))))

;; Test for Roskam's h-structure example

(deftest test-roskams-h-structure-config ()
  (with-fixture roskams-h-structure
    (is (not (configuration-errors? *roskams-h-structure*)))))

(deftest test-roskams-h-structure-graph ()
  (with-fixture roskams-h-structure
    (is (typep (hm::make-new-sequence-graph *roskams-h-structure* nil)
               'graph:digraph))))

(deftest test-roskams-h-structure-configure-sequence ()
  (with-fixture roskams-h-structure
    (is (typep (hm:configure-archaeological-sequence
                (hm::make-archaeological-sequence) *roskams-h-structure* nil)
               'hm::archaeological-sequence))))

(deftest test-roskams-h-structure-graph-nodes-edges ()
  (with-fixture roskams-h-structure
    (let ((g (hm::make-new-sequence-graph *roskams-h-structure* nil)))
      (is (equal (graph:nodes g) '(|1| |2| |3| |4| |5|)))
      (is (equal
           (graph:edges g)
           '((|1| |3|) (|1| |4|) (|1| |5|) (|2| |3|) (|2| |5|) (|3| |5|) (|4| |5|)))))))

;; (deftest test-roskams-h-structure-missing-interfaces ()
;;   (with-fixture roskams-h-structure
;;     (hm::set-option *roskams-h-structure* "General configuration" "add-interfaces" "yes")
;;     (let ((seq (hm::configure-archaeological-sequence
;;                 (hm::make-archaeological-sequence) *roskams-h-structure* nil)))
;;       (is (equal (graph:nodes (hm::archaeological-sequence-graph seq))
;;                  '(|1| |2| |3| |4| |5| |5-*surface*| |3-*surface*| |4-*surface*|)))
;;       (is (equal (graph:edges (hm::archaeological-sequence-graph seq))
;;                  '((|1| |3-*surface*|) (|1| |4-*surface*|) (|1| |5-*surface*|)
;;                    (|2| |3-*surface*|) (|2| |5-*surface*|) (|3| |5-*surface*|)
;;                    (|4| |5-*surface*|) (|5-*surface*| |5|) (|3-*surface*| |3|)
;;                    (|4-*surface*| |4|)))))))
