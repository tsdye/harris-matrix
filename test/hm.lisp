;;;; test/hm.lisp --- tests for hm application

;;; Copyright (C) Thomas S. Dye 2016

;;; Licensed under the Gnu Public License Version 3 or later

;;; Setup code:
(in-package :hm-test)

(defsuite test)
(in-suite test)

(defvar *transitive* nil
  "Variable to hold a graph for use in hm tests.")

(defvar *intransitive* nil
  "Variable to hold a graph for use in hm tests.")

(defvar *cfg* nil
  "Variable to hold a configuration for use in hm tests.")

(defvar *path* nil
  "Variable to hold a path for use in hm tests.")

(defvar *sequence* nil
  "Variable to hold an archaeological-sequence for use in hm tests.")

(defixture fig-12-chronology
  (:setup (setf *sequence*
                (hm::configure-archaeological-sequence
                 (hm::make-archaeological-sequence)
                 (hm:read-configuration-from-files
                  nil
                  (uiop:merge-pathnames*
                   "test/assets/examples/harris-fig-12-chronology/fig-12.ini"
                   (asdf:system-source-directory :hm-test)))
                 nil)))
  (:teardown (setf *sequence* nil)))

(defixture bldg-1-5
  (:setup (setf *sequence*
                (hm::configure-archaeological-sequence
                 (hm::make-archaeological-sequence)
                 (hm:read-configuration-from-files
                  nil
                  (uiop:merge-pathnames*
                   "test/assets/examples/bldg-1-5/bldg-1-5.ini"
                   (asdf:system-source-directory :hm-test)))
                 nil)))
  (:teardown (setf *sequence* nil)))

(defixture fig-12-correlations
    (:setup (setf *sequence*
                  (hm::configure-archaeological-sequence
                   (hm::make-archaeological-sequence)
                   (hm:read-configuration-from-files
                    nil
                    (uiop:merge-pathnames*
                     "test/assets/examples/harris-fig-12-correlations/fig-12.ini"
                     (asdf:system-source-directory :hm-test)))
                   nil)))
  (:teardown (setf *sequence* nil)))

(defixture fig-12-polygon-skew
  (:setup (setf *sequence*
                (hm::configure-archaeological-sequence
                 (hm::make-archaeological-sequence)
                 (hm:read-configuration-from-files
                  nil
                  (uiop:merge-pathnames*
                   "test/assets/examples/harris-fig-12-polygon-skew/fig-12.ini"
                   (asdf:system-source-directory :hm-test)))
                 nil)))
  (:teardown (setf *sequence* nil)))

(defixture fig-12-polygon-sides
  (:setup (setf *sequence*
                (hm::configure-archaeological-sequence
                 (hm::make-archaeological-sequence)
                 (hm:read-configuration-from-files
                  nil
                  (uiop:merge-pathnames*
                   "test/assets/examples/harris-fig-12-polygon-sides/fig-12.ini"
                   (asdf:system-source-directory :hm-test)))
                 nil)))
  (:teardown (setf *sequence* nil)))

(defixture fig-12-polygon-orientation
  (:setup (setf *sequence*
                (hm::configure-archaeological-sequence
                 (hm::make-archaeological-sequence)
                 (hm:read-configuration-from-files
                  nil
                  (uiop:merge-pathnames*
                   "test/assets/examples/harris-fig-12-polygon-orientation/fig-12.ini"
                   (asdf:system-source-directory :hm-test)))
                 nil)))
  (:teardown (setf *sequence* nil)))

(defixture fig-12-polygon-distortion
  (:setup (setf *sequence*
                (hm::configure-archaeological-sequence
                 (hm::make-archaeological-sequence)
                 (hm:read-configuration-from-files
                  nil
                  (uiop:merge-pathnames*
                   "test/assets/examples/harris-fig-12-polygon-distortion/fig-12.ini"
                   (asdf:system-source-directory :hm-test)))
                 nil)))
  (:teardown (setf *sequence* nil)))

(defixture fig-12-units
  (:setup (setf *sequence*
                (hm::configure-archaeological-sequence
                 (hm::make-archaeological-sequence)
                 (hm:read-configuration-from-files
                  nil
                  (uiop:merge-pathnames*
                   "test/assets/examples/harris-fig-12-units/fig-12.ini"
                   (asdf:system-source-directory :hm-test)))
                 nil)))
  (:teardown (setf *sequence* nil)))

(defixture fig-12-phases
  (:setup (setf *sequence*
                (hm::configure-archaeological-sequence
                 (hm::make-archaeological-sequence)
                 (hm:read-configuration-from-files
                  nil
                  (uiop:merge-pathnames*
                   "test/assets/examples/harris-fig-12-phases/fig-12.ini"
                   (asdf:system-source-directory :hm-test)))
                 nil)))
  (:teardown (setf *sequence* nil)))

(defixture fig-12-periods
  (:setup (setf *sequence*
                (hm::configure-archaeological-sequence
                 (hm::make-archaeological-sequence)
                 (hm:read-configuration-from-files
                  nil
                  (uiop:merge-pathnames*
                   "test/assets/examples/harris-fig-12-periods/fig-12.ini"
                   (asdf:system-source-directory :hm-test)))
                 nil)))
  (:teardown (setf *sequence* nil)))

(defixture fig-12-levels
  (:setup (setf *sequence*
                (hm::configure-archaeological-sequence
                 (hm::make-archaeological-sequence)
                 (hm:read-configuration-from-files
                  nil
                  (uiop:merge-pathnames*
                   "test/assets/examples/harris-fig-12-levels/fig-12.ini"
                   (asdf:system-source-directory :hm-test)))
                 nil)))
  (:teardown (setf *sequence* nil)))

(defixture fig-12-distance
  (:setup (setf *sequence*
                (hm::configure-archaeological-sequence
                 (hm::make-archaeological-sequence)
                 (hm:read-configuration-from-files
                  nil
                  (uiop:merge-pathnames*
                   "test/assets/examples/harris-fig-12-distance/fig-12.ini"
                   (asdf:system-source-directory :hm-test)))
                 nil)))
  (:teardown (setf *sequence* nil)))

(defixture fig-12-adjacent
  (:setup (setf *sequence*
                (hm::configure-archaeological-sequence
                 (hm::make-archaeological-sequence)
                 (hm:read-configuration-from-files
                  nil
                  (uiop:merge-pathnames*
                   "test/assets/examples/harris-fig-12-adjacent/fig-12.ini"
                   (asdf:system-source-directory :hm-test)))
                 nil)))
  (:teardown (setf *sequence* nil)))

(defixture fig-12-reachable
  (:setup (setf *sequence*
                (hm::configure-archaeological-sequence
                 (hm::make-archaeological-sequence)
                 (hm:read-configuration-from-files
                  nil
                  (uiop:merge-pathnames*
                   "test/assets/examples/harris-fig-12-reachable/fig-12.ini"
                   (asdf:system-source-directory :hm-test)))
                 nil)))
  (:teardown (setf *sequence* nil)))

(defixture fig-12
  (:setup (setf *sequence*
                (hm::configure-archaeological-sequence
                 (hm::make-archaeological-sequence)
                 (hm:read-configuration-from-files
                  nil
                  (uiop:merge-pathnames* "test/assets/examples/harris-fig-12/fig-12.ini"
                                         (asdf:system-source-directory :hm-test)))
                 nil)))
  (:teardown (setf *sequence* nil)))

(defixture roskams-h-class
  (:setup (setf *sequence*
                (hm::configure-archaeological-sequence
                 (hm::make-archaeological-sequence)
                 (hm:read-configuration-from-files
                  nil
                  (uiop:merge-pathnames*
                   "test/assets/examples/roskams-h-classified/roskams-h-classified.ini"
                       (asdf:system-source-directory :hm-test)))
                 nil)))
  (:teardown (setf *sequence* nil)))

(defixture roskams-h-seq
  (:setup (setf *sequence*
                (hm::configure-archaeological-sequence
                 (hm::make-archaeological-sequence)
                 (hm:read-configuration-from-files
                  nil (uiop:merge-pathnames*
                       "test/assets/examples/roskams-h-structure/roskams-h.ini"
                       (asdf:system-source-directory :hm-test)))
                 nil)))
  (:teardown (setf *sequence* nil)))

(defixture roskams-h-structure
  (:setup (setf *cfg*
                (hm:read-configuration-from-files
                 nil
                 (uiop:merge-pathnames*
                  "test/assets/examples/roskams-h-structure/roskams-h.ini"
                  (asdf:system-source-directory :hm-test)))))
  (:teardown (setf *cfg* nil)))

(defixture hm-test-config-path
    (:setup
     (setf *path*
           (uiop:merge-pathnames* "test/assets/configurations/"
                                  (asdf:system-source-directory :hm-test))))
  (:teardown (setf *path* nil)))

(defixture transitive-graph
  (:setup (setf *transitive* (graph:populate (make-instance 'graph:digraph)
                                             :nodes '(a b c d)
                                             :edges '((a b)
                                                      (b c)
                                                      (c d)
                                                      (a d)))
                *intransitive* (graph:populate (make-instance 'graph:digraph)
                                               :nodes '(a b c)
                                               :edges '((a b)
                                                        (b c)
                                                        (c d)))
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
  (is (equal (funcall (hm::graphviz-node-shape) 1) "trapezium"))
  (is (equal (funcall (hm::graphviz-node-shape) 2) "ellipse"))
  (is (equal (funcall (hm::graphviz-node-shape) 3) "egg"))
  (is (equal (funcall (hm::graphviz-node-shape) 4) "triangle"))
  (is (equal (funcall (hm::graphviz-node-shape) 37) "box")))

(deftest graphviz-arrow-shape-test ()
  (is (equal (funcall (hm::graphviz-arrow-shape) 0) "none"))
  (is (equal (funcall (hm::graphviz-arrow-shape) 1) "box"))
  (is (equal (funcall (hm::graphviz-arrow-shape) 2) "lbox"))
  (is (equal (funcall (hm::graphviz-arrow-shape) 42) "none")))

(deftest correlated-node-test ()
  (with-fixture transitive-graph
    (is (eq (hm::correlated-node (first (graph:nodes *transitive*))
                                 (second (graph:nodes *transitive*)) nil)
            'a=b))
    (is (equal (hm::correlated-node (first (graph:nodes *transitive*))
                                    (second (graph:nodes *transitive*)) t)
               "A=B"))))

;; graph tests
(deftest node-index-test ()
  (with-fixture transitive-graph
    (let ((i (hm::map-index-to-node *transitive*)))
      (is (eq (first (graph:nodes *transitive*))
              (fset:lookup i 0)))
      (is (eq (second (graph:nodes *transitive*))
              (fset:lookup i 1)))
      (is (eq (third (graph:nodes *transitive*))
              (fset:lookup i 2))))))

(deftest transitive-reduction ()
  (with-fixture transitive-graph
    (is (graph:graph-equal
         (hm::transitive-reduction *transitive* nil)
         *intransitive*))))

;;; Tests for configurations

(deftest test-penwidths ()
  "Test that edge and node penwidth min and max return the correct strings."
  (with-fixture default-config
    (let ((res 1.0))
      (is (equal (hm::penwidth-min *cfg* :node) res))
      (is (equal (hm::penwidth-min *cfg* :edge) res))
      (is (equal (hm::penwidth-max *cfg* :node) res))
      (is (equal (hm::penwidth-max *cfg* :edge) res)))))

(deftest test-fontsizes ()
  "Test that edge and node fontsize min and max return correct strings."
  (with-fixture default-config
    (let ((res 14.0))
      (is (equal (hm::fontsize-min *cfg* :node) 6.0))
      (is (equal (hm::fontsize-min *cfg* :edge) res))
      (is (equal (hm::fontsize-max *cfg* :node) 22.0))
      (is (equal (hm::fontsize-max *cfg* :edge) res)))))

(deftest test-lookup-fast-matrix ()
  "Test setting and getting the FAST-MATRIX option."
  (with-fixture default-config
    (is (hm:fast-matrix-p *cfg*))
    (hm::set-option *cfg* "General configuration" "fast-matrix" "off")
    (is (not (hm:fast-matrix-p *cfg*)))))

(deftest test-assume-correlations-p ()
  "Test that the default configuration returns nil, that setting the option to `yes' returns non-nil, and that setting the option to `foo' returns an error."
  (with-fixture default-config
    (is (not (hm::assume-correlations-p *cfg*)))
    (hm::set-option *cfg* "General configuration" "assume-correlations" "yes")
    (is (hm::assume-correlations-p *cfg*))))

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
  "Test that GRAPHVIZ-SEQUENCE-GRAPH-ATTRIBUTE returns correct values from the
default configuration."
  (with-fixture default-config
    (is (equal (hm::graphviz-sequence-graph-attribute *cfg* :splines) "ortho"))
    (is (equal (hm::graphviz-sequence-graph-attribute *cfg* :fontsize-subscript) "10"))
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
  "Test GRAPHVIZ-SEQUENCE-EDGE-ATTRIBUTE returns correct values from the default
configuration."
  (with-fixture default-config
    (is (equal
         (funcall (hm::graphviz-sequence-edge-attribute *cfg* :penwidth-max))
         (hm::quotes-around "1.0")))
    (is (equal
         (funcall (hm::graphviz-sequence-edge-attribute *cfg* :penwidth-min))
         (hm::quotes-around "1.0")))
    (is (equal
         (funcall (hm::graphviz-sequence-edge-attribute *cfg* :penwidth))
         (hm::quotes-around "1.0")))
    (is (equal
         (funcall (hm::graphviz-sequence-edge-attribute *cfg* :arrowhead))
         (hm::quotes-around "normal")))
    (is (equal
         (funcall (hm::graphviz-sequence-edge-attribute *cfg* :fontcolor))
         (hm::quotes-around "black")))
    (is (equal
         (funcall (hm::graphviz-sequence-edge-attribute *cfg* :fontsize-max))
         (hm::quotes-around "14.0")))
    (is (equal
         (funcall (hm::graphviz-sequence-edge-attribute *cfg* :fontsize-min))
         (hm::quotes-around "14.0")))
    (is (equal
         (funcall (hm::graphviz-sequence-edge-attribute *cfg* :fontsize))
         (hm::quotes-around "14.0")))
    (is (equal
         (funcall (hm::graphviz-sequence-edge-attribute *cfg* :fontname))
         (hm::quotes-around "Helvetica")))
    (is (equal
         (funcall (hm::graphviz-sequence-edge-attribute *cfg* :color))
         (hm::quotes-around "black")))
    (is (equal
         (funcall (hm::graphviz-sequence-edge-attribute *cfg* :style))
         (hm::quotes-around "solid")))
    (is (equal
         (funcall (hm::graphviz-sequence-edge-attribute *cfg* :colorscheme))
         (hm::quotes-around "x11")))
    (is (equal
         (funcall
          (hm::graphviz-sequence-edge-attribute *cfg* :edge-classify-by))
         (hm::quotes-around "from")))))

(deftest test-graphviz-sequence-node-attribute ()
  "Test GRAPHVIZ-SEQUENCE-NODE-ATTRIBUTE returns correct values from the default
configuration."
  (with-fixture default-config
    (is (equal
         (funcall (hm::graphviz-sequence-node-attribute *cfg* :penwidth-max))
         (hm::quotes-around "1.0")))
    (is (equal
         (funcall (hm::graphviz-sequence-node-attribute *cfg* :penwidth-min))
         (hm::quotes-around "1.0")))
    (is (equal
         (funcall (hm::graphviz-sequence-node-attribute *cfg* :penwidth))
         (hm::quotes-around "1.0")))
    (is (equal
         (funcall (hm::graphviz-sequence-node-attribute *cfg* :polygon-skew))
         (hm::quotes-around "0.0")))
    (is (equal
         (funcall (hm::graphviz-sequence-node-attribute *cfg* :polygon-sides))
         (hm::quotes-around "4")))
    (is (equal
         (funcall (hm::graphviz-sequence-node-attribute *cfg* :polygon-orientation))
         (hm::quotes-around "0.0")))
    (is (equal
         (funcall (hm::graphviz-sequence-node-attribute *cfg* :polygon-image))
         (hm::quotes-around "")))
    (is (equal
         (funcall (hm::graphviz-sequence-node-attribute *cfg* :polygon-distortion))
         (hm::quotes-around "0.0")))
    (is (equal
         (funcall (hm::graphviz-sequence-node-attribute *cfg* :fontcolor))
         (hm::quotes-around "black")))
    (is (equal
         (funcall (hm::graphviz-sequence-node-attribute *cfg* :fontsize-max))
         (hm::quotes-around "22.0")))
    (is (equal
         (funcall (hm::graphviz-sequence-node-attribute *cfg* :fontsize-min))
         (hm::quotes-around "6.0")))
    (is (equal
         (funcall (hm::graphviz-sequence-node-attribute *cfg* :fontsize))
         (hm::quotes-around "14.0")))
    (is (equal
         (funcall (hm::graphviz-sequence-node-attribute *cfg* :fontname))
         (hm::quotes-around "Helvetica")))
    (is (equal
         (funcall (hm::graphviz-sequence-node-attribute *cfg* :color))
         (hm::quotes-around "black")))
    (is (equal
         (funcall (hm::graphviz-sequence-node-attribute *cfg* :style))
         (hm::quotes-around "filled")))
    (is (equal
         (funcall (hm::graphviz-sequence-node-attribute *cfg* :colorscheme))
         (hm::quotes-around "x11")))
    (is (equal
         (funcall
          (hm::graphviz-sequence-node-attribute *cfg* :shape))
         (hm::quotes-around "box")))))

(deftest test-graphviz-sequence-graph-color ()
  (with-fixture default-config
    (is (equal "/x11/black" (hm::graphviz-sequence-graph-color *cfg* :fontcolor)))
    (is (equal "/x11/white" (hm::graphviz-sequence-graph-color *cfg* :bgcolor)))))

(deftest test-graphviz-classification ()
  "Test that classifications are read from a user configuration read from disk."
  (with-fixture roskams-h-seq
    (is (not (hm::graphviz-classification *sequence* :node :polygon-skew)))
    (is (not (hm::graphviz-classification *sequence* :node :polygon-sides)))
    (is (not (hm::graphviz-classification *sequence* :node :polygon-orientation)))
    (is (not (hm::graphviz-classification *sequence* :node :polygon-image)))
    (is (not (hm::graphviz-classification *sequence* :node :polygon-distortion)))
    (is (not (hm::graphviz-classification *sequence* :node :style)))
    (is (not (hm::graphviz-classification *sequence* :node :penwidth)))
    (is (not (hm::graphviz-classification *sequence* :node :color)))
    (is (eq :units (hm::graphviz-classification *sequence* :node :shape)))
    (is (not (hm::graphviz-classification *sequence* :node :fontcolor)))
    (is (not (hm::graphviz-classification *sequence* :node :fillcolor)))
    (is (not (hm::graphviz-classification *sequence* :edge :style)))
    (is (not (hm::graphviz-classification *sequence* :edge :arrowhead)))
    (is (not (hm::graphviz-classification *sequence* :edge :penwidth)))
    (is (not (hm::graphviz-classification *sequence* :edge :fontsize)))
    (is (not (hm::graphviz-classification *sequence* :edge :fontcolor)))
    (is (not (hm::graphviz-classification *sequence* :edge :color)))))

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
    (is (not (hm::chronology-graph-p *cfg*)))))

;; (deftest test-include-url-p ()
;;   "Test that INCLUDE-URL-P returns nil given the default configuration, and
;; that it errors out when set to an invalid value."
;;   (with-fixture default-config
;;     (is (not (hm::include-url-p *cfg*)))
;;     (set-option *cfg* "General configuration" "url-include" "foo")
;;     (with-expected-failures
;;       (is (hm::include-url-p *cfg*)))))

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
    (is (eq :units (hm::sequence-classifier *cfg* :node-shape-by)))
    (is (not (hm::sequence-classifier *cfg* :node-fontcolor-by)))
    (is (not (hm::sequence-classifier *cfg* :node-fillcolor-by)))))

(deftest test-lookup-graphviz-option ()
  "Test that LOOKUP-GRAPHVIZ-OPTION returns values from the default configuration."
  (with-fixture default-config
    (is (equal "filled"
               (hm::lookup-graphviz-option *cfg* :node :style :sequence)))
    (is (equal "solid"
               (hm::lookup-graphviz-option *cfg* :edge :style :sequence)))
    (is (equal "x11"
               (hm::lookup-graphviz-option *cfg* :edge :colorscheme :sequence)))
    (is (equal "x11"
               (hm::lookup-graphviz-option *cfg* :node :colorscheme :sequence)))
    (is (equal "0"
               (hm::lookup-graphviz-option *cfg* :node :origin :sequence
                                           :node-color-by :reachable)))
    (is (equal "1"
               (hm::lookup-graphviz-option *cfg* :node :reachable :sequence
                                                       :node-color-by :reachable)))
    (is (equal "2"
               (hm::lookup-graphviz-option *cfg* :node :not-reachable :sequence
                                           :node-color-by :reachable)))))

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
            (uiop:merge-pathnames* "test-config.ini" *path*))
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
    (let ((gen-path-name (uiop:merge-pathnames* "test-general-config.ini" *path*))
          (gv-path-name (uiop:merge-pathnames* "test-graphviz-config.ini" *path*))
          (config-from-file nil))
      (write-general-configuration *cfg* gen-path-name)
      (write-Graphviz-style-configuration *cfg* gv-path-name)
      (setf config-from-file
            (read-configuration-from-files nil gen-path-name gv-path-name))
      (is (equal (get-configuration-sections *cfg*)
                 (get-configuration-sections config-from-file)))
      (is (equal (get-all-configuration-options *cfg*)
                 (get-all-configuration-options config-from-file))))))

;; Data structure tests


;; Test for Roskam's h-structure example

(deftest test-roskams-h-structure-config ()
  (with-fixture roskams-h-structure
    (is (not (configuration-errors? *cfg*)))))

(deftest test-roskams-h-structure-graph ()
  (with-fixture roskams-h-structure
    (is (typep (hm::make-new-sequence-graph *cfg* nil)
               'graph:digraph))))

(deftest test-roskams-h-structure-configure-sequence ()
  (with-fixture roskams-h-structure
    (is (typep (hm:configure-archaeological-sequence
                (hm::make-archaeological-sequence) *cfg* nil)
               'hm::archaeological-sequence))))

(deftest test-roskams-h-structure-graph-nodes-edges ()
  (with-fixture roskams-h-structure
    (let ((g (hm::make-new-sequence-graph *cfg* nil)))
      (is (equal (graph:nodes g) '(|1| |2| |3| |4| |5|)))
      (is (equal
           (graph:edges g)
           '((|1| |3|) (|1| |4|) (|1| |5|) (|2| |3|) (|2| |5|) (|3| |5|) (|4| |5|)))))))


;; Tests using Roskams H sequence

(deftest test-to-dot-macro ()
  "Run TO-DOT-MACRO against roskams-h-seq to test anonymous functions."
  (with-fixture roskams-h-seq
    (is (equal (hm::quotes-around "solid")
               (funcall
                (hm::to-dot-macro *sequence* :edge :style :sequence nil))))
    (is (equal (hm::quotes-around "normal")
               (funcall
                (hm::to-dot-macro *sequence* :edge :arrowhead :sequence nil))))
    (is (equal (hm::quotes-around "14.0")
               (funcall
                (hm::to-dot-macro *sequence* :edge :fontsize :sequence nil))))
    (is (equal (hm::quotes-around "/x11/black")
               (funcall
                (hm::to-dot-macro *sequence* :edge :fontcolor :sequence nil))))
    (is (equal (hm::quotes-around "/x11/black")
               (funcall
                (hm::to-dot-macro *sequence* :edge :color :sequence nil))))
    (is (equal (hm::quotes-around "1.0")
               (funcall
                (hm::to-dot-macro *sequence* :edge :penwidth :sequence nil))))
    (is (equal (hm::quotes-around "box")
               (funcall
                (hm::to-dot-macro *sequence* :node :shape :sequence nil)
                (hm::symbolicate "1"))))
    (is (equal (hm::quotes-around "box")
               (funcall
                (hm::to-dot-macro *sequence* :node :shape :sequence nil)
                (hm::symbolicate "2"))))
    (is (equal (hm::quotes-around "filled")
               (funcall
                (hm::to-dot-macro *sequence* :node :style :sequence nil))))
    (is (equal (hm::quotes-around "/x11/black")
               (funcall
                (hm::to-dot-macro *sequence* :node :color :sequence nil))))
    (is (equal (hm::quotes-around "/x11/white")
               (funcall
                (hm::to-dot-macro *sequence* :node :fillcolor :sequence nil))))
    (is (equal (hm::quotes-around "/x11/black")
               (funcall
                (hm::to-dot-macro *sequence* :node :fontcolor :sequence nil))))
    (is (equal (hm::quotes-around "1.0")
               (funcall
                (hm::to-dot-macro *sequence* :node :penwidth :sequence nil))))))

(deftest test-write-dot-with-classifications ()
  "Test whether or not it is possible to write the sequence graph dot file for
roskams-h-class. Does not check for a correct output file, just whether is
exists or not."
  (with-fixture roskams-h-class
    (let ((old-file (probe-file
                     (hm::output-file-name
                      (hm::archaeological-sequence-configuration *sequence*)
                      "sequence-dot"))))
      (uiop:delete-file-if-exists old-file))
    (hm::write-sequence-graph-to-dot-file *sequence* nil)
    (is (probe-file
         (hm::output-file-name
          (hm::archaeological-sequence-configuration *sequence*)
          "sequence-dot")))))

(deftest test-periods-phases ()
  "Test whether fig-12 will produce a sequence graph dot file."
  (with-fixture fig-12
    (let ((old-file (probe-file
                     (hm::output-file-name
                      (hm::archaeological-sequence-configuration *sequence*)
                      "sequence-dot"))))
      (uiop:delete-file-if-exists old-file))
    (hm::write-sequence-graph-to-dot-file *sequence* nil)
    (is (probe-file
         (hm::output-file-name
          (hm::archaeological-sequence-configuration *sequence*)
          "sequence-dot")))))

(deftest test-reachable ()
  "Test that reachable classifies node fill color, shape, and pen width, and
edge color, pen width, arrowhead, and style, then writes a sequence graph dot
file. Checks whether the dot file exists, compiles it with dot, and opens the
resulting pdf file for viewing."
  (with-fixture fig-12-reachable
    (let* ((cfg (hm::archaeological-sequence-configuration *sequence*))
           (old-file (probe-file (hm::output-file-name cfg "sequence-dot"))))
      (uiop:delete-file-if-exists old-file)
      (hm::write-sequence-graph-to-dot-file *sequence* nil)
      (is (probe-file (hm::output-file-name cfg "sequence-dot")))
      (hm::make-graphics-file cfg :sequence "pdf" "open"))))

(deftest test-adjacent ()
  "Test that adjacent classifies node fill color, shape, and pen width, and edge
color, pen width, arrowhead, and style, then writes a sequence graph dot file.
Checks whether the dot file exists, compiles it with dot, and opens the
resulting pdf file for viewing."
  (with-fixture fig-12-adjacent
    (let* ((cfg (hm::archaeological-sequence-configuration *sequence*))
           (old-file (probe-file (hm::output-file-name cfg "sequence-dot"))))
      (uiop:delete-file-if-exists old-file)
      (hm::write-sequence-graph-to-dot-file *sequence* nil)
      (is (probe-file (hm::output-file-name cfg "sequence-dot")))
      (hm::make-graphics-file cfg :sequence "pdf" "open"))))

(deftest test-distance ()
  "Test that distance classifies node fill color, shape, and pen width, and edge
color, pen width, arrowhead, and style, then writes a sequence graph dot file.
Checks whether the dot file exists, compiles it with dot, and opens the
resulting pdf file for viewing."
  (with-fixture fig-12-distance
    (let* ((cfg (hm::archaeological-sequence-configuration *sequence*))
           (old-file (probe-file (hm::output-file-name cfg "sequence-dot"))))
      (uiop:delete-file-if-exists old-file)
      (hm::write-sequence-graph-to-dot-file *sequence* nil)
      (is (probe-file (hm::output-file-name cfg "sequence-dot")))
      (hm::make-graphics-file cfg :sequence "pdf" "open"))))

(deftest test-levels ()
  "Test that levels classifies node fill color, shape, and pen width, and edge
color, pen width, arrowhead, and style, then writes a sequence graph dot file.
Checks whether the dot file exists, compiles it with dot, and opens the
resulting pdf file for viewing."
  (with-fixture fig-12-levels
    (let* ((cfg (hm::archaeological-sequence-configuration *sequence*))
           (old-file (probe-file (hm::output-file-name cfg "sequence-dot"))))
      (uiop:delete-file-if-exists old-file)
      (hm::write-sequence-graph-to-dot-file *sequence* nil)
      (is (probe-file (hm::output-file-name cfg "sequence-dot")))
      (hm::make-graphics-file cfg :sequence "pdf" "open"))))


(deftest test-periods ()
  "Test that periods classifies node fill color, shape, and pen width, and edge
color, pen width, arrowhead, and style, then writes a sequence graph dot file.
Checks whether the dot file exists, compiles it with dot, and opens the
resulting pdf file for viewing."
  (with-fixture fig-12-periods
    (let* ((cfg (hm::archaeological-sequence-configuration *sequence*))
           (old-file (probe-file (hm::output-file-name cfg "sequence-dot"))))
      (uiop:delete-file-if-exists old-file)
      (hm::write-sequence-graph-to-dot-file *sequence* nil)
      (is (probe-file (hm::output-file-name cfg "sequence-dot")))
      (hm::make-graphics-file cfg :sequence "pdf" "open"))))

(deftest test-phases ()
  "Test that phases classifies node fill color, shape, and pen width, and edge
color, pen width, arrowhead, and style, then writes a sequence graph dot file.
Checks whether the dot file exists, compiles it with dot, and opens the
resulting pdf file for viewing."
  (with-fixture fig-12-phases
    (let* ((cfg (hm::archaeological-sequence-configuration *sequence*))
           (old-file (probe-file (hm::output-file-name cfg "sequence-dot"))))
      (uiop:delete-file-if-exists old-file)
      (hm::write-sequence-graph-to-dot-file *sequence* nil)
      (is (probe-file (hm::output-file-name cfg "sequence-dot")))
      (hm::make-graphics-file cfg :sequence "pdf" "open"))))

(deftest test-units ()
  "Test that units classifies node fill color, shape, and pen width, and edge
color, pen width, arrowhead, and style, then writes a sequence graph dot file.
Checks whether the dot file is exists, compiles it with dot, and opens the
resulting pdf file for viewing."
  (with-fixture fig-12-units
    (let* ((cfg (hm::archaeological-sequence-configuration *sequence*))
           (old-file (probe-file (hm::output-file-name cfg "sequence-dot"))))
      (uiop:delete-file-if-exists old-file)
      (hm::write-sequence-graph-to-dot-file *sequence* nil)
      (is (probe-file (hm::output-file-name cfg "sequence-dot")))
      (hm::make-graphics-file cfg :sequence "pdf" "open"))))

(deftest test-polygon-distortion ()
  "Test that distance classifies polygon distortion, then writes a sequence
graph dot file. Checks whether the dot file exists, compiles it with dot, and
opens the resulting pdf file for viewing."
  (with-fixture fig-12-polygon-distortion
    (let* ((cfg (hm::archaeological-sequence-configuration *sequence*))
           (old-file (probe-file (hm::output-file-name cfg "sequence-dot"))))
      (uiop:delete-file-if-exists old-file)
      (hm::write-sequence-graph-to-dot-file *sequence* nil)
      (is (probe-file (hm::output-file-name cfg "sequence-dot")))
      (hm::make-graphics-file cfg :sequence "pdf" "open"))))

(deftest test-polygon-orientation ()
  "Test that distance classifies polygon orientation, then writes a sequence
graph dot file. Checks whether the dot file exists, compiles it with dot, and
opens the resulting pdf file for viewing."
  (with-fixture fig-12-polygon-orientation
    (let* ((cfg (hm::archaeological-sequence-configuration *sequence*))
           (old-file (probe-file (hm::output-file-name cfg "sequence-dot"))))
      (uiop:delete-file-if-exists old-file)
      (hm::write-sequence-graph-to-dot-file *sequence* nil)
      (is (probe-file (hm::output-file-name cfg "sequence-dot")))
      (hm::make-graphics-file cfg :sequence "pdf" "open"))))

(deftest test-polygon-sides ()
  "Test that distance classifies polygon sides, then writes a sequence graph dot
file. Checks whether the dot file exists, compiles it with dot, and opens the
resulting pdf file for viewing."
  (with-fixture fig-12-polygon-sides
    (let* ((cfg (hm::archaeological-sequence-configuration *sequence*))
           (old-file (probe-file (hm::output-file-name cfg "sequence-dot"))))
      (uiop:delete-file-if-exists old-file)
      (hm::write-sequence-graph-to-dot-file *sequence* nil)
      (is (probe-file (hm::output-file-name cfg "sequence-dot")))
      (hm::make-graphics-file cfg :sequence "pdf" "open"))))

(deftest test-polygon-skew ()
  "Test that distance classifies polygon skew, then writes a sequence graph dot
file. Checks whether the dot file exists, compiles it with dot, and opens the
resulting pdf file for viewing."
  (with-fixture fig-12-polygon-skew
    (let* ((cfg (hm::archaeological-sequence-configuration *sequence*))
           (old-file (probe-file (hm::output-file-name cfg "sequence-dot"))))
      (uiop:delete-file-if-exists old-file)
      (hm::write-sequence-graph-to-dot-file *sequence* nil)
      (is (probe-file (hm::output-file-name cfg "sequence-dot")))
      (hm::make-graphics-file cfg :sequence "pdf" "open"))))

(deftest test-correlations ()
  "Test that correlations are made correctly for periods, then writes a sequence
graph dot file. Checks whether the dot file exists, compiles it with dot, and
opens the resulting pdf file for viewing."
  (with-fixture fig-12-correlations
    (let* ((cfg (hm::archaeological-sequence-configuration *sequence*))
           (old-file (probe-file (hm::output-file-name cfg "sequence-dot"))))
      (uiop:delete-file-if-exists old-file)
      (hm::write-sequence-graph-to-dot-file *sequence* nil)
      (is (probe-file (hm::output-file-name cfg "sequence-dot")))
      (hm::make-graphics-file cfg :sequence "pdf" "open"))))

(deftest test-chronology ()
  "Test the chronology graph. Checks whether the dot file exists, compiles it
with dot, and opens the resulting pdf file for viewing."
  (with-fixture fig-12-chronology
    (let* ((cfg (hm::archaeological-sequence-configuration *sequence*))
           (old-file (probe-file (hm::output-file-name cfg "chronology-dot"))))
      (uiop:delete-file-if-exists old-file)
      (hm::write-chronology-graph-to-dot-file *sequence* nil)
      (is (probe-file (hm::output-file-name cfg "chronology-dot")))
      (hm::make-graphics-file cfg :chronology "pdf" "open"))))

;; (deftest test-bldg-1-5 ()
;;   "Test that a large project actually runs, then writes a sequence graph dot
;; file. Does not test whether the dot file is correct."
;;   (with-fixture bldg-1-5
;;       (let ((old-file (probe-file
;;                        (hm::output-file-name
;;                         (hm::archaeological-sequence-configuration *sequence*)
;;                         "sequence-dot"))))
;;         (uiop:delete-file-if-exists old-file))
;;     (hm::write-sequence-graph-to-dot-file *sequence* nil)
;;     (is (probe-file
;;          (hm::output-file-name
;;           (hm::archaeological-sequence-configuration *sequence*)
;;           "sequence-dot")))))
