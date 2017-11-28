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
  (is (string= (funcall (hm::graphviz-edge-style) 0) "solid"))
  (is (string= (funcall (hm::graphviz-edge-style) 1) "dashed"))
  (is (string= (funcall (hm::graphviz-edge-style) 2) "dotted"))
  (is (string= (funcall (hm::graphviz-edge-style) 3) "bold"))
  (is (string= (funcall (hm::graphviz-edge-style) 4) "solid")))

(deftest graphviz-node-style-test ()
  (is (string= (funcall (hm::graphviz-node-style) 0) "solid"))
  (is (string= (funcall (hm::graphviz-node-style) 1) "dashed"))
  (is (string= (funcall (hm::graphviz-node-style) 2) "dotted"))
  (is (string= (funcall (hm::graphviz-node-style) 3) "bold"))
  (is (string= (funcall (hm::graphviz-node-style) 4) "rounded"))
  (is (string= (funcall (hm::graphviz-node-style) 10) "solid")))

(deftest graphviz-node-shape-test ()
  (is (string= (funcall (hm::graphviz-node-shape) 0) "box"))
  (is (string= (funcall (hm::graphviz-node-shape) 1) "polygon"))
  (is (string= (funcall (hm::graphviz-node-shape) 2) "ellipse"))
  (is (string= (funcall (hm::graphviz-node-shape) 3) "egg"))
  (is (string= (funcall (hm::graphviz-node-shape) 4) "triangle"))
  (is (string= (funcall (hm::graphviz-node-shape) 39) "box")))

(deftest graphviz-arrow-shape-test ()
  (is (string= (funcall (hm::graphviz-arrow-shape) 0) "none"))
  (is (string= (funcall (hm::graphviz-arrow-shape) 1) "box"))
  (is (string= (funcall (hm::graphviz-arrow-shape) 2) "lbox"))
  (is (string= (funcall (hm::graphviz-arrow-shape) 42) "none")))

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

(deftest test-lookup-fast-matrix ()
  (with-fixture default-config
    (is (hm:fast-matrix-p *cfg*))
    (hm::set-option *cfg* "General configuration" "fast-matrix" "off")
    (is (not (hm:fast-matrix-p *cfg*)))
    (hm::set-option *cfg* "General configuration" "fast-matrix" "foo")
    (with-expected-failures
      (is (not (hm:fast-matrix-p *cfg*))))))

(deftest test-lookup-graphviz-option ()
  (with-fixture default-config
    (is (string= "filled"
                 (hm::lookup-graphviz-option *cfg* "node" "style" "sequence")))
    (is (string= "solid"
                 (hm::lookup-graphviz-option *cfg* "edge" "style" "sequence")))
    (is (string= "x11"
                 (hm::lookup-graphviz-option *cfg* "edge" "colorscheme" "sequence")))
    (is (string= "x11"
                 (hm::lookup-graphviz-option *cfg* "node" "colorscheme" "sequence")))
    (is (string= "1"
                 (hm::lookup-graphviz-option *cfg* "node" "origin" "sequence"
                                             "node-color-by" "reachable")))
    (is (string= "2"
                 (hm::lookup-graphviz-option *cfg* "node" "reachable" "sequence"
                                             "node-color-by" "reachable")))
    (is (string= "3"
                 (hm::lookup-graphviz-option *cfg* "node" "not-reachable" "sequence"
                                             "node-color-by" "reachable")))))

(deftest test-graphviz-classification ()
  (with-fixture roskams-h-seq
    (is (not (hm::graphviz-classification *roskams-h-seq* "node" "fill")))
    (is (not (hm::graphviz-classification *roskams-h-seq* "node" "color")))
    (is (string= "units"
                 (hm::graphviz-classification *roskams-h-seq* "node" "shape")))
    (is (not (hm::graphviz-classification *roskams-h-seq* "edge" "style")))
    (is (not (hm::graphviz-classification *roskams-h-seq* "edge" "penwidth")))
    (is (not (hm::graphviz-classification *roskams-h-seq* "edge" "color")))))

;; test that configurations are written to file and read back in correctly
(deftest read-write-configuration ()
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

;; test that configurations written to two files are read back in correctly
(deftest read-write-split-configuration ()
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

;; test reset-option
(deftest test-reset-option ()
  (let ((default-config (hm:default-configuration)))
    (with-fixture default-config
      (is (equal (get-all-configuration-options *cfg*)
                 (get-all-configuration-options default-config)))
      (reset-option default-config "General configuration" "legend" "on")
      (is (not (equal (get-all-configuration-options *cfg*)
                      (get-all-configuration-options default-config)))))))

;; test set-input-file
(deftest test-set-input-file ()
  (with-fixture default-config
    (let* ((file-name-string "test/assets/configurations/contexts-eg.csv"))
      (set-input-file *cfg* "contexts" file-name-string t)
      (set-input-file *cfg* "observations" file-name-string nil)
      (is (string= (hm::get-option *cfg* "Input files" "contexts")
                   file-name-string))
      (is (string= (hm::get-option *cfg* "Input files" "observations")
                   file-name-string))
      (is (hm::get-option *cfg* "Input file headers" "contexts" :type :boolean))
      (is (not
           (hm::get-option *cfg* "Input file headers" "observations"
                           :type :boolean))))))

;; test set-dot-file
(deftest test-set-dot-file ()
  (with-fixture default-config
    (set-dot-file *cfg* "chronology-dot" "foo.dot" nil)
    (set-dot-file *cfg* "sequence-dot" "bar.dot" nil)
    (is (string= (hm::get-option *cfg* "Output files" "chronology-dot") "foo.dot"))
    (is (string= (hm::get-option *cfg* "Output files" "sequence-dot") "bar.dot"))))

;;; Color functions

(deftest test-graphviz-color-string ()
  (is (string= (hm::graphviz-color-string 1 "reds" 3) "/reds3/2"))
  (is (string= (hm::graphviz-color-string "white" "x11") "/x11/white"))
  (is (string= (hm::graphviz-color-string "white" "x11" 3) "/x11/white"))
  (is (string= (hm::graphviz-color-string "blue" "solarized") "#268bd2"))
  (is (string= (hm::graphviz-color-string "base03" "solarized") "#002b36"))
  (is (string= (hm::graphviz-color-string 0 "cet-inferno" 256) "0.640 1.000 0.365")))

(deftest test-graphviz-hsv-string ()
  (is (string= (hm::graphviz-hsv-string "white") "0.000 0.000 1.000"))
  (is (string= (hm::graphviz-hsv-string "black") "0.000 0.000 0.000"))
  (is (string= (hm::graphviz-hsv-string "red") "0.000 1.000 1.000"))
  (is (string= (hm::graphviz-hsv-string "turquoise") "0.483 0.714 0.878"))
  (is (string= (hm::graphviz-hsv-string "sienna") "0.054 0.719 0.627")))

(deftest test-graphviz-color-from-ramp ()
  (is (string= (hm::graphviz-color-from-ramp 0 "black" "white" 3)
               "0.000 0.000 0.000"))
  (is (string= (hm::graphviz-color-from-ramp 1 "black" "white" 3)
               "0.000 0.000 0.333"))
  (is (string= (hm::graphviz-color-from-ramp 2 "black" "white" 3)
               "0.000 0.000 0.667"))
  (is (string= (hm::graphviz-color-from-ramp 3 "black" "white" 3)
               "0.000 0.000 1.000")))

(deftest test-cet-color ()
  (is (string= (hm::cet-color "cet-inferno" 0 256) "0.640 1.000 0.365"))
  (is (string= (hm::cet-color "cet-inferno" 255 256) "0.171 0.687 0.976"))
  (is (string= (hm::cet-color "cet-inferno" 0 1) "0.943 0.780 0.910"))
  (is (string= (hm::cet-color "cet-bgyw" 0 1) "0.310 0.590 0.612"))
  (is (string= (hm::cet-color "cet-kbc" 0 1) "0.615 0.822 0.992"))
  (is (string= (hm::cet-color "cet-blues" 0 1) "0.589 0.332 0.863"))
  (is (string= (hm::cet-color "cet-bmw" 0 1) "0.784 0.890 0.996"))
  (is (string= (hm::cet-color "cet-kgy" 0 1) "0.307 0.920 0.541"))
  (is (string= (hm::cet-color "cet-gray" 0 1) "0.000 0.000 0.467"))
  (is (string= (hm::cet-color "cet-dimgray" 0 1) "0.000 0.000 0.494"))
  (is (string= (hm::cet-color "cet-fire" 0 1) "0.015 1.000 0.929"))
  (is (string= (hm::cet-color "cet-kb" 0 1) "0.623 0.902 0.518"))
  (is (string= (hm::cet-color "cet-kg" 0 1) "0.333 1.000 0.255"))
  (is (string= (hm::cet-color "cet-kr" 0 1) "0.031 1.000 0.463"))
  (is (string= (hm::cet-color "cet-rainbow" 0 1) "0.179 0.834 0.757")))

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
