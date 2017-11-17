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

;;; Tests for internal functions

(deftest fast-matrix-test ()
  (with-fixture default-config
    (is (hm:fast-matrix-p *cfg*))
    (hm::set-option *cfg* "General configuration" "fast-matrix" "off")
    (is (not (hm:fast-matrix-p *cfg*)))
    (hm::set-option *cfg* "General configuration" "fast-matrix" "foo")
    (is (not (hm:fast-matrix-p *cfg*)))))

(deftest quotes-around-test ()
  (let ((goal "\"abc\""))
    (is (string= (hm::quotes-around "abc") goal))
    (is (string= (hm::quotes-around "\"abc\"") goal))))

(deftest graphviz-edge-style-test ()
  (is (string= (funcall (hm::graphviz-edge-style) 0) "solid"))
  (is (string= (funcall (hm::graphviz-edge-style) 1) "dashed"))
  (is (string= (funcall (hm::graphviz-edge-style)  2) "dotted"))
  (is (string= (funcall (hm::graphviz-edge-style)  3) "bold")))

(deftest lookup-option-test ()
  (with-fixture default-config
    (let ((master (hm:master-table))
          (result t))
      (dolist (row master)
        (and result
             (or
              (string= (nth 6 row)
                       (hm:lookup-option *cfg* (nth 1 row) (nth 2 row)
                                         (nth 3 row) (nth 4 row) (nth 5 row)))
              (setf result nil))))
      (is result))))

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

;;; Tests for exported functions

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

;; color functions

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
