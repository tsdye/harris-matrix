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
  (is (string= (hm::graphviz-edge-style 0) "solid"))
  (is (string= (hm::graphviz-edge-style 1) "dashed"))
  (is (string= (hm::graphviz-edge-style 2) "dotted"))
  (is (string= (hm::graphviz-edge-style 3) "bold"))
  (is (string= (hm::graphviz-edge-style 4) "solid"))
  (is (string= (hm::graphviz-edge-style 5) "dashed"))
  (is (string= (hm::graphviz-edge-style 6) "dotted"))
  (is (string= (hm::graphviz-edge-style 7) "bold")))

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

(deftest solarized-map-name ()
  (is (string-equal "002b36" (hm::solarized-map "base03"))))

(deftest solarized-map-name-member()
  (is (hm::solarized-map "base03" :member t)))

;;; Tests for exported functions

;; test that configurations are written to file and read back in correctly
(deftest read-write-configuration ()
  (let ((path-name "../test/temp/test-config.ini")
        (config-from-file nil))
    (with-fixture default-config
      (write-configuration *cfg* path-name)
      (setf config-from-file (read-configuration-from-files nil path-name))
      (is (equal (get-configuration-sections *cfg*)
                 (get-configuration-sections config-from-file)))
      (is (equal (get-all-configuration-options *cfg*)
                 (get-all-configuration-options config-from-file))))))

;; test that configurations written to two files are read back in correctly
(deftest read-write-split-configuration ()
  (let ((general-path-name "../test/temp/test-general-config.ini")
        (graphviz-path-name "../test/temp/test-graphviz-config.ini")
        (config-from-file nil))
    (with-fixture default-config
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
  (let ((file-name "../test/temp/contexts-eg.csv"))
    (with-fixture default-config
      (set-input-file *cfg* "contexts" file-name t)
      (set-input-file *cfg* "observations" file-name nil)
      (is (string= (hm::get-option *cfg* "Input files" "contexts") file-name))
      (is (string= (hm::get-option *cfg* "Input files" "observations") file-name))
      (is (hm::get-option *cfg* "Input file headers" "contexts" :type :boolean))
      (is
       (not
        (hm::get-option *cfg* "Input file headers" "observations"
                        :type :boolean))))))

;; test set-dot-file
(deftest test-set-dot-file ()
  (with-fixture default-config
    (set-dot-file *cfg* "chronology-dot" "foo.dot" nil)
    (set-dot-file *cfg* "sequence-dot" "bar.dot" nil)
    (is (string= (hm::get-option *cfg* "Output files" "chronology-dot") "foo.dot"))
    (is (string= (hm::get-option *cfg* "Output files" "sequence-dot") "bar.dot"))))
