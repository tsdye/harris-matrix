;;;; test/hm.lisp --- tests for hm application

;;; Copyright (C) Thomas S. Dye 2016

;;; Licensed under the Gnu Public License Version 3 or later

;;; Setup code:
(in-package :hm-test)

(defsuite test)
(in-suite test)

(defvar *digraph* nil
  "Variable for use in hm tests.")

(defixture transitive-graph
    (:setup (setf *digraph*
                  (graph:populate (make-instance 'graph:digraph)
                                  :nodes '(a b c)
                                  :edges '((a b)
                                           (b c)
                                           (a c)))))
    (:teardown (setf *digraph* nil)))

;;; Tests

(deftest transitive-reduction ()
  (with-fixture transitive-graph
      (is (set-equal (edges (transitive-reduction *digraph* nil))
                     '((a b) (b c))
                     :test 'tree-equal))))
