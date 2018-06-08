;;; hm-fas.lisp

;; Copyright (C) Thomas Dye 2018

;; Licensed under the Gnu Public License Version 3 or later

;; This is a Common Lisp version based on the Java implementation here:
;; https://github.com/stamps/FAS/blob/master/ArrayFAS.java

(in-package  #:hm)

(defun array-fas (graph &optional (verbose t))
  "Estimate the feedback arc set of GRAPH using the method of Eades et al. 1993."
  (let* ((n-nodes (length (graph:nodes graph)))
         (n-classes (- (* 2 n-nodes) 3))
         (bins (make-array n-classes :initial-element -1))
         (deltas (make-array n-nodes :initial-element 0))
         (prev (make-array n-nodes :initial-element -1))
         (next (make-array n-nodes :initial-element -1))
         (max-delta most-negative-fixnum)
         (seq))
    (dolist (node (graph:nodes graph))
      (let ((out-degree (graph:outdegree graph node))
            (in-degree (graph:indegree graph node))
            (u (node-to-int graph node)))
        (cond ((= out-degree 0)
               (multiple-value-setq (bins prev next max-delta)
                 (add-to-bin
                  (- 2 n-nodes) u bins prev next max-delta n-nodes))
               (setf (svref deltas u) (- 2 n-nodes)))
              ((and (= in-degree 0) (> out-degree 0))
               (multiple-value-setq (bins prev next max-delta)
                 (add-to-bin
                  (- n-nodes 2) u bins prev next max-delta n-nodes))
               (setf (svref deltas u) (- n-nodes 2)))
              (t (let ((d (- out-degree in-degree)))
                   (multiple-value-setq (bins prev next max-delta)
                     (add-to-bin d u bins prev next max-delta n-nodes))
                   (setf (svref deltas u) d))))))
    (compute-fas seq n-nodes bins prev next n-classes max-delta graph
                 deltas verbose)))

(defun add-to-bin (delta node bins prev next max-delta n-nodes)
  (let ((b bins)
        (p prev)
        (n next)
        (md max-delta)
        (ind (- delta (- 2 n-nodes))))
    (if (= (svref b ind) -1)
        (progn
          (setf (svref b ind) node)
          (setf (svref p node) -1))
        (progn
          (setf (svref n (svref b ind)) node)
          (setf (svref p node) (svref b ind))
          (setf (svref b ind) node)))
    (setf (svref n node) -1)
    (when (and (< delta (- n-nodes 2)) (< md delta)) (setf md delta))
    (values b p n md)))

(defun compute-seq (n-nodes bins prev next n-classes max-delta graph deltas)
  "Returns a new seq and modified BINS, PREV, NEXT, MAX-DELTA, and GRAPH."
  (let ((s1)
        (s2)
        (numdel 0))
    (loop while (< numdel n-nodes) do
      (loop while (/= (svref bins 0) -1) do
        (let ((u (svref bins 0)))
          (setf (svref bins 0) (svref prev u))
          (unless (= (svref prev u) -1) (setf (svref next (svref prev u)) -1))
          (multiple-value-setq (graph deltas prev next bins max-delta)
            (delete-node u graph deltas prev next bins max-delta n-nodes))
          (incf numdel)
          (push u s2)))
      (loop while (/= (svref bins (- n-classes 1)) -1) do
        (let ((u (svref bins (- n-classes 1))))
          (setf (svref bins (- n-classes 1)) (svref prev u))
          (unless (= (svref prev u) -1) (setf (svref next (svref prev u)) -1))
          (multiple-value-setq (graph deltas prev next bins max-delta)
              (delete-node u graph deltas prev next bins max-delta n-nodes))
          (incf numdel)
          (setf s1 (append s1 (list u)))))
      (when (< numdel n-nodes)
        (let ((u (svref bins (- max-delta (- 2 n-nodes)))))
          (when (= (svref bins (- max-delta (- 2 n-nodes))) -1)
            (format t "max-delta bin is empty: ~a.~&" max-delta))
          ;; write to file
          (setf (svref bins (- max-delta (- 2 n-nodes))) (svref prev u))
          (unless (= (svref prev u) -1) (setf (svref next (svref prev u)) -1))
          (setf max-delta (update-max-delta max-delta bins n-nodes))
          (multiple-value-setq (graph deltas prev next bins max-delta)
              (delete-node u graph deltas prev next bins max-delta n-nodes))
          (incf numdel)
          (setf s1 (append s1 (list u))))))
    (values (append s1 s2) bins next max-delta graph)))

(defun delete-node (u graph deltas prev next bins max-delta n-nodes)
  (setf (svref deltas u) most-negative-fixnum)
  (multiple-value-setq (deltas bins next prev max-delta)
      (delete-node- graph u t deltas bins prev next max-delta n-nodes))
  (multiple-value-setq (deltas bins next prev max-delta)
      (delete-node- graph u nil deltas bins prev next max-delta n-nodes))
  (setf (svref prev u) -1)
  (setf (svref next u) -1)
  (values graph deltas prev next bins max-delta))

(defun delete-node- (graph u out deltas bins prev next max-delta n-nodes)
  (let ((neighbors (graph:adjacent-from graph (int-to-node graph u))))
    (dolist (neighbor neighbors)
      (let ((v (node-to-int graph neighbor)))
        (when (> (svref deltas v) most-negative-fixnum)
          (let* ((old-delta (svref deltas v))
                 (new-delta old-delta))
            (if out (incf new-delta) (decf new-delta))
            (setf (svref deltas v) new-delta)
            (when (= (svref bins (- old-delta (- 2 n-nodes))) v)
              (setf (svref bins (- old-delta (- 2 n-nodes))) (svref prev v)))
            (unless (= (svref prev v) -1)
              (setf (svref next (svref prev v)) (svref next v)))
            (unless (= (svref next v) -1)
              (setf (svref prev (svref next v)) (svref prev v)))
            (multiple-value-setq (bins prev next max-delta)
                (add-to-bin new-delta v bins prev next max-delta n-nodes))
            (setf max-delta (update-max-delta max-delta bins n-nodes))))))
    (values deltas bins next prev max-delta)))

(defun update-max-delta (max-delta bins n)
  "Update and return max-delta."
  (let ((delta max-delta))
    (if (and (= delta max-delta) (= (svref bins (- delta (- 2 n))) -1))
        (loop :while (= (svref bins (- max-delta (- 2 n))) -1) :do
          (decf max-delta)
          (when (= max-delta (- 2 n)) (loop-finish))
              :finally (return max-delta))
        max-delta)))

(defun compute-fas (seq n-nodes bins prev next n-classes max-delta graph deltas
                    &optional (verbose t))
  (let ((s seq)
        (varray)
        (fvs (make-array n-nodes :initial-element nil))
        (fas 0)
        (ret))
    (unless s (multiple-value-setq (s bins next max-delta graph)
                (compute-seq n-nodes bins prev next n-classes max-delta
                             graph deltas)))
    (setf varray (make-array n-nodes :initial-contents s))
    (dolist (node (graph:nodes graph))
      (let ((v (node-to-int graph node))
            (neighbors (graph:adjacent-from graph node)))
        (dolist (neighbor neighbors)
          (let ((w (node-to-int graph neighbor)))
            (when (> (svref varray v) (svref varray w))
              (setf (svref fvs v) t)
              (incf fas))))))
    (when verbose (format t "fvs size is ~d.~&" (count t fvs)))
    (when verbose (format t "fas size is ~d.~&" fas))
    (loop for i below n-nodes do
      (when (svref fvs i) (push (int-to-node graph i) ret)))
    ret))


(defun make-node-to-int (graph)
  "Return an fset map where key is a node in GRAPH and value is its integer index."
  (let ((ret (fset:empty-map))
        (count 0))
    (dolist (node (graph:nodes graph))
      (setf ret (fset:with ret node count))
      (incf count))
    ret))

(defun make-int-to-node (graph)
  "Return an fset map where key is an integer index and value is a node in GRAPH."
  (let ((ret (fset:empty-map))
        (count 0))
    (dolist (node (graph:nodes graph))
      (setf ret (fset:with ret count node))
      (incf count))
    ret))

(defun node-to-int (graph node)
  "Return the integer index of NODE in GRAPH."
  (let ((index (make-node-to-int graph)))
    (fset:@ index node)))

(defun int-to-node (graph int)
  "Return the node of GRAPH indexed by the integer INT."
  (let ((index (make-int-to-node graph)))
    (fset:@ index int)))
