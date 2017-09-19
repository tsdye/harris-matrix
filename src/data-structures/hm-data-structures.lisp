;;; hm-data-structures.lisp

;; Copyright (C) Thomas Dye 2017

;; Licensed under the Gnu Public License Version 3 or later

(in-package #:hm)

(defun make-map (cfg element dot-attr user-class)
  (cond
    ((fset:contains?
      (fset:set "color" "fill" "fontcolor") dot-attr)
     (make-color-map cfg element dot-attr))
    ((fset:contains?
      (fset:set "shape" "style" "polygon-image" "classify")
      dot-attr)
     (make-string-map cfg element dot-attr))
    (t (make-integer-map cfg element dot-attr))))

(defun make-vector (cfg graph user-class))

(defun make-matrix (cfg graph user-class from-node))

(defun get-from-map (map key))

(defun get-from-vector (vector element))

(defun get-from-matrix (matrix from to))

(defun tables-to-map (contexts other-table table-type)
  "Given a CONTEXTS table, an OTHER-TABLE, and a TABLE-TYPE in
`periods' or `phases', return an fset map where the key is the context
and the value is either the period or phase of the context."
  (let ((ht (fset:empty-map))
        (ret (fset:empty-map))
        (n (cond
            ((string= table-type "periods") 3)
            ((string= table-type "phases") 4)
            (t (error "Error: unrecognized table type")))))
    (mapcar #'(lambda (x)
                (setf ht (fset:with ht (nth 0 x) (nth 2 x))))
            other-table)
    (mapcar #'(lambda (x)
                (setf ret
                      (fset:with ret (symbolicate (nth 0 x))
                                 (read-from-string (fset:@ ht (nth n x))))))
            contexts)
    ret))

(defun alist-to-map (alist)
  "Given an assoc-list, ALIST, return the corresponding fset map."
  (let ((ret (fset:empty-map)))
    (mapc #'(lambda (pair)
              (setq ret (fset:with ret (car pair) (cdr pair))))
          alist)
    ret))

(defun context-type-to-map (contexts)
  "Given a context table, CONTEXTS, return an fset map where the key
  is the context and the value is 0 if the unit-type is `deposit' or 1
  if the unit-type is `interface'."
  (let ((ret (fset:empty-map)))
    (mapcar #'(lambda (x)
                (setf ret
                      (fset:with ret (symbolicate (nth 0 x))
                                 (cond
                                   ((string= (nth 1 x) "deposit") 0)
                                   ((string= (nth 1 x) "interface") 1)))))
            contexts)
    ret))


;; ;; Needs work!
;; (defun process-user-value (value dot-attr option seq &key element)
;;   (if (fset:contains? (fset:set "color" "fillcolor" "fontcolor")
;;                       dot-attr)
;;       (let* ((section (if (equal element :node)
;;                           "Graphviz sequence node attributes"
;;                         "Graphviz sequence edge attributes"))
;;              (scheme (get-option (archaeological-sequence-configuration seq)
;;                                  section
;;                                  "colorscheme"))
;;              (classifiers (archaeological-sequence-classifiers seq))
;;              (range (unless (emptyp option)
;;                       (round (1+ (- (fset:greatest (fset:range (fset:@ classifiers option)))
;;                                     (fset:least (fset:range (fset:@ classifiers option)))))))))
;;         (if (and option
;;                  (not (emptyp scheme)))
;;             (quotes-around (graphviz-color-string value scheme range))
;;           ""))
;;     (quotes-around value)))

;; (defun edge--process-matrix-value (value attr option seq)
;;   "Given a VALUE from a graph matrix, a Graphviz attribute, ATTR, an ini file
;; OPTION, and an archaeological sequence, SEQ, return a properly formatted
;; Graphviz dot value."
;;   (let ((range (fset:range (fset:@ (archaeological-sequence-classifiers seq)
;;                                    option))))
;;     (cond
;;      ((string= attr "style")
;;       (quotes-around (graphviz-edge-style value)))
;;      ((fset:contains? (fset:set "color" "fontcolor")
;;                       attr)
;;       (quotes-around (graphviz-color-string value
;;                                             (get-option (archaeological-sequence-configuration seq)
;;                                                         "Graphviz sequence edge attributes"
;;                                                         "colorscheme")
;;                                             (1+ (- (fset:greatest range)
;;                                                    (fset:least range))))))
;;      ((string= attr "penwidth")
;;       (let ((interval (- (get-option (archaeological-sequence-configuration seq)
;;                                      "Graphviz sequence edge attributes"
;;                                      "penwidth-max"
;;                                      :type :number)
;;                          (get-option (archaeological-sequence-configuration seq)
;;                                      "Graphviz sequence edge attributes"
;;                                      "penwidth-min"
;;                                      :type :number)))
;;             (base (get-option (archaeological-sequence-configuration seq)
;;                               "Graphviz sequence edge attributes"
;;                               "penwidth-min"
;;                               :type :number)))
;;         (quotes-around (+ base
;;                           (* value
;;                              (/ interval
;;                                 (1+ (- (fset:greatest range)
;;                                        (fset:least range))))))))))))

;; (defun node--process-matrix-value (value attr option seq)
;;   "Given a VALUE from a graph matrix, a Graphviz attribute, ATTR, an ini file
;; OPTION, and an archaeological sequence, SEQ, return a properly formatted
;; Graphviz dot value."
;;   (let ((range (fset:range (fset:@ (archaeological-sequence-classifiers seq)
;;                                    option))))
;;     (cond
;;      ((string= attr "shape")
;;       (quotes-around (graphviz-node-shape value)))
;;      ((string= attr "style")
;;       (quotes-around (graphviz-node-style value)))
;;      ((fset:contains? (fset:set "color" "fillcolor" "fontcolor")
;;                       attr)
;;       (let ((scheme (get-option (archaeological-sequence-configuration seq)
;;                                 "Graphviz sequence node attributes"
;;                                 "colorscheme")))
;;         (quotes-around (graphviz-color-string value
;;                                               scheme
;;                                               (1+ (- (fset:greatest range)
;;                                                      (fset:least range)))))))
;;      ((string= attr "penwidth")
;;       (let ((interval (- (get-option (archaeological-sequence-configuration seq)
;;                                      "Graphviz sequence node attributes"
;;                                      "penwidth-max"
;;                                      :type :number)
;;                          (get-option (archaeological-sequence-configuration seq)
;;                                      "Graphviz sequence node attributes"
;;                                      "penwidth-min"
;;                                      :type :number)))
;;             (base (get-option (archaeological-sequence-configuration seq)
;;                               "Graphviz sequence node attributes"
;;                               "penwidth-min"
;;                               :type :number)))
;;         (quotes-around (+ base
;;                           (* value
;;                              (/ interval
;;                                 (1+ (- (fset:greatest range)
;;                                        (fset:least range))))))))))))

;; (lol:defmacro! <-dot-edge (o!seq classifier dot-attr)
;;   "Make functions to configure Graphviz edges."
;;   `(let* ((g!cfg (archaeological-sequence-configuration ,g!seq))
;;           (g!graph (archaeological-sequence-graph ,g!seq))
;;           (g!classifiers (archaeological-sequence-classifiers ,g!seq))
;;           (g!attribute ,dot-attr)
;;           (g!map (make-edge-lookup-map g!cfg))
;;           (g!class-with-node (graphviz-edge-classification g!cfg "classify"))
;;           (g!value (when ,classifier (sequence-classifier g!cfg ,classifier))))
;;      (if (or (not ,classifier) (and ,classifier (emptyp g!value))
;;              (not (fset:domain-contains? g!classifiers g!value)))
;;          (let ((g!y (process-user-value
;;                    (lookup-option g!map g!attribute) g!attribute g!value ,g!seq)))
;;            (constantly (if (emptyp g!y) "" g!y)))
;;        (cond
;;         ((string= "units" g!value)
;;          (lambda (x)
;;            (let ((g!user-value
;;                    (if (= 0 (round (fset:@ (fset:@ g!classifiers g!value)
;;                                            (if (string= "to" g!class-with-node)
;;                                                (nth 1 x)
;;                                                (nth 0 x)))))
;;                        (lookup-option g!map g!value ,classifier "deposit")
;;                        (lookup-option g!map g!value ,classifier "interface"))))
;;              (if g!user-value
;;                  (process-user-value g!user-value ,dot-attr g!value
;;                                      ,o!seq :element :edge)
;;                  (error "Unable to set edge function.  User value not set for ~s.~&"
;;                         ,classifier)))))
;;         ((string= "adjacent" g!value)
;;          (lambda (x)
;;            (let ((g!user-value
;;                    (case (round (fset:@ (fset:@ g!classifiers g!value)
;;                                         (if (string= "to" g!class-with-node)
;;                                             (nth 1 x)
;;                                             (nth 0 x))))
;;                      (0 (lookup-option g!map g!value ,classifier "origin"))
;;                      (1 (lookup-option g!map g!value ,classifier "adjacent"))
;;                      (t (lookup-option g!map g!value ,classifier "not-adjacent")))))
;;              (if g!user-value
;;                  (process-user-value g!user-value ,dot-attr g!value
;;                                      ,o!seq :element :edge)
;;                (error "Unable to set edge function.  User value not set for ~s.~&"
;;                       ,classifier)))))
;;         ((string= "reachable" g!value)
;;          (lambda (x)
;;            (let ((g!user-value
;;                    (case (round (fset:@ (fset:@ g!classifiers g!value)
;;                                         (if (string= "to" g!class-with-node)
;;                                             (nth 1 x)
;;                                             (nth 0 x))))
;;                      (0 (lookup-option g!map g!value ,classifier "origin"))
;;                      (1 (lookup-option g!map g!value ,classifier "adjacent"))
;;                      (2 (lookup-option g!map g!value ,classifier "reachable"))
;;                      (t (lookup-option g!map g!value ,classifier "not-reachable")))))
;;              (if g!user-value
;;                  (process-user-value g!user-value ,dot-attr g!value
;;                                      ,o!seq :element :edge)
;;                (error "Unable to set edge function.  User value not set for ~s.~&"
;;                       ,classifier)))))
;;         ((fset:contains? (fset:set "distance" "levels" "periods" "phases")
;;                          g!value)
;;          (lambda (x)
;;            (edge--process-matrix-value
;;             (fset:@ (fset:@ g!classifiers g!value)
;;                     (if (string= "to" g!class-with-node)
;;                         (nth 1 x)
;;                         (nth 0 x)))
;;             ,dot-attr
;;             g!value
;;             ,o!seq)))
;;         (t (error "Error: Unable to set edge function.~&"))))))

;; (lol:defmacro! <-dot-node (o!seq classifier dot-attr)
;;   `(let* ((g!sequence ,g!seq)
;;           (g!opt ,classifier)
;;           (g!attribute ,dot-attr)
;;           (g!cfg (archaeological-sequence-configuration ,g!seq))
;;           (g!classifiers (archaeological-sequence-classifiers ,g!seq))
;;           (g!value (when ,classifier (sequence-classifier g!cfg ,classifier)))
;;           (map (make-node-lookup-map g!cfg)))
;;      (if (or (not g!opt) (and g!opt (emptyp g!value))
;;              (not (fset:domain-contains? g!classifiers g!value)))
;;          (let ((g!y (process-user-value (lookup-option map g!attribute)
;;                                       g!attribute g!opt g!sequence)))
;;            (constantly (if (emptyp g!y) "" g!y)))
;;        (cond
;;         ((string= "units" g!value)
;;          (lambda (x)
;;            (let ((g!user-value
;;                    (if (= 0 (round (fset:@ (fset:@ g!classifiers g!value) x)))
;;                        (lookup-option map g!value g!opt "deposit")
;;                        (lookup-option map g!value g!opt "interface"))))
;;              (if (emptyp g!user-value)
;;                  (error "Unable to set node function.  User value not set for ~s.~&"
;;                         g!opt)
;;                  (let ((g!user-value-parsed
;;                          (parse-integer g!user-value :junk-allowed t)))
;;                    (process-user-value
;;                     (if g!user-value-parsed g!user-value-parsed g!user-value)
;;                     g!attribute g!value g!sequence :element :node))))))
;;         ((string= "adjacent" g!value)
;;          (lambda (x)
;;            (let ((g!user-value
;;                    (case (round (fset:@ (fset:@ g!classifiers g!value) x))
;;                      (0 (lookup-option map g!value g!opt "origin"))
;;                      (1 (lookup-option map g!value g!opt "adjacent"))
;;                      (t (lookup-option map g!value g!opt "not-adjacent")))))
;;              (if (emptyp g!user-value)
;;                  (error "Unable to set node function.  User value not set for ~s.~&"
;;                         g!opt)
;;                  (let ((g!user-value-parsed
;;                          (parse-integer g!user-value :junk-allowed t)))
;;                    (process-user-value
;;                     (if g!user-value-parsed g!user-value-parsed g!user-value)
;;                     g!attribute g!value g!sequence :element :node))))))
;;         ((string= "reachable" g!value)
;;          (lambda (x)
;;            (let ((g!user-value
;;                    (case (round (fset:@ (fset:@ g!classifiers g!value) x))
;;                      (0 (lookup-option map g!value g!opt "origin"))
;;                      (1 (lookup-option map g!value g!opt "adjacent"))
;;                      (2 (lookup-option map g!value g!opt "reachable"))
;;                      (t (lookup-option map g!value g!opt "not-reachable")))))
;;              (if (emptyp g!user-value)
;;                  (error "Unable to set node function.  User value not set for ~s.~&"
;;                         g!opt)
;;                  (let ((g!user-value-parsed
;;                          (parse-integer g!user-value :junk-allowed t)))
;;                    (process-user-value
;;                     (if g!user-value-parsed g!user-value-parsed g!user-value)
;;                     g!attribute g!value g!sequence :element :node))))))
;;         ((fset:contains? (fset:set "distance" "levels" "periods" "phases")
;;                          g!value)
;;          (lambda (x)
;;            (node--process-matrix-value (fset:@ (fset:@ g!classifiers g!value) x)
;;                                        g!attribute g!value g!sequence)))
;;         (t (error "Error: Unable to set node function.~&"))))))
