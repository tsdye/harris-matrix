;;; hm-cli.lisp

;; Copyright (C) Thomas Dye 2019

;; Licensed under the Gnu Public License Version 3 or later

(in-package #:hm)

(defun unknown-option (condition)
  (format t "warning: ~s option is unknown!~%" (opts:option condition))
  (invoke-restart 'opts:skip-option))

(defmacro when-option ((options opt) &body body)
  `(let ((it (getf ,options ,opt)))
     (when it
       ,@body)))

(defun hm-run ()
  (opts:define-opts
    (:name :help
     :description "print this help text"
     :short #\h
     :long "help")
    (:name :verbose
     :description "run silently"
     :short #\q
     :long "quiet")
    (:name :sequence-display
     :description "image file format for sequence graph"
     :short #\g
     :long "seq-disp"
     :arg-parser #'identity
     :meta-var "EXT")
    (:name :chronology-display
     :description "image file format for the chronology graph"
     :short #\c
     :long "chron-disp"
     :arg-parser #'identity
     :meta-var "EXT")
    (:name :sequence-command
     :description "command to display the sequence graph"
     :short #\G
     :long "seq-cmd"
     :arg-parser #'identity
     :meta-var "CMD")
    (:name :chronology-command
     :description "command to display the chronology graph"
     :short #\C
     :long "chron-cmd"
     :arg-parser #'identity
     :meta-var "CMD")
    (:name :draw-sequence
     :description "flag to suppress sequence graph display"
     :short #\s
     :long "no-seq")
    (:name :draw-chronology
     :description "flag to suppress chronology graph display"
     :short #\S
     :long "no-chron")
    (:name :delete-sequence
     :description "delete sequence graph file"
     :short #\d
     :long "del-seq")
    (:name :delete-chronology
     :description "delete chronology graph file"
     :short #\D
     :long "del-chron"))

  (let ((configuration-file)
        (configuration-others)
        (verbose t)
        (sequence-display "png")
        (chronology-display "png")
        (sequence-command "xdg-open")
        (chronology-command "xdg-open")
        (draw-sequence t)
        (draw-chronology t)
        (delete-sequence)
        (delete-chronology))
    (multiple-value-bind (options free-args)
        (handler-case
            (handler-bind ((opts:unknown-option #'unknown-option))
              (opts:get-opts))
          (opts:missing-arg (condition)
            (format t "fatal: option ~s needs an argument!~%"
                    (opts:option condition)))
          (opts:arg-parser-failed (condition)
            (format t "fatal: cannot parse ~s as argument of ~s~%"
                    (opts:raw-arg condition)
                    (opts:option condition)))
          (opts:missing-required-option (con)
            (format t "fatal: ~a~%" con)
            (opts:exit 1)))
      ;; Here all options are checked independently, it's trivial to code any
      ;; logic to process them.
      (when-option (options :help)
        (opts:describe
         :prefix "Write Graphviz dot files for archaeological sequence diagrams and Bayesian chronological models"
         :suffix ""
         :usage-of "hm"
         :args     "<configuration file(s)>")
        (opts:exit 0))
      (when-option (options :verbose)
        (format t "Running in quiet mode~%")
        (setf verbose nil))
      (when-option (options :sequence-display)
        (if (image-file-format-p it)
            (progn
              (format t "Using ~a format for the sequence graph.~%" it)
              (setf sequence-display it))
            (progn
              (format t "Image file format '~a' is not recognized.~&" it)
              (opts:exit 1))))
      (when-option (options :chronology-display)
        (if (image-file-format-p it)
            (progn
              (format t "Using ~a format for the chronology graph.~%" it)
              (setf chronology-display it))
            (progn
              (format t "Image file format '~a' is not recognized.~&" it)
              (opts:exit 1))))
      (when-option (options :sequence-command)
        (format t "Using ~a command to display the sequence graph.~%" it)
        (setf sequence-command it))
      (when-option (options :chronology-command)
        (format t "Using ~a command to display the chronology graph.~%" it)
        (setf chronology-command it))
      (when-option (options :draw-sequence)
        (format t "Suppressing display of the sequence graph.~%")
        (setf draw-sequence nil))
      (when-option (options :draw-chronology)
        (format t "Suppressing display of the chronology graph.~%")
        (setf draw-chronology nil))
      (when-option (options :delete-sequence)
        (format t "Preparing to delete sequence graph file.~%")
        (setf delete-sequence t))
      (when-option (options :delete-chronology)
        (format t "Preparing to delete chronology graph file.~%")
        (setf delete-chronology t))
      (if free-args
          (progn
            (format t "Initializing with: ~{~a~^, ~}.~%" free-args)
            (setf configuration-file (first free-args))
            (setf configuration-others (rest free-args)))
          (progn
            (format t "Error: no configuration file specified.~%")
            (opts:exit 1))))
    (run-project configuration-file
                 configuration-others :verbose verbose
                                      :sequence-display sequence-display
                                      :chronology-display chronology-display
                                      :sequence-cmd sequence-command
                                      :chronology-cmd chronology-command
                                      :draw-sequence draw-sequence
                                      :draw-chronology draw-chronology
                                      :delete-sequence delete-sequence
                                      :delete-chronology delete-chronology)))
(defun hm-main ()
  (handler-case
      (hm-run)
    (sb-sys:interactive-interrupt ()
      (progn
        (format *error-output* "Aborted by request.~&")
        (opts:exit)))))
