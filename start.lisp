(defpackage #:start
  (:use #:cl)
  (:export #:start))

(in-package #:start)

(defun print-usage (program)
  (format t "~
Usage: ~A <subcommand> [options or arguments]

Subcommands: tofu, graph, cert-path and count-steps. A subcommand is
executed automatically if this program is started with any of the
following names: gpg-tofu, gpg-graph, gpg-cert-path or gpg-count-steps.

Option \"-h\" or \"--help\" for a subcommand prints help on that
specific subcommand.~%~%" program))

(defun start ()
  (handler-case
      (let ((program (sb-ext:native-pathname (first sb-ext:*posix-argv*)))
            (args (rest sb-ext:*posix-argv*)))
        (setf program (concatenate 'string (pathname-name program)
                                   (let ((type (pathname-type program)))
                                     (if type
                                         (concatenate 'string "." type)
                                         ""))))

        (cond
          ((equal program "gpg-tofu") (push "tofu" args))
          ((equal program "gpg-graph") (push "graph" args))
          ((equal program "gpg-cert-path") (push "cert-path" args))
          ((equal program "gpg-count-steps") (push "count-steps" args)))

        (let ((command (first args)))
          (cond
            ((equal command "tofu")
             (apply #'gpg-tofu:main (rest args)))
            ((equal command "graph")
             (apply #'gpg-graph:main (rest args)))
            ((equal command "cert-path")
             (apply #'gpg-cert-path:main (rest args)))
            ((equal command "count-steps")
             (apply #'gpg-count-steps:main (rest args)))
            (t (print-usage program))))

        (common:exit-program 0))

    (common:exit-program (c)
      (sb-ext:exit :code (common:code c)))
    (sb-int:simple-stream-error () nil)
    (sb-sys:interactive-interrupt ()
      (terpri)
      (sb-ext:exit :code 1))
    (serious-condition (c)
      (format *error-output* "~&~A~%" c)
      (sb-ext:exit :code 1))))
