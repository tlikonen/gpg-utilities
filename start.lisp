(defpackage #:start
  (:use #:cl)
  (:export #:start))

(in-package #:start)

(defun start ()
  (let ((program (sb-ext:native-pathname (first sb-ext:*posix-argv*)))
        (args (rest sb-ext:*posix-argv*)))
    (setf program (concatenate 'string (pathname-name program)
                               (let ((type (pathname-type program)))
                                 (if type (concatenate 'string "." type) ""))))

    (cond
      ((equal program "gpg-tofu") (push "tofu" args))
      ((equal program "gpg-graph") (push "graph" args))
      ((equal program "gpg-cert-path") (push "cert-path" args))
      ((equal program "gpg-count-steps") (push "count-steps" args)))

    (let ((command (first args)))
      (cond
        ((equal command "tofu")
         (gpg-tofu:start (rest args)))
        ((equal command "graph")
         (gpg-graph:start (rest args)))
        ((equal command "cert-path")
         (gpg-cert-path:start (rest args)))
        ((equal command "count-steps")
         (gpg-count-steps:start (rest args)))
        (t
         (format t "~
Usage: ~A <subcommand> [options or arguments]

Subcommands: tofu, graph, cert-path and count-steps. A subcommand is
executed automatically if this program is started with any of the
following names: gpg-tofu, gpg-graph, gpg-cert-path or gpg-count-steps.

Type \"-h\" or \"--help\" as an option to print help on specific
subcommand." program))))))
