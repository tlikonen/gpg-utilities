;; Author: Teemu Likonen <tlikonen@iki.fi>
;;
;; License: Creative Commons CC0 (public domain dedication)
;; https://creativecommons.org/publicdomain/zero/1.0/legalcode

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
  (handler-bind
      ((just-getopt-parser:unknown-option
         (lambda (c)
           (format *error-output* "~A~%" c)
           (invoke-restart 'just-getopt-parser:skip-option)))
       (common:exit-program
         (lambda (c)
           (sb-ext:exit :code (common:code c))))
       (sb-int:simple-stream-error
         (lambda (c) (declare (ignore c))))
       (sb-sys:interactive-interrupt
         (lambda (c)
           (declare (ignore c))
           (terpri)
           (sb-ext:exit :code 1)))
       (serious-condition
         (lambda (c)
           (format *error-output* "~&~A~%" c)
           (sb-ext:exit :code 1))))

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
           (apply #'tofu:main (rest args)))
          ((equal command "graph")
           (apply #'graph:main (rest args)))
          ((equal command "cert-path")
           (apply #'cert-path:main (rest args)))
          ((equal command "count-steps")
           (apply #'count-steps:main (rest args)))
          (t (print-usage program))))

      (common:exit-program 0))))
