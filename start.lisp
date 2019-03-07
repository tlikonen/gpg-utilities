(defpackage #:start
  (:use #:cl)
  (:export #:start))

(in-package #:start)

(defun start ()
  (let ((program (pathname-name (sb-ext:native-pathname
                                 (first sb-ext:*posix-argv*)))))
    (cond ((equal program "gpg-tofu") (gpg-tofu:start))
          ((equal program "gpg-graph") (gpg-graph:start))
          ((equal program "gpg-cert-path") (gpg-cert-path:start))
          ((equal program "gpg-count-steps") (gpg-count-steps:start))
          (t (format *error-output* "This program can only be called ~
                with names gpg-tofu, gpg-graph or gpg-cert-path.~%")))))
