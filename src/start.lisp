;; Author: Teemu Likonen <tlikonen@iki.fi>
;;
;; License: Creative Commons CC0 (public domain dedication)
;; https://creativecommons.org/publicdomain/zero/1.0/legalcode

(defpackage #:start
  (:use #:cl)
  (:export #:start))

(in-package #:start)

(defun start (command)
  (handler-bind
      ((common:exit-program
         (lambda (c)
           (sb-ext:exit :code (common:code c))))
       (sb-int:simple-stream-error
         (lambda (c)
           (declare (ignore c))
           (sb-ext:exit :code 1)))
       (sb-sys:interactive-interrupt
         (lambda (c)
           (declare (ignore c))
           (sb-ext:exit :code 1)))
       (serious-condition
         (lambda (c)
           (format *error-output* "~&~A~%" c)
           (sb-ext:exit :code 1))))

    (let ((args (rest sb-ext:*posix-argv*)))
      
      (cond
        ((equal command "gpg-tofu")
         (apply #'tofu:main args))
        ((equal command "gpg-graph")
         (apply #'graph:main args))
        ((equal command "gpg-cert-path")
         (apply #'cert-path:main args))
        ((equal command "gpg-count-steps")
         (apply #'count-steps:main args))
        (t (error "Invalid argument for START function.")))

      (common:exit-program 0))))
