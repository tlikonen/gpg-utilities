(require 'asdf)

(asdf:clear-source-registry)
(setf asdf:*central-registry* (list *default-pathname-defaults*))
(asdf:load-system "gpg-utilities")

(sb-ext:save-lisp-and-die
 "gpg-utilities"
 :executable t
 :toplevel
 (lambda ()
   (let ((program (pathname-name (first sb-ext:*posix-argv*))))
     (cond ((equal program "gpg-tofu") (gpg-tofu:start))
           ((equal program "gpg-graph") (gpg-graph:start))
           ((equal program "gpg-cert-path") (gpg-cert-path:start))
           (t (format *error-output* "This program can only be called ~
                with names gpg-tofu, gpg-graph or gpg-cert-path.~%")))))
 :save-runtime-options t
 :compression (if (member :sb-core-compression *features*) t))
