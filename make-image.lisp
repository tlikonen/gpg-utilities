(require 'asdf)

(asdf:clear-source-registry)
(setf asdf:*central-registry* (list *default-pathname-defaults*))
(asdf:disable-output-translations)
(asdf:load-system "gpg-utilities")

(sb-ext:save-lisp-and-die
 "gpg-utilities"
 :executable t
 :toplevel #'start:start
 :save-runtime-options t
 :compression (if (member :sb-core-compression *features*) t))
