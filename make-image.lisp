(require 'asdf)

(asdf:initialize-source-registry
 (list :source-registry
       :ignore-inherited-configuration
       (list :directory *default-pathname-defaults*)))
(asdf:disable-output-translations)
(asdf:load-system "gpg-utilities")

(sb-ext:save-lisp-and-die
 "gpg-utilities"
 :executable t
 :toplevel #'start:start
 :save-runtime-options t
 :compression (if (member :sb-core-compression *features*) t))
