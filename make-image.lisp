(require 'asdf)

(asdf:initialize-source-registry
 (list :source-registry
       :ignore-inherited-configuration
       (list :directory *default-pathname-defaults*)))
(asdf:disable-output-translations)
(asdf:load-system "gpg-utilities")
(when (nth 1 sb-ext:*posix-argv*)
  (setf common:*gpg-program* (nth 1 sb-ext:*posix-argv*)))

(sb-ext:save-lisp-and-die
 "gpg-utilities"
 :executable t
 :toplevel #'start:start
 :save-runtime-options t
 :compression (if (member :sb-core-compression *features*) t))
