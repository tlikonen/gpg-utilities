(defsystem "gpg-utilities"
  :description "Utilities for GnuPG security tool"
  :author "Teemu Likonen <tlikonen@iki.fi>"
  :licence "Creative Commons CC0 (public domain dedication)"
  :components
  ((:file "gpg-tofu" :depends-on ("common"))
   (:file "gpg-graph" :depends-on ("common"))
   (:file "gpg-cert-path" :depends-on ("common"))
   (:file "gpg-count-steps" :depends-on ("common"))
   (:file "start" :depends-on ("common"
                               "gpg-tofu" "gpg-graph"
                               "gpg-cert-path"
                               "gpg-count-steps"))
   (:file "common" :depends-on ("just-getopt-parser" "c-strings"))
   (:file "just-getopt-parser")
   (:file "c-strings")))
