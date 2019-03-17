(defsystem :gpg-utilities
  :description "Utilities for GnuPG security tool"
  :author "Teemu Likonen <tlikonen@iki.fi>"
  :licence "Creative Commons CC0 (public domain dedication)"
  :components ((:file "common")
               (:file "just-getopt-parser")
               (:file "gpg-tofu"
                :depends-on ("common" "just-getopt-parser"))
               (:file "gpg-graph"
                :depends-on ("common" "just-getopt-parser"))
               (:file "gpg-cert-path"
                :depends-on ("common" "just-getopt-parser"))
               (:file "gpg-count-steps"
                :depends-on ("common" "just-getopt-parser"))
               (:file "start"
                :depends-on ("common" "gpg-tofu" "gpg-graph" "gpg-cert-path"
                                      "gpg-count-steps"
                                      "just-getopt-parser"))))
