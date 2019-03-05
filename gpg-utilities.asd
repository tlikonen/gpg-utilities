(defsystem :gpg-utilities
  :description "Utilities for GnuPG security tool"
  :author "Teemu Likonen <tlikonen@iki.fi>"
  :licence "Public domain"
  :components ((:file "common")
               (:file "gpg-tofu" :depends-on ("common"))
               (:file "gpg-graph" :depends-on ("common"))
               (:file "gpg-cert-path" :depends-on ("common"))
               (:file "start"
                :depends-on ("gpg-tofu" "gpg-graph" "gpg-cert-path"))))
