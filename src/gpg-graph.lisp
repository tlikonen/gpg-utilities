;; Author: Teemu Likonen <tlikonen@iki.fi>
;;
;; License: Creative Commons CC0 (public domain dedication)
;; https://creativecommons.org/publicdomain/zero/1.0/legalcode

(defpackage #:graph
  (:use #:cl #:common)
  (:export #:main))

(in-package #:graph)

(defvar *program* "gpg-graph")

(defun print-usage ()
  (format t "~
Usage: ~A [options] [--] [key1 ...]

Find connections between GnuPG keys based on certificates (key
signatures) and output data for Graphviz which can draw a web of trust
image. The arguments can be any valid references to GnuPG keys. See
gpg(1) manual for help on that topic.

Graphviz comes with tools like \"dot\", \"neato\", \"fdp\" etc. which
use different algorithms for drawing nodes and edges. Example:

  $ gpg-graph [key1 ...] | dot -Tpng > web-of-trust.png

Options:

  --invalid

        Include revoked keys, expired keys, revoked user ids, expired
        certificates and certificates for revoked user ids.

  --fingerprint

        Print keys' full fingerprint.

  --all-user-ids

        Print all user ids.

  --two-way

        Print two-way arrows between cross-certified keys.

  -h, --help

        Print this help text.~%~%" *program*))

(defun main (&rest args)
  (getopt args '((:help #\h)
                 (:help "help")
                 (:invalid "invalid")
                 (:fingerprint "fingerprint")
                 (:all-user-ids "all-user-ids")
                 (:two-way "two-way")))

  (when (optionp :help)
    (print-usage)
    (exit-program 0))

  (clrhash *keys*)

  (format *error-output* "Reading data from GnuPG...")
  (force-output *error-output*)

  (with-open-stream
      (gpg (sb-ext:process-output
            (sb-ext:run-program *gpg-program*
                                (list* "--batch" "--no-tty"
                                       "--with-colons"
                                       "--with-fingerprint"
                                       "--check-signatures"
                                       "--" (arguments))
                                :search t :wait nil
                                :output :stream
                                :error nil)))

    (collect-key-data gpg))

  (clean-all-keys)

  (format *error-output* " done.~%")
  (force-output *error-output*)

  (format t "~
digraph \"GnuPG key graph\" {
  overlap=scale;
  splines=true;
  node [shape=box];
")

  (loop
    :with cross := (make-hash-table)
    :for key :being :each :hash-value :in *keys*
    :if (and (user-id key)
             (or (optionp :invalid)
                 (validp key)))

      :do (print-graphviz-key-node key :indent 2)
          (loop
            :for cert :in (certificates-from key)
            :for from-key := (creator-key cert)
            :if (and (user-id from-key)
                     (not (find from-key (gethash key cross)))
                     (or (optionp :invalid)
                         (valid-certificate-p from-key key)))

              :do (print-graphviz-edge
                   from-key key
                   :indent 4
                   :both
                   (when (and (optionp :two-way)
                              (valid-certificate-p from-key key)
                              (valid-certificate-p key from-key))
                     (push key (gethash from-key cross))
                     t))))

  (format t "}~%"))
