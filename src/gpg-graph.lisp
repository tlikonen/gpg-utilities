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

  --invalid     Include revoked keys, expired keys, expired certificates
                and certificates for revoked user ids.

  --two-way     Print two-way arrows between cross-certified keys.

  -h, --help    Print this help text.~%~%"
          *program*))

(defun main (&rest args)
  (getopt args '((:help #\h)
                 (:help "help")
                 (:invalid "invalid")
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
    :for key :being :each :hash-value :in *keys*
    :for user-id := (user-id key)
    :if (and user-id
             (or (optionp :invalid)
                 (validp key)))
      :do (print-graphviz-key-node key :indent 2)
          (loop
            :for cert-key :in (mapcar #'key (certificates-from key))
            :if (and (user-id cert-key)
                     (or (optionp :invalid)
                         (valid-certificate-p cert-key key)))
              :do (print-graphviz-edge
                   cert-key key
                   :indent 4
                   :both
                   (when (and (optionp :two-way)
                              (or (and (valid-certificate-p
                                        cert-key key)
                                       (valid-certificate-p
                                        key cert-key))
                                  (and (not (valid-certificate-p
                                             cert-key key))
                                       (not (valid-certificate-p
                                             key cert-key)))))
                     (remove-certificates-from cert-key key)
                     t))))

  (format t "}~%"))
