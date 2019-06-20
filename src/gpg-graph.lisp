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

    gpg-graph ... | dot -Tpng > web-of-trust.png

Options:

  --fingerprint
        Print keys' full fingerprint.

  --all-user-ids
        Print all user ids.

  --two-way
        Print two-way arrows between cross-certified keys.

  --invalid
        Include revoked keys, expired keys, revoked user ids, expired
        certificates and certificates for revoked user ids.

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
    (return-from main))

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

  (format *error-output* " done.~%")
  (force-output *error-output*)

  (format t "~
digraph \"GnuPG key graph\" {
overlap=false;
splines=true;
node [shape=box];~%~%")

  (loop
    :with cross := (make-hash-table)
    :for key :being :each :hash-value :in *keys*
    :if (and (user-ids key)
             (or (optionp :invalid)
                 (validp key)))

      :do (print-graphviz-key-node key)
          (loop
            :for cert :in (certificates-from key)
            :for from-key := (creator-key cert)
            :if (and (user-ids from-key)
                     (not (find from-key (gethash key cross)))
                     (or (optionp :invalid)
                         (some-valid-certificates-p from-key key)))

              :do (cond
                    ((and (optionp :two-way)
                          (or (and (some-valid-certificates-p from-key key)
                                   (some-valid-certificates-p key from-key))
                              (and (only-invalid-certificates-p from-key key)
                                   (only-invalid-certificates-p key from-key))))
                     (push key (gethash from-key cross))
                     (print-graphviz-edge from-key key :two-way t))
                    (t (print-graphviz-edge from-key key)))))

  (format t "}~%"))
