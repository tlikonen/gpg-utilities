;; Author: Teemu Likonen <tlikonen@iki.fi>
;;
;; License: Creative Commons CC0 (public domain dedication)
;; https://creativecommons.org/publicdomain/zero/1.0/legalcode

(defpackage #:cert-path
  (:use #:cl #:common)
  (:export #:main))

(in-package #:cert-path)

(defvar *program* "gpg-cert-path")

(defun print-usage ()
  (format t "~
Usage: ~A [options] [--] <from-key> <to-key>

Find the shortest certificate path(s) between two GnuPG keys. The output
is data for Graphviz which can draw an image of certificate path. Both
arguments must be 40-character key fingerprints. By default revoked and
expired keys are accepted only at the endpoints of the path.

Graphviz comes with tools like \"dot\", \"neato\", \"fdp\" etc. which
use different algorithms for drawing nodes and edges. Example:

  $ gpg-cert-path <from-key> <to-key> | dot -Tpng > trust-path.png

Options:

  --invalid     Accept revoked keys, certificates for revoked user ids,
                expired keys and expired certificates.

  -h, --help    Print this help text.~%~%"
          *program*))

(defun main (&rest args)
  (let ((key1 nil)
        (key2 nil))

    (getopt-store args '((:help #\h)
                         (:help "help")
                         (:invalid "invalid")))

    (when (optionp :help)
      (print-usage)
      (exit-program 0))

    (setf key1 (arguments 0)
          key2 (arguments 1))

    (unless (and (stringp key1)
                 (stringp key2)
                 (= 40 (length key1) (length key2))
                 (string/= key1 key2)
                 (every (lambda (char)
                          (digit-char-p char 16))
                        key1)
                 (every (lambda (char)
                          (digit-char-p char 16))
                        key2))
      (error "Invalid arguments. See \"-h\" for help."))

    (clrhash *keys*)

    (format *error-output* "Reading data from GnuPG...")
    (force-output *error-output*)

    (with-open-stream
        (gpg (sb-ext:process-output
              (sb-ext:run-program *gpg-program*
                                  (list "--batch" "--no-tty"
                                        "--with-colons"
                                        "--with-fingerprint"
                                        "--check-signatures")
                                  :search t :wait nil
                                  :output :stream
                                  :error nil)))

      (collect-key-data gpg))

    (clean-all-keys)

    (format *error-output* " done.~%")
    (force-output *error-output*)

    (loop :for key :being :each :hash-value :in *keys*
          :for fingerprint := (fingerprint key)
          :while (or (not (typep key1 'key))
                     (not (typep key2 'key)))
          :do (cond ((equalp key1 fingerprint)
                     (setf key1 key))
                    ((equalp key2 fingerprint)
                     (setf key2 key))))

    (cond ((stringp key1)
           (error "The FROM key not found in the keyring."))
          ((stringp key2)
           (error "The TO key not found in the keyring.")))

    (let ((paths (multiple-value-bind (paths steps)
                     (shortest-paths key1 key2)
                   (when steps
                     (format *error-output* "Number of steps: ~D~%"
                             steps))
                   paths))
          (keys nil)
          (edges nil))

      (unless paths
        (format *error-output* "Couldn't find a path between the keys.~%~
        Maybe there is no connection or at least not in this keyring~%~
        or within the maximum steps (~D).~%" *shortest-path-max-steps*)
        (exit-program 0))

      (loop :for path :in paths
            :do (loop :for (key . rest) :on path
                      :do (push key keys)
                          (when (first rest)
                            (push (cons key (first rest)) edges)))

            :finally
               (setf keys (delete-duplicates (list* key1 key2 keys))
                     edges (delete-duplicates edges :test #'equal)))

      (format t "~
digraph \"GnuPG certificate path\" {
  overlap=scale;
  splines=true;
  node [shape=box];
")

      (loop :for key :in keys
            :do (print-graphviz-key-node key :indent 2))

      (loop :for (key1 . key2) :in edges
            :do (print-graphviz-edge key1 key2 :indent 2))

      (format t "}~%"))))
