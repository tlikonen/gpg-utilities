;; Author: Teemu Likonen <tlikonen@iki.fi>
;;
;; License: Creative Commons CC0 (public domain dedication)
;; https://creativecommons.org/publicdomain/zero/1.0/legalcode

(defpackage #:cert-path
  (:use #:cl #:common)
  (:export #:main))

(in-package #:cert-path)

(defun print-usage ()
  (format t "~
Usage: gpg-cert-path [options] [--] <from-key> <to-key>

Find the shortest certificate path(s) between two GnuPG keys. The output
is data for Graphviz which can draw an image of certificate path. Both
arguments must be 40-character key fingerprints. By default revoked and
expired keys are accepted only at the endpoints of the path.

Graphviz comes with tools like \"dot\", \"neato\", \"fdp\" etc. which
use different algorithms for drawing nodes and edges. Example:

  $ gpg-cert-path <from-key> <to-key> | dot -Tpng > trust-path.png

Options:

  --revoked     Accept revoked keys.

  --expired     Accept expired keys.

  -h, --help    Print this help text.~%~%"))

(defun main (&rest args)
  (let ((key1 nil)
        (key2 nil))

    (multiple-value-bind (options arguments)
        (just-getopt-parser:getopt args '((:help #\h)
                                          (:help "help")
                                          (:revoked "revoked")
                                          (:expired "expired"))
                                   :error-on-unknown-option t
                                   :error-on-argument-not-allowed t)
      (when (assoc :help options)
        (print-usage)
        (exit-program 0))
      (setf *options* options)
      (setf key1 (nth 0 arguments)
            key2 (nth 1 arguments)))

    (unless (and (stringp key1)
                 (stringp key2)
                 (= 40 (length key1) (length key2))
                 (string/= key1 key2)
                 (every (lambda (char)
                          (or (digit-char-p char)
                              (find (char-upcase char) "ABCDEF")))
                        key1)
                 (every (lambda (char)
                          (or (digit-char-p char)
                              (find (char-upcase char) "ABCDEF")))
                        key2))
      (error "Invalid arguments. See \"-h\" for help."))

    (setf key1 (string-upcase key1)
          key2 (string-upcase key2))

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

      (loop :with key-id :with key :with expect
            :for line := (read-line gpg nil)
            :for fields := (if line (split-colon-string line))

            :while line :do
              (cond
                ((string= "pub" (nth 0 fields))
                 (setf expect '(:fpr))
                 (setf key-id (nth 4 fields))
                 (setf key (get-create-key key-id))
                 (when (plusp (length (nth 1 fields)))
                   (case (aref (nth 1 fields) 0)
                     (#\r (setf (revoked key) t))
                     (#\e (setf (expired key) t)))))

                ((string= "sub" (nth 0 fields))
                 (setf expect nil))

                ((and (member :fpr expect)
                      (string= "fpr" (nth 0 fields)))
                 (setf expect '(:uid))
                 (setf (fingerprint key) (nth 9 fields))
                 (cond ((and (stringp key1)
                             (string= key1 (fingerprint key)))
                        (setf key1 key))
                       ((and (stringp key2)
                             (string= key2 (fingerprint key)))
                        (setf key2 key))))

                ((and (member :uid expect)
                      (string= "uid" (nth 0 fields))
                      (not (user-id key)))
                 (setf expect '(:sig))
                 (setf (user-id key) (prepare-user-id (nth 9 fields))))

                ((and (member :sig expect)
                      (or (string= "sig" (nth 0 fields))
                          (string= "rev" (nth 0 fields)))
                      (plusp (length (nth 1 fields)))
                      (char= #\! (aref (nth 1 fields) 0))
                      (string/= key-id (nth 4 fields)))
                 (add-certificates-for
                  (get-create-key (nth 4 fields))
                  (make-instance (if (string= "sig" (nth 0 fields))
                                     'certificate
                                     'revocation)
                                 :key key
                                 :created (parse-time-stamp (nth 5 fields))
                                 :expires
                                 (parse-time-stamp (nth 6 fields))))))))

    (clean-all-keys)

    (format *error-output* " done.~%")
    (force-output *error-output*)

    (cond ((not (typep key1 'key))
           (error "The FROM key not found in the keyring."))
          ((not (typep key2 'key))
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
