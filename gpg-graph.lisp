;; Author: Teemu Likonen <tlikonen@iki.fi>
;;
;; No restrictions for use: this program is placed in the public domain.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

(defpackage #:graph
  (:use #:cl #:common)
  (:export #:main))

(in-package #:graph)

(defun print-usage ()
  (format t "~
Usage: gpg-graph [options] [--] [key1 ...]

Find connections between GnuPG keys based on certificates (key
signatures) and output data for Graphviz which can draw a web of trust
image. The arguments can be any valid references to GnuPG keys. See
gpg(1) manual for help on that topic.

Graphviz comes with tools like \"dot\", \"neato\", \"fdp\" etc. which
use different algorithms for drawing nodes and edges. Example:

  $ gpg-graph [key1 ...] | dot -Tpng > web-of-trust.png

Options:

  -h, --help    Print this help text.~%~%"))

(defun main (&rest args)
  (multiple-value-bind (options arguments unknown)
      (just-getopt-parser:getopt args '((:help #\h)
                                        (:help "help"))
                                 :error-on-unknown-option t
                                 :error-on-argument-not-allowed t)
    (declare (ignore unknown))
    (when (assoc :help options)
      (print-usage)
      (exit-program 0))
    (setf args arguments))

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
                                       "--" args)
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
               (setf (fingerprint key) (nth 9 fields)))

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
               (let ((cert-key (get-create-key (nth 4 fields)))
                     (cert-type (if (string= "sig" (nth 0 fields))
                                    'certificate
                                    'revocation)))
                 (add-certificates-from
                  key (make-instance
                       cert-type
                       :key cert-key
                       :created (parse-time-stamp (nth 5 fields))
                       :expires (parse-time-stamp (nth 6 fields))))
                 (add-certificates-for
                  cert-key (make-instance
                            cert-type
                            :key key
                            :created (parse-time-stamp (nth 5 fields))
                            :expires
                            (parse-time-stamp (nth 6 fields)))))))))

  (clean-all-keys)

  (format *error-output* " done.~%")
  (force-output *error-output*)

  (format t "~
digraph \"GnuPG key graph\" {
  overlap=scale;
  splines=true;
  node [shape=box];
")

  (loop :for key :being :each :hash-value :in *keys*
        :for user-id := (user-id key)
        :if user-id :do
          (print-graphviz-key-node key :indent 2)
          (loop :for cert :in (mapcar #'key (certificates-from key))
                :if (user-id cert) :do
                  (print-graphviz-edge cert key
                                       :indent 4
                                       :both (when (certificates-for-p key cert)
                                               (remove-certificates-from cert key)
                                               t))))

  (format t "}~%"))
