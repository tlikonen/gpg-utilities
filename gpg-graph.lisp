;; gpg-graph - Draw web of trust graphs of a GnuPG keyring
;;
;; Author: Teemu Likonen <tlikonen@iki.fi>
;;
;; No restrictions for use: this program is placed in the public domain.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

(defpackage #:gpg-graph
  (:use #:cl #:common)
  (:export #:main))

(in-package #:gpg-graph)

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
      (parse-command-line args)
    (loop :for u :in unknown
          :do (format *error-output* "Unknown option \"~A\".~%" u))
    (when (getf options :help)
      (print-usage)
      (sb-ext:exit :code 0))
    (setf args arguments))

  (clrhash *keys*)

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
                                :error *error-output*)))

    (loop :with key-id :with key :with expect
          :for line := (read-line gpg nil)
          :for fields := (if line (split-colon-string line))

          :while line :do
            (cond
              ((string= "pub" (nth 0 fields))
               (setf expect '(:fpr))
               (setf key-id (nth 4 fields))
               (setf key (get-create-key key-id))
               (when (and (plusp (length (nth 1 fields)))
                          (find (aref (nth 1 fields) 0) "re"))
                 (setf (key-ok key) nil)))

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
                    (string/= key-id (nth 4 fields))
                    (date-not-expired-p (nth 6 fields)))
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

  (remove-old-certs)

  (format t "~
digraph \"GnuPG key graph\" {
  overlap=scale;
  splines=true;
  node [shape=box];
")

  (loop :for key :being :each :hash-value :in *keys*
        :for user-id := (user-id key)
        :if user-id :do
          (format t "  \"~A\"~%    [label=\"~A\\l~?\"~A];~%"
                  (fingerprint key) user-id
                  (if (>= (length user-id) 55)
                      "~{~A~^ ~}\\l"
                      "~{~A ~A ~A ~A ~A~^ ...\\l... ~}\\r")
                  (list (split-fingerprint (fingerprint key)))
                  (if (key-ok key) "" ", fontcolor=\"#aaaaaa\""))

          (loop :for cert :in (mapcar #'key (certificates-from key))
                :if (user-id cert) :do
                  (format t "    \"~A\" -> \"~A\" [dir=~A];~%"
                          (fingerprint cert) (fingerprint key)
                          (if (certificates-for-p key cert)
                              (progn (remove-certificates-from cert key)
                                     "both")
                              "forward"))))

  (format t "}~%"))
