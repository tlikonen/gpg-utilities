;; gpg-count-steps - Count certificate steps between gpg keys
;;
;; Author: Teemu Likonen <tlikonen@iki.fi>
;;
;; No restrictions for use: this program is placed in the public domain.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

(defpackage #:gpg-count-steps
  (:use #:cl #:common)
  (:export #:main))

(in-package #:gpg-count-steps)

(defun print-usage ()
  (format t "~
Usage: gpg-count-steps [options] [--] [from-key [to-key]]

Count the steps of the shortest certificate path between two keys in the
keyring. If no arguments are given test all keys between each other (can
take some time on large keyrings). If one key argument is given count
certificate steps from that key to all other keys. If two keys are given
count steps just between those keys. Both arguments, if given at all,
must be 40-character key fingerprints.

The output consists of lines with three fields:

 1. The fingerprint of the from key.
 2. The fingerprint of the to key.
 3. The number of steps between the keys (or \"-\" if connection wasn't
    found).

Options:

  -h, --help    Print this help text.~%~%"))

(defun main (&rest args)
  (let ((key1 nil)
        (key2 nil))

    (multiple-value-bind (options arguments unknown)
        (parse-command-line args)
      (loop :for u :in unknown
            :do (format *error-output* "Unknown option \"~A\".~%" u))
      (when (getf options :help)
        (print-usage)
        (sb-ext:exit :code 0))
      (setf key1 (nth 0 arguments)
            key2 (nth 1 arguments)))

    (loop :for key :in (list key1 key2)
          :if (and key
                   (not (and (= 40 (length key))
                             (every (lambda (char)
                                      (or (digit-char-p char)
                                          (find (char-upcase char) "ABCDEF")))
                                    key))))
            :do (error "Usage: gpg-count-steps [from-key [to-key]]~%~
                (Arguments are 40-character key fingerprints.)"))

    (when (and key1 key2 (equalp key1 key2))
      (error "The FROM and TO keys can't be the same."))

    (clrhash *keys*)

    (with-open-stream
        (gpg (sb-ext:process-output
              (sb-ext:run-program "gpg" (list "--batch" "--no-tty"
                                              "--with-colons"
                                              "--with-fingerprint"
                                              "--check-signatures")
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
                      (string/= key-id (nth 4 fields))
                      (date-not-expired-p (nth 6 fields)))
                 (add-certificates-for
                  (get-create-key (nth 4 fields))
                  (make-instance (if (string= "sig" (nth 0 fields))
                                     'certificate
                                     'revocation)
                                 :key key
                                 :date (parse-time-stamp (nth 5 fields))))))))

    (cond ((stringp key1)
           (error "The FROM key not found in the keyring."))
          ((stringp key2)
           (error "The TO key not found in the keyring.")))

    (remove-old-certs)

    (flet ((print-steps (key1 key2)
             (multiple-value-bind (paths steps)
                 (shortest-paths key1 key2)
               (declare (ignore paths))
               (format t "~A ~A ~D~%" (fingerprint key1) (fingerprint key2)
                       (or steps "-")))))

      (cond ((and key1 key2)
             (print-steps key1 key2))
            ((and key1 (not key2))
             (loop :for key2 :being :each :hash-value :in *keys*
                   :unless (eql key1 key2) :do
                     (print-steps key1 key2)))
            (t
             (loop :for key1 :being :each :hash-value :in *keys* :do
               (loop :for key2 :being :each :hash-value :in *keys*
                     :unless (eql key1 key2) :do
                       (print-steps key1 key2))))))))
