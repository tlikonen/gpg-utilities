;; Author: Teemu Likonen <tlikonen@iki.fi>
;;
;; License: Creative Commons CC0 (public domain dedication)
;; https://creativecommons.org/publicdomain/zero/1.0/legalcode

(defpackage #:count-steps
  (:use #:cl #:common)
  (:export #:main))

(in-package #:count-steps)

(defvar *program* "gpg-count-steps")

(defun print-usage ()
  (format t "~
Usage: ~A [options] [--] [from-key [to-key]]

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

  --max-steps=N
        Set the maximum certificate steps between keys to number
        N (positive integer). After that the search will give up. The
        default is ~D.

  --invalid
        Include revoked keys, expired keys, expired certificates and
        certificates for revoked user ids.

  -h, --help
        Print this help text.~%~%"
          *program*
          *shortest-path-max-steps*))

(defun main (&rest args)
  (let ((key1 nil)
        (key2 nil))

    (getopt args '((:help #\h)
                   (:help "help")
                   (:invalid "invalid")
                   (:max-steps "max-steps" :required)))

    (when (optionp :help)
      (print-usage)
      (return-from main))

    (validate-option-integer-arg :max-steps "--max-steps" #'plusp)

    (setf key1 (arguments 0)
          key2 (arguments 1))

    (loop :for key :in (list key1 key2)
          :if (and key
                   (not (and (= 40 (length key))
                             (every (lambda (char)
                                      (digit-char-p char 16))
                                    key))))
            :do (error 'invalid-arguments
                       :text (format nil "Key arguments must be ~
                        40-character key fingerprints.")))

    (when (and key1 key2 (equalp key1 key2))
      (error 'invalid-arguments
             :text "The FROM and TO keys can't be the same."))

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

    (when (and key1 (not (typep key1 'key)))
      (error 'key-not-found :key key1))

    (when (and key2 (not (typep key2 'key)))
      (error 'key-not-found :key key2))

    (let ((ht (make-hash-table)))
      (flet ((print-steps (key1 key2)
               (clrhash ht)
               (format t "~A ~A ~A~%" (fingerprint key1) (fingerprint key2)
                       (handler-case (study-levels key1 key2 ht)
                         (path-not-found () "-")))))

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
                         (print-steps key1 key2)))))))))
