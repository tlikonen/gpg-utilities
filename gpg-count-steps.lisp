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
  (:export #:start))

(in-package #:gpg-count-steps)

(defvar *max-steps* 20)

(defun shortest-paths (from to)
  (let ((paths nil)
        (max-steps *max-steps*)
        (studied (make-hash-table)))

    (labels ((route (place path steps)
               (push place path)
               (cond ((> steps max-steps))
                     ((> steps (gethash place studied)))
                     ((eql place to)
                      (setf max-steps (min max-steps steps))
                      (push (cons steps (reverse path)) paths))
                     ((and (not (eql place from))
                           (not (key-ok place))))
                     (t
                      (let ((certs-for
                              (mapcar #'key (certificates-for place))))
                        (loop :for next :in certs-for
                              :for std := (gethash next studied)
                              :do (setf (gethash next studied)
                                        (if std
                                            (min std (1+ steps))
                                            (1+ steps))))
                        (loop :for next :in certs-for
                              :do (route next path (1+ steps))))))))

      (setf (gethash from studied) 0)
      (route from nil 0)
      (if paths
          (let ((steps (reduce #'min paths :key #'first)))
            (values (mapcar #'rest (delete steps paths
                                           :key #'first :test-not #'=))
                    steps))
          (values nil nil)))))

(defun main (&optional key1 key2)
  (loop :for key :in (list key1 key2)
        :if (and key
                 (not (and (= 40 (length key))
                           (every (lambda (char)
                                    (or (digit-char-p char)
                                        (find (char-upcase char) "ABCDEF")))
                                  key))))
          :do (error "Usage: [from-key [to-key]]"))

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
                     (print-steps key1 key2)))))))

(defun start ()
  (handler-case (main (nth 1 sb-ext:*posix-argv*)
                      (nth 2 sb-ext:*posix-argv*))
    (sb-int:simple-stream-error ()
      nil)
    (sb-sys:interactive-interrupt ()
      (terpri))
    (serious-condition (c)
      (format *error-output* "~&~A~%" c))))
