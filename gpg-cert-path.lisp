;; gpg-cert-path - Find the shortest certificate path(s) between two keys
;;
;; Author: Teemu Likonen <tlikonen@iki.fi>
;;
;; No restrictions for use: this program is placed in the public domain.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

(defpackage #:gpg-cert-path
  (:use #:cl #:common)
  (:export #:start))

(in-package #:gpg-cert-path)

(defvar *max-steps* 8)

(defun shortest-paths (from to)
  (let ((paths nil)
        (max-steps *max-steps*))
    (labels ((route (place path reject steps)
               (push place path)
               (cond ((> steps max-steps))
                     ((eql place to)
                      (setf max-steps (min max-steps steps))
                      (push (cons steps (reverse path)) paths))
                     ((and (not (eql place from))
                           (not (key-ok place))))
                     (t (loop :with current-certs-for
                                := (mapcar #'key (certificates-for place))
                              :with next-reject := (append current-certs-for reject)
                              :for next :in current-certs-for
                              :if (and (not (member next path))
                                       (not (member next reject)))
                                :do (route next path next-reject (1+ steps)))))))
      (route from nil nil 0)
      (when paths
        (mapcar #'rest (delete (reduce #'min paths :key #'first)
                               paths :key #'first :test-not #'=))))))

(defun main (key1 key2)
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
    (error "Give two different 40-character key fingerprints as arguments."))

  (setf key1 (string-upcase key1)
        key2 (string-upcase key2))

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

  (cond ((not (typep key1 'key))
         (error "The KEY1 not found in the keyring."))
        ((not (typep key2 'key))
         (error "The KEY2 not found in the keyring.")))

  (remove-old-certs)

  (let ((paths (shortest-paths key1 key2))
        (keys nil)
        (edges nil))

    (unless paths
      (format *error-output* "Couldn't find a path from KEY1 to KEY2 ~
                within maximum steps (~D).~%" *max-steps*)
      (force-output *error-output*))

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
          :do (format t "  \"~A\"~%    [label=\"~A\\l~?\"~A];~%"
                      (fingerprint key) (user-id key)
                      (if (>= (length (user-id key)) 55)
                          "~{~A~^ ~}\\l"
                          "~{~A ~A ~A ~A ~A~^ ...\\l... ~}\\r")
                      (list (split-fingerprint (fingerprint key)))
                      (if (key-ok key) "" ", fontcolor=\"#aaaaaa\"")))

    (loop :for (key1 . key2) :in edges
          :do (format t "  \"~A\" -> \"~A\";~%"
                      (fingerprint key1) (fingerprint key2)))

    (format t "}~%")))

(defun start ()
  (handler-case (main (nth 1 sb-ext:*posix-argv*)
                      (nth 2 sb-ext:*posix-argv*))
    (sb-int:simple-stream-error ()
      nil)
    (sb-sys:interactive-interrupt ()
      (terpri))
    (serious-condition (c)
      (format *error-output* "~&~A~%" c))))
