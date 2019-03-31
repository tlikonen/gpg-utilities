;; Author: Teemu Likonen <tlikonen@iki.fi>
;;
;; License: Creative Commons CC0 (public domain dedication)
;; https://creativecommons.org/publicdomain/zero/1.0/legalcode

(defpackage #:common
  (:use #:cl)
  (:export #:*gpg-program* #:*keys*
           #:optionp
           #:arguments
           #:getopt-store
           #:exit-program #:code
           #:key #:user-id #:fingerprint
           #:revoked
           #:expired
           #:certificates-from #:certificates-for
           #:certificate #:created #:expires #:revocation
           #:validp
           #:valid-display-p
           #:get-create-key
           #:clean-all-keys
           #:certificates-for-p
           #:add-certificates-from
           #:add-certificates-for
           #:remove-certificates-from
           #:split-colon-string
           #:unescape-user-id
           #:parse-time-stamp
           #:split-fingerprint
           #:*shortest-path-max-steps*
           #:study-levels
           #:shortest-paths
           #:print-graphviz-key-node
           #:print-graphviz-edge
           #:collect-key-data
           ))

(in-package #:common)

(defvar *gpg-program* "gpg")
(defvar *keys* (make-hash-table :test #'equal))
(defvar *options* nil)
(defvar *arguments* nil)
(defvar *shortest-path-max-steps* 20)
(defvar *graphviz-invalid-color* "#999999")

(define-condition exit-program ()
  ((code :reader code :initarg :code :type integer)))

(defun exit-program (exit-code)
  (error 'exit-program :code exit-code))

(defun optionp (option-symbol)
  (assoc option-symbol *options*))

(defun arguments (&optional position)
  (if position
      (nth position *arguments*)
      *arguments*))

(defun getopt-store (args spec)
  (handler-bind ((just-getopt-parser:unknown-option
                   (lambda (c)
                     (format *error-output* "~A~%" c)
                     (invoke-restart 'just-getopt-parser:skip-option))))

    (multiple-value-bind (options arguments unknown)
        (just-getopt-parser:getopt args spec
                                   :error-on-unknown-option t
                                   :error-on-argument-not-allowed t)

      (setf *options* options)
      (setf *arguments* arguments)

      (when (and unknown (not (optionp :help)))
        (format *error-output* "Use option \"-h\" for help.~%")
        (exit-program 1)))))

(defclass key ()
  ((user-id :accessor user-id :initform nil)
   (fingerprint :accessor fingerprint :initform nil)
   (revoked :accessor revoked :initform nil)
   (expired :accessor expired :initform nil)
   (certificates-from :accessor certificates-from :initform nil)
   (cerfificates-for :accessor certificates-for :initform nil)))

(defclass certificate ()
  ((key :reader key :initarg :key)
   (created :reader created :initarg :created)
   (expires :reader expires :initarg :expires)))

(defclass revocation (certificate) nil)

(defun validp (key)
  (or (optionp :invalid)
      (and (not (revoked key))
           (not (expired key)))))

(defun valid-display-p (key)
  (and (not (revoked key))
       (not (expired key))))

(defun get-create-key (key-id)
  (or (gethash key-id *keys*)
      (setf (gethash key-id *keys*) (make-instance 'key))))

(defun remove-old-certificates (certs)
  ;; Remove old certificates from CERTS list and keep only the latest
  ;; for each key. Does not modify the original list.
  (delete-duplicates
   (sort (copy-list certs)
         (lambda (cert1 cert2)
           (or (string< (fingerprint (key cert1))
                        (fingerprint (key cert2)))
               (and (string= (fingerprint (key cert1))
                             (fingerprint (key cert2)))
                    (> (created cert1) (created cert2))))))
   :from-end t :key #'key))

(defun time-stamp-expired-p (time)
  (if time (<= time (get-universal-time)) nil))

(defun remove-unusable-certificates (certs)
  ;; Remove expired certificates or revocation certificates from CERTS
  ;; list. Does not modify the original list.
  (remove-if (lambda (cert)
               (or (typep cert 'revocation)
                   (and (not (optionp :invalid))
                        (time-stamp-expired-p (expires cert)))))
             certs))

(defun clean-all-keys ()
  (loop :for key :being :each :hash-value :in *keys*
        :do (setf (certificates-for key)
                  (remove-unusable-certificates
                   (remove-old-certificates (certificates-for key))))
            (setf (certificates-from key)
                  (remove-unusable-certificates
                   (remove-old-certificates (certificates-from key))))))

(defun certificates-for-p (key cert-key)
  (member cert-key (certificates-for key) :key #'key))

(defun add-certificates-from (key cert)
  (pushnew cert (certificates-from key)))

(defun add-certificates-for (key cert)
  (pushnew cert (certificates-for key)))

(defun remove-certificates-from (key cert-key)
  (setf (certificates-from key)
        (remove cert-key (certificates-from key) :key #'key)))

(defun split-colon-string (string)
  (loop :with items
        :with start := 0
        :for end := (position #\: string :start start)
        :do (push (subseq string start end) items)
            (if end
                (setf start (1+ end))
                (loop-finish))
        :finally (return (nreverse items))))

(defun unescape-user-id (string)
  (remove-if-not #'graphic-char-p (c-strings:unescape-c-string string)))

(defun parse-time-stamp (time-stamp)
  ;; Parse GnuPG time stamp and return Lisp universal time. If the
  ;; original time stamp is missing return nil.
  (cond ((or (string= time-stamp "")
             (string= time-stamp "0"))
         nil)
        ((every #'digit-char-p time-stamp)
         (+ (parse-integer time-stamp)
            2208988800))                ;1970-01-01T00:00:00Z
        ((and (>= (length time-stamp) 15)
              (char= #\T (aref time-stamp 8))
              (every #'digit-char-p (subseq time-stamp 0 8))
              (every #'digit-char-p (subseq time-stamp 9 15)))
         (let ((year (parse-integer (subseq time-stamp 0 4)))
               (month (parse-integer (subseq time-stamp 4 6)))
               (day (parse-integer (subseq time-stamp 6 8)))
               (hour (parse-integer (subseq time-stamp 9 11)))
               (min (parse-integer (subseq time-stamp 11 13)))
               (sec (parse-integer (subseq time-stamp 13 15))))
           (encode-universal-time sec min hour day month year 0)))))

(defun split-fingerprint (fingerprint)
  (loop :for i :from 4 :upto (length fingerprint) :by 4
        :collect (subseq fingerprint (- i 4) i)))

(defun study-levels (from-key to-key &optional (hash-table (make-hash-table)))
  (let ((found-level nil))
    (labels
        ((levels (keys level)
           (unless (> level *shortest-path-max-steps*)
             (loop :for key :in keys
                   :do (when (validp key)
                         (setf (gethash key hash-table) level))
                       (when (eql key to-key)
                         (setf found-level level)
                         (setf (gethash key hash-table) level)))

             (unless found-level
               (loop :with next-keys
                     :for key :in keys
                     :do (loop :for cert :in (certificates-for key)
                               :for cert-key := (key cert)
                               :do (when (and (or (validp cert-key)
                                                  (eql cert-key to-key))
                                              (not (gethash cert-key
                                                            hash-table)))
                                     (push cert-key next-keys)))
                     :finally
                        (when next-keys
                          (levels (delete-duplicates next-keys)
                                  (1+ level))))))))

      (setf (gethash from-key hash-table) 0)
      (levels (list from-key) 0)
      found-level)))

(defun shortest-paths (from to)
  (let ((paths nil)
        ;; (calls 0)
        (studied (make-hash-table)))

    (labels ((routes (place path steps)
               ;; (incf calls)
               (push place path)
               (cond ((> steps (gethash place studied)))
                     ((eql place to)
                      (push (reverse path) paths))
                     ((and (not (eql place from))
                           (not (validp place))))
                     (t
                      (loop :for cert :in (certificates-for place)
                            :for next-key := (key cert)
                            :do (when (gethash next-key studied)
                                  (routes next-key path (1+ steps))))))))

      (let ((steps (study-levels from to studied)))
        (when steps (routes from nil 0))
        ;; (format *error-output* "Function calls: ~D~%" calls)
        (if paths
            (values paths steps)
            (values nil nil))))))

(defun escape-characters (string esc-chars esc)
  (with-output-to-string (out)
    (loop :for char :across string
          :do (when (find char esc-chars)
                (princ esc out))
              (princ char out))))

(defun escape-graphviz-label (string)
  (escape-characters string "\\" #\\))

(defun print-graphviz-key-node (key &key (indent 0)
                                      (stream *standard-output*))
  (format stream "~V,2T\"~A\"~%~V,2T  [label=\"~A\\l~?\"~A];~%"
          indent (fingerprint key) indent
          (escape-graphviz-label (user-id key))
          (if (>= (length (user-id key)) 55)
              "~{~A~^ ~}\\l"
              "~{~A ~A ~A ~A ~A~^ ...\\l... ~}\\r")
          (list (split-fingerprint (fingerprint key)))
          (if (valid-display-p key)
              ""
              (format nil ", fontcolor=\"~A\", color=\"~:*~A\"style=dashed"
                      *graphviz-invalid-color*))))

(defun print-graphviz-edge (from-key to-key &key (indent 0) both
                                              (stream *standard-output*))
  (format stream "~V,2T\"~A\" -> \"~A\" [dir=~A~A];~%"
          indent (fingerprint from-key) (fingerprint to-key)
          (if both "both" "forward")
          (let ((cert (loop :for cert :in (certificates-for from-key)
                            :if (eql (key cert) to-key) :return cert)))
            (if (or (time-stamp-expired-p (expires cert))
                    (not (valid-display-p from-key))
                    (not (valid-display-p to-key)))
                (format nil ", color=\"~A\", style=dashed"
                        *graphviz-invalid-color*)
                ""))))

(defun collect-key-data (stream)
  (loop
    :with key-id :with key :with expect
    :for line := (read-line stream nil)
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
              (string= "uid" (nth 0 fields)))
         (if (and (plusp (length (nth 1 fields)))
                  (char= #\r (aref (nth 1 fields) 0))
                  (not (optionp :invalid)))
             (setf expect '(:uid))
             (setf expect '(:sig :uid)))
         (unless (user-id key)
           (setf (user-id key) (unescape-user-id (nth 9 fields)))))

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
