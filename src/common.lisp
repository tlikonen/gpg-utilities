;; Author: Teemu Likonen <tlikonen@iki.fi>
;;
;; License: Creative Commons CC0 (public domain dedication)
;; https://creativecommons.org/publicdomain/zero/1.0/legalcode

(defpackage #:common
  (:use #:cl)
  (:export #:*gpg-program*
           #:*keys*
           #:optionp
           #:arguments
           #:getopt
           #:invalid-arguments
           #:key-not-found
           #:key
           #:user-ids
           #:fingerprint
           #:creator-key
           #:certificates-from
           #:validp
           #:some-valid-certificates-p
           #:only-invalid-certificates-p
           #:split-colon-string
           #:unescape-user-id
           #:parse-time-stamp
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
(defvar *graphviz-invalid-color* "#888888")

(define-condition invalid-arguments (error)
  nil
  (:report "Invalid arguments. Use option \"-h\" for help."))

(define-condition key-not-found (error)
  ((key :type string :reader key-argument :initarg :key))
  (:report (lambda (condition stream)
             (format stream "Key ~A not found from the keyring."
                     (key-argument condition)))))

(defun optionp (option-symbol)
  (assoc option-symbol *options*))

(defun arguments (&optional position)
  (if position
      (nth position *arguments*)
      *arguments*))

(defun getopt (args spec)
  (handler-bind ((just-getopt-parser:unknown-option
                   (lambda (c)
                     (format *error-output* "~A~%" c)
                     (invoke-restart 'just-getopt-parser:skip-option))))

    (multiple-value-bind (options arguments unknown)
        (just-getopt-parser:getopt args spec
                                   :prefix-match-long-options t
                                   :error-on-unknown-option t
                                   :error-on-ambiguous-option t
                                   :error-on-argument-not-allowed t)

      (setf *options* options)
      (setf *arguments* arguments)

      (when (and unknown (not (optionp :help)))
        (error 'invalid-arguments)))))

(defclass key ()
  ((user-ids :accessor user-ids :initform nil)
   (fingerprint :accessor fingerprint :initform nil)
   (revoked :accessor revoked :initform nil)
   (expired :accessor expired :initform nil)
   (cerfificates-for :accessor certificates-for :initform nil)
   (cerfificates-from :accessor certificates-from :initform nil)))

(defclass user-id ()
  ((id-string :reader id-string :initarg :id-string)
   (key :reader key :initarg :key)
   (revoked :accessor revoked :initform nil)
   (expired :accessor expired :initform nil)
   (certificates-from :accessor certificates-from :initform nil)))

(defclass primary-user-id (user-id) nil)

(defun primary-user-id (key)
  (loop :for uid :in (user-ids key)
        :if (typep uid 'primary-user-id)
          :return (id-string uid)))

(defclass certificate ()
  ((creator-key :reader creator-key :initarg :creator-key)
   (created :reader created :initarg :created)
   (expires :reader expires :initarg :expires)
   (target-uid :reader target-uid :initarg :target-uid)))

(defclass revocation (certificate) nil)

(defun time-stamp-expired-p (time)
  (if time (<= time (get-universal-time)) nil))

(defun target-key (cert)
  (key (target-uid cert)))

(defgeneric validp (object))

(defmethod validp ((key key))
  (and (not (revoked key))
       (not (expired key))))

(defmethod validp ((uid user-id))
  (not (revoked uid)))

(defmethod validp ((cert certificate))
  (and (not (time-stamp-expired-p (expires cert)))
       (validp (creator-key cert))
       (validp (target-uid cert))
       (validp (target-key cert))))

(defun get-create-key (key-id)
  (or (gethash key-id *keys*)
      (setf (gethash key-id *keys*) (make-instance 'key))))

(defun some-valid-certificates-p (from-key to-key)
  (some (lambda (cert)
          (and (eql from-key (creator-key cert))
               (validp cert)))
        (certificates-from to-key)))

(defun only-invalid-certificates-p (from-key to-key)
  (some (lambda (cert)
          (eql from-key (creator-key cert)))
        (remove-if #'validp (certificates-from to-key))))

(defun add-certificates-for (key cert)
  (pushnew cert (certificates-for key)))

(defun add-certificates-from (user-id cert)
  (pushnew cert (certificates-from user-id)))

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
  (remove-if-not #'graphic-char-p (string-io:unescape-c-string string)))

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

(defun study-levels (from-key to-key hash-table)
  (let ((found-level nil))
    (labels
        ((levels (keys level)
           (unless (> level *shortest-path-max-steps*)
             (loop :for key :in keys
                   :do (when (or (optionp :invalid)
                                 (validp key))
                         (setf (gethash key hash-table) level))
                       (when (eql key to-key)
                         (setf found-level level)
                         (setf (gethash key hash-table) level)))

             (unless found-level
               (loop :with next-keys
                     :for key :in keys
                     :do (loop :for cert :in (certificates-for key)
                               :for target-key := (target-key cert)
                               :do (when (and (or (optionp :invalid)
                                                  (and (validp cert)
                                                       (validp target-key)))
                                              (not (gethash target-key
                                                            hash-table)))
                                     (push target-key next-keys)))
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
                           (not (optionp :invalid))
                           (not (validp place))))
                     (t
                      (loop :for cert :in (certificates-for place)
                            :for next-key := (target-key cert)
                            :do (when (and (or (optionp :invalid)
                                               (validp cert))
                                           (gethash next-key studied))
                                  (routes next-key path (1+ steps))))))))

      (let ((steps (study-levels from to studied)))
        (when steps (routes from nil 0))
        ;; (format *error-output* "Function calls: ~D~%" calls)
        (if paths
            (values paths steps)
            (error "Couldn't find a path between the keys.~%~
        Maybe there is no connection or at least not in this keyring~%~
        or within the maximum of ~D steps." *shortest-path-max-steps*))))))

(defun escape-graphviz-label (string)
  (string-io:escape-characters string '(#\" #\\) #\\))

(defun print-graphviz-key-node (key)

  (let ((uids (if (optionp :all-user-ids)
                  (loop :for uid :in (user-ids key)
                        :if (or (optionp :invalid)
                                (validp uid))
                          :collect (escape-graphviz-label (id-string uid)))
                  (list (escape-graphviz-label (primary-user-id key))))))

    (format t "\"~A\"~%[label=\"\\~%~{~A\\l~^\\~%~}~A\"~A];~%~%"
            (fingerprint key)
            uids
            (if (optionp :fingerprint)
                (format nil "\\~%~?"
                        (if (>= (reduce #'max uids :key #'length) 55)
                            "~{~A~^ ~}\\l"
                            "~{~A ~A ~A ~A ~A~^ ...\\l... ~}\\l")
                        (list (split-fingerprint (fingerprint key))))
                "")
            (if (validp key)
                ""
                (format nil ",~%fontcolor=\"~A\", color=\"~:*~A\", style=dashed"
                        *graphviz-invalid-color*)))))

(defun print-graphviz-edge (from-key to-key &key two-way)
  (format t "\"~A\"~%-> \"~A\"~%[dir=~A~A];~%~%"
          (fingerprint from-key) (fingerprint to-key)
          (if two-way "both" "forward")
          (if (or (not (some-valid-certificates-p from-key to-key))
                  (and two-way
                       (not (some-valid-certificates-p to-key from-key))))
              (format nil ", color=\"~A\", style=dashed"
                      *graphviz-invalid-color*)
              "")))

(defun clean-all-keys ()
  (let ((cert-hash (make-hash-table)))
    (flet ((clean-cert-list (cert-list key-predicate)
             (clrhash cert-hash)

             (loop
               :with update
               :for cert :in cert-list
               :for cert-key := (funcall key-predicate cert)
               :for old-cert := (gethash cert-key cert-hash)

               :do (setf update nil)
                   (cond
                     ((not old-cert)
                      (setf update t))
                     ((and (validp cert)
                           (not (validp old-cert)))
                      (setf update t))
                     ((and (validp old-cert)
                           (not (validp cert))))
                     ((> (created cert)
                         (created old-cert))
                      (setf update t)))

                   (when update
                     (setf (gethash cert-key cert-hash) cert))

               :finally
                  (return (loop :for cert :being :each :hash-value
                                  :in cert-hash
                                :unless (typep cert 'revocation)
                                  :collect cert)))))

      (loop
        :for key :being :each :hash-value :in *keys* :do

          (setf (certificates-for key)
                (clean-cert-list (certificates-for key) #'target-key))

          (loop :for uid :in (user-ids key)
                :append (certificates-from uid) :into certs
                :finally
                   (setf (certificates-from key)
                         (clean-cert-list certs #'creator-key)))))))

(defun collect-key-data (stream)
  (loop
    :with key-id :with key :with uid :with expect
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
         (setf expect '(:sig :uid))
         (setf uid (make-instance
                    (if (null (user-ids key))
                        'primary-user-id
                        'user-id)
                    :id-string (unescape-user-id (nth 9 fields))
                    :key key))
         (push uid (user-ids key))
         (when (plusp (length (nth 1 fields)))
           (case (aref (nth 1 fields) 0)
             (#\r (setf (revoked uid) t))
             (#\e (setf (expired uid) t)))))

        ((string= "uat" (nth 0 fields))
         (setf expect '(:uid)))

        ((and (member :sig expect)
              (or (string= "sig" (nth 0 fields))
                  (string= "rev" (nth 0 fields)))
              (plusp (length (nth 1 fields)))
              (char= #\! (aref (nth 1 fields) 0))
              (string/= key-id (nth 4 fields)))
         (let* ((creator-key (get-create-key (nth 4 fields)))
                (cert (make-instance (if (string= "sig" (nth 0 fields))
                                         'certificate
                                         'revocation)
                                     :creator-key creator-key
                                     :created (parse-time-stamp (nth 5 fields))
                                     :expires (parse-time-stamp (nth 6 fields))
                                     :target-uid uid)))
           (add-certificates-from uid cert)
           (add-certificates-for creator-key cert)))))

  (loop :for key :being :each :hash-value :in *keys*
        :do (setf (user-ids key) (reverse (user-ids key))))

  (clean-all-keys))
