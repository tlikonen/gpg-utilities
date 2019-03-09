(defpackage #:common
  (:use #:cl)
  (:export #:*gpg-program* #:*keys* #:key #:user-id #:fingerprint
           #:revoked
           #:expired
           #:key-ok #:certificates-from #:certificates-for
           #:certificate #:created #:expires #:revocation
           #:get-create-key
           #:clean-all-keys
           #:certificates-for-p
           #:add-certificates-from
           #:add-certificates-for
           #:remove-certificates-from
           #:split-colon-string
           #:string-replace
           #:escape-characters
           #:prepare-user-id
           #:parse-time-stamp
           #:split-fingerprint
           #:*shortest-path-max-steps*
           #:study-levels
           #:shortest-paths
           #:parse-command-line
           ))

(in-package #:common)

(defvar *gpg-program* "gpg")
(defvar *keys* (make-hash-table :test #'equal))

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

(defun key-ok (key)
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

(defun remove-unusable-certificates (certs)
  ;; Remove expired certificates or revocation certificates from CERTS
  ;; list. Does not modify the original list.
  (remove-if (lambda (cert)
               (or (typep cert 'revocation)
                   (time-stamp-expired-p (expires cert))))
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

(defun string-replace (string search replace)
  (assert (plusp (length search)) (search)
          "Can't search for zero length SEARCH string.")
  (with-output-to-string (out)
    (loop :with length-search := (length search)
          :for pos := 0 :then (+ find length-search)
          :for find := (search search string :start2 pos)
          :if find :do
            (princ (subseq string pos find) out)
            (princ replace out)
          :else :do
            (princ (subseq string pos) out)
            (loop-finish))))

(defun escape-characters (string esc-chars esc)
  (with-output-to-string (out)
    (loop :for char :across string
          :do (when (find char esc-chars)
                (princ esc out))
              (princ char out))))

(defun prepare-user-id (string)
  ;; This should actually decode C language string.
  (setf string (string-replace string "\\x3a" ":"))
  (escape-characters string "\\" #\\))

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

(defun time-stamp-expired-p (time)
  (if time (<= time (get-universal-time)) nil))

(defun split-fingerprint (fingerprint)
  (loop :for i :from 4 :upto (length fingerprint) :by 4
        :collect (subseq fingerprint (- i 4) i)))

(defvar *shortest-path-max-steps* 20)

(defun study-levels (from-key to-key &optional (hash-table (make-hash-table)))
  (let ((found-level nil))
    (labels
        ((levels (keys level)
           (unless (> level *shortest-path-max-steps*)
             (loop :for key :in keys
                   :do (when (key-ok key)
                         (setf (gethash key hash-table) level))
                       (when (eql key to-key)
                         (setf found-level level)
                         (setf (gethash key hash-table) level)))
             (unless found-level
               (loop :with next-keys
                     :for key :in keys
                     :do (loop :for cert :in (certificates-for key)
                               :for cert-key := (key cert)
                               :do (when (and (or (key-ok cert-key)
                                                  (eql cert-key to-key))
                                              (not (gethash cert-key
                                                            hash-table)))
                                     (push cert-key next-keys)))
                     :finally
                        (when next-keys
                          (levels next-keys (1+ level))))))))
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
                           (not (key-ok place))))
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

(defun parse-command-line (args)
  (loop :with help :with unknown
        :with arg
        :while args
        :if (setf arg (pop args)) :do

          (cond
            ((string= "--" arg)
             (loop-finish))

            ((and (> (length arg) 2)
                  (string= "--" (subseq arg 0 2)))
             (cond ((string= arg "--help")
                    (setf help t))
                   (t (push arg unknown))))

            ((and (> (length arg) 1)
                  (char= #\- (aref arg 0)))
             (loop :for option :across (subseq arg 1)
                   :do (case option
                         (#\h (setf help t))
                         (t (push (format nil "-~C" option) unknown)))))

            (t (push arg args)
               (loop-finish)))

        :finally
           (return
             (values
              (list :help help)
              args
              (delete-duplicates (nreverse unknown) :test #'string=)))))
