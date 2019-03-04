(defpackage #:common
  (:use #:cl)
  (:export #:*keys* #:key #:user-id #:fingerprint
           #:key-ok #:certificates-from #:certificates-for
           #:certificate #:date #:revocation
           #:get-create-key #:only-latest-certs
           #:certificates-for-p
           #:list-of-certificates-from
           #:add-certificates-from
           #:add-certificates-for
           #:remove-certificates-from
           #:split-colon-string
           #:string-replace
           #:escape-characters
           #:prepare-user-id
           #:parse-time-stamp
           #:date-not-expired-p
           #:split-fingerprint
           ))

(in-package #:common)

(defvar *keys* (make-hash-table :test #'equal))

(defclass key ()
  ((user-id :accessor user-id :initform nil)
   (fingerprint :accessor fingerprint :initform nil)
   (key-ok :accessor key-ok :initform t)
   (certificates-from :accessor certificates-from :initform nil)
   (cerfificates-for :accessor certificates-for :initform nil)))

(defclass certificate ()
  ((key :accessor key :initarg :key)
   (date :accessor date :initarg :date)))

(defclass revocation (certificate) nil)

(defun get-create-key (key-id)
  (or (gethash key-id *keys*)
      (setf (gethash key-id *keys*) (make-instance 'key))))

(defun only-latest-certs (certs)
  ;; Remove old certificates and keep only the latest for each key. If
  ;; the key's latest certificate is a revocation certificate remove
  ;; that too.
  (delete-if (lambda (item)
               (typep item 'revocation))
             (delete-duplicates
              (sort (copy-list certs)
                    (lambda (cert1 cert2)
                      (or (string< (fingerprint (key cert1))
                                   (fingerprint (key cert2)))
                          (and (string= (fingerprint (key cert1))
                                        (fingerprint (key cert2)))
                               (> (date cert1) (date cert2))))))
              :from-end t :key #'key)))

(defun certificates-for-p (key cert-key)
  (member cert-key (certificates-for key) :key #'key))

(defun list-of-certificates-from (key)
  (mapcar #'key (certificates-from key)))

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

(defun date-not-expired-p (expire)
  (let ((now (get-universal-time))
        (date (parse-time-stamp expire)))
    (if date (> date now) t)))

(defun split-fingerprint (fingerprint)
  (loop :for i :from 4 :upto (length fingerprint) :by 4
        :collect (subseq fingerprint (- i 4) i)))
