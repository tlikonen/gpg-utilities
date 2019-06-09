;; Author: Teemu Likonen <tlikonen@iki.fi>
;;
;; License: Creative Commons CC0 (public domain dedication)
;; https://creativecommons.org/publicdomain/zero/1.0/legalcode

(defpackage #:tofu
  (:use #:cl #:common)
  (:export #:main))

(in-package #:tofu)

(defvar *program* "gpg-tofu")

(defun format-time-stamp (universal-time)
  (local-time:format-timestring
   nil
   (local-time:universal-to-timestamp universal-time)
   :format '((:year 4) "-" (:month 2) "-" (:day 2) " "
             (:hour 2) ":" (:min 2) ":" (:sec 2) :gmt-offset-or-z)
   :timezone (if (optionp :utc)
                 local-time:+utc-zone+
                 local-time:*default-timezone*)))

(defun format-time-interval (seconds)
  (let* ((seconds-in-day #. (* 60 60 24))
         (days (round seconds seconds-in-day))
         (hours (round seconds 3600))
         (minutes (round seconds 60)))
    (cond
      ((and (<= 0 seconds)
            (< seconds 60))
       (format nil "~D second~:*~P" seconds))
      ((and (<= 0 minutes)
            (< minutes 60))
       (format nil "~D minute~:*~P" minutes))
      ((and (<= 1 hours)
            (< hours 24))
       (format nil "~D hour~:*~P" hours))
      ((and (<= 1 days)
            (< days 365))
       (format nil "~D day~:*~P" days))
      ((= 365 days)
       (format nil "1 year"))
      ((< 365 days)
       (let* ((seconds-in-year #. (* 60 60 24 365))
              (full-years (truncate seconds seconds-in-year))
              (days (round (- seconds (* full-years seconds-in-year))
                           seconds-in-day)))
         (cond ((= days 0)
                (format nil "~D years" full-years))
               ((= days 365)
                (format nil "~D years" (1+ full-years)))
               (t (format nil "~D year~:*~P ~D day~:*~P"
                          full-years days))))))))

(defun center-string (string width)
  (if (<= width (length string))
      string
      (let ((spaces (- width (length string))))
        (replace (make-string width :initial-element #\space)
                 string :start1 (ceiling spaces 2)))))

(defun format-validity (string &optional (width 0))
  (if (plusp (length string))
      (center-string (case (aref string 0)
                       (#\o "unknown")
                       (#\i "invalid")
                       (#\d "disabled")
                       (#\r "revoked")
                       (#\e "expired")
                       (#\- "unknown")
                       (#\q "undef")
                       (#\n "never")
                       (#\m "marginal")
                       (#\f "full")
                       (#\u "ultimate")
                       (#\s "special")
                       (t ""))
                     width)
      ""))

(defparameter *echo-buffer* (make-array 200 :element-type 'character
                                            :adjustable t
                                            :fill-pointer 0))

(defun echo (format &rest arguments)
  (loop :for c :across (apply #'format nil format arguments)
        :do (vector-push-extend c *echo-buffer*)))

(defun echo-buffer-length ()
  (length *echo-buffer*))

(defmacro with-echo-buffer (&body body)
  `(progn
     (setf (fill-pointer *echo-buffer*) 0)
     (multiple-value-prog1 (progn ,@body)
       (princ *echo-buffer*))))

(defun print-usage ()
  (format t "~
Usage: ~A [options] [--] [key1 ...]

Print \"trust on first use\" (TOFU) statistics for GnuPG keys. The
arguments can be any valid references to GnuPG keys. See gpg(1) manual
for help on that topic.

Options:

  --utc
        Display time stamps in UTC time.

  -h, --help
        Print this help text.~%~%" *program*))

(defun main (&rest args)
  (getopt args '((:help #\h)
                 (:help "help")
                 (:utc "utc")))

  (when (optionp :help)
    (print-usage)
    (exit-program 0))

  (with-open-stream
      (gpg (sb-ext:process-output
            (sb-ext:run-program *gpg-program*
                                (list* "--batch" "--no-tty"
                                       "--with-tofu-info"
                                       "--with-colons"
                                       "--list-keys"
                                       "--" (arguments))
                                :search t :wait nil
                                :output :stream
                                :error nil)))

    (loop :with expect :with want-empty-line
          :for line := (read-line gpg nil)
          :for fields := (if line (split-colon-string line))
          :while line :do

            (cond
              ((string= "pub" (nth 0 fields))
               (setf expect '(:fpr))
               (when want-empty-line (format t "~%"))
               (setf want-empty-line t))

              ((and (member :fpr expect)
                    (string= "fpr" (nth 0 fields)))
               (setf expect '(:uid))
               (format t "pub ~A~%" (nth 9 fields)))

              ((string= "sub" (nth 0 fields))
               (setf expect nil))

              ((and (member :uid expect)
                    (string= "uid" (nth 0 fields)))
               (setf expect '(:tfs :uid))
               (format t "uid [~8A] ~A~%"
                       (format-validity (nth 1 fields) 8)
                       (unescape-user-id (nth 9 fields))))

              ((and (member :tfs expect)
                    (string= "tfs" (nth 0 fields))
                    (string= "1" (nth 1 fields)))
               (setf expect '(:uid))
               (let ((tofu-validity (parse-integer (nth 2 fields)))
                     (signature-count (parse-integer (nth 3 fields)))
                     (encryption-count (parse-integer (nth 4 fields)))
                     (policy (nth 5 fields))
                     (signature-first (parse-time-stamp (nth 6 fields)))
                     (signature-last (parse-time-stamp (nth 7 fields)))
                     (encryption-first (parse-time-stamp (nth 8 fields)))
                     (encryption-last (parse-time-stamp (nth 9 fields))))

                 (format t "    TOFU validity: (~D/4) ~A, TOFU policy: ~A~%"
                         tofu-validity
                         (case tofu-validity
                           (0 "conflict")
                           (1 "no history")
                           (2 "little history")
                           (3 "enough history for basic trust")
                           (4 "a lot of history for trust")
                           (t "unknown"))
                         policy)

                 (when (plusp signature-count)
                   (with-echo-buffer
                     (echo "    ~D signature~:*~P" signature-count)
                     (when (and signature-first signature-last)
                       (cond
                         ((plusp (- signature-last signature-first))
                          (echo " in ~A" (format-time-interval
                                          (- signature-last
                                             signature-first)))
                          (let ((indent (+ 2 (echo-buffer-length))))
                            (echo ", first: ~A~%"
                                  (format-time-stamp signature-first))
                            (echo "~VTlast:  ~A" indent
                                  (format-time-stamp signature-last))))
                         ((= signature-last signature-first)
                          (echo " in ~A" (format-time-stamp signature-last))))
                       (echo "~%"))))

                 (when (plusp encryption-count)
                   (with-echo-buffer
                     (echo "    ~D encryption~:*~P" encryption-count)
                     (when (and encryption-first encryption-last)
                       (cond
                         ((plusp (- encryption-last encryption-first))
                          (echo " in ~A" (format-time-interval
                                          (- encryption-last
                                             encryption-first)))
                          (let ((indent (+ 2 (echo-buffer-length))))
                            (echo ", first: ~A~%"
                                  (format-time-stamp encryption-first))
                            (echo "~VTlast:  ~A" indent
                                  (format-time-stamp encryption-last))))
                         ((= encryption-last encryption-first)
                          (echo " in ~A" (format-time-stamp encryption-last))))
                       (echo "~%"))))))))))
