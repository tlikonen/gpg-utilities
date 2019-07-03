;; Author: Teemu Likonen <tlikonen@iki.fi>
;;
;; License: Creative Commons CC0 (public domain dedication)
;; https://creativecommons.org/publicdomain/zero/1.0/legalcode

(defpackage #:tofu
  (:nicknames #:gpg-tofu)
  (:use #:cl #:common)
  (:export #:main #:print-usage))

(in-package #:tofu)

(defvar *program* "gpg-tofu")

(defun format-time-stamp (universal-time)
  (local-time:format-timestring
   nil
   (local-time:universal-to-timestamp universal-time)
   :format '((:year 4) "-" (:month 2) "-" (:day 2) "T"
             (:hour 2) ":" (:min 2) ":" (:sec 2) :gmt-offset-or-z)
   :timezone (if (optionp :utc)
                 local-time:+utc-zone+
                 local-time:*default-timezone*)))

(defun format-time-duration (time1 time2)
  (let* ((duration (date-time:decoded-time-duration time1 time2))
         (y (date-time:years duration))
         (m (date-time:months duration))
         (d (date-time:days duration))
         (h (date-time:hours duration))
         (min (date-time:minutes duration))
         (s (date-time:seconds duration)))

    (cond
      ((= 0 y m d h min)
       (format nil "~D second~:*~P (~A)" s duration))
      ((= 0 y m d h)
       (format nil "~D minute~:*~P ~D second~:*~P (~A)" min s duration))
      ((= 0 y m d)
       (format nil "~D hour~:*~P ~D minute~:*~P (~A)" h min duration))
      ((= 0 y m)
       (format nil "~D day~:*~P ~D hour~:*~P (~A)" d h duration))
      ((= 0 y)
       (format nil "~D month~:*~P ~D day~:*~P (~A)" m d duration))
      ((= 0 m)
       (format nil "~D year~:*~P ~D day~:*~P (~A)" y d duration))
      (t (format nil "~D year~:*~P ~D month~:*~P ~D day~:*~P (~A)" y m d
                 duration)))))

(defun center-string (string width)
  (if (<= width (length string))
      string
      (replace (make-string width :initial-element #\space)
               string :start1 (ceiling (- width (length string)) 2))))

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

(defun print-usage ()
  (format t "~
Usage: ~A [options] [--] [key1 ...]

Print \"trust on first use\" (TOFU) statistics for GnuPG keys. The
arguments can be any valid references to GnuPG keys. See gpg(1) manual
for help on that topic.

Options:

  --utc
        Display time stamps in UTC time. (The default is local time
        zone.)

  -h, --help
        Print this help text.~%~%" *program*))

(defun main (&rest args)
  (getopt args '((:help #\h)
                 (:help "help")
                 (:utc "utc")))

  (when (optionp :help)
    (print-usage)
    (return-from main))

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

                 (format t "~4,2TTOFU validity: (~D/4) ~A, TOFU policy: ~A~%"
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
                   (format t "~4,2T~D signature~:*~P " signature-count)
                   (when (and signature-first signature-last)
                     (cond
                       ((plusp (- signature-last signature-first))
                        (format t "in ~A~%~6,2T~A/~A"
                                (format-time-duration
                                 signature-first signature-last)
                                (format-time-stamp signature-first)
                                (format-time-stamp signature-last)))
                       ((= signature-last signature-first)
                        (format t "in ~A" (format-time-stamp signature-last))))
                     (format t "~%")))

                 (when (plusp encryption-count)
                   (format t "~4,2T~D encryption~:*~P " encryption-count)
                   (when (and encryption-first encryption-last)
                     (cond
                       ((plusp (- encryption-last encryption-first))
                        (format t "in ~A~%~6,2T~A/~A"
                                (format-time-duration
                                 encryption-first encryption-last)
                                (format-time-stamp encryption-first)
                                (format-time-stamp encryption-last)))
                       ((= encryption-last encryption-first)
                        (format t "in ~A" (format-time-stamp
                                           encryption-last))))
                     (format t "~%")))))))))
