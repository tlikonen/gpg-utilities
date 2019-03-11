;; Author: Teemu Likonen <tlikonen@iki.fi>
;;
;; No restrictions for use: this program is placed in the public domain.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

(defpackage #:tofu
  (:use #:cl #:common)
  (:export #:main))

(in-package #:tofu)

(defun format-user-id (string)
  ;; This should actually decode C language string.
  (string-replace string "\\x3a" ":"))

(defun format-time-stamp (universal-time)
  (multiple-value-bind (second minute hour date month year
                        day daylight-p zone)
      (decode-universal-time universal-time 0)
    (declare (ignore day daylight-p zone))
    (format nil "~4,'0D-~2,'0D-~2,'0D ~2,'0D:~2,'0D:~2,'0DZ"
            year month date hour minute second)))

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
         (format nil "~D year~:*~P ~D day~:*~P" full-years days))))))

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

(defun echo (format &rest arguments)
  (apply #'format t format arguments))

(defun print-usage ()
  (format t "~
Usage: gpg-tofu [options] [--] [key1 ...]

Print \"trust on first use\" (TOFU) statistics for GnuPG keys. The
arguments can be any valid references to GnuPG keys. See gpg(1) manual
for help on that topic.

Options:

  -h, --help    Print this help text.~%~%"))

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

(defun main (&rest args)
  (multiple-value-bind (options arguments unknown)
      (parse-command-line args)
    (loop :for u :in unknown
          :do (format *error-output* "Unknown option \"~A\".~%" u))
    (when (getf options :help)
      (print-usage)
      (exit-program 0))
    (setf args arguments))

  (with-open-stream
      (gpg (sb-ext:process-output
            (sb-ext:run-program *gpg-program*
                                (list* "--batch" "--no-tty"
                                       "--with-tofu-info"
                                       "--with-colons"
                                       "--list-keys"
                                       "--" args)
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
               (when want-empty-line (echo "~%"))
               (setf want-empty-line t))

              ((and (member :fpr expect)
                    (string= "fpr" (nth 0 fields)))
               (setf expect '(:uid))
               (echo "pub ~A~%" (nth 9 fields)))

              ((string= "sub" (nth 0 fields))
               (setf expect nil))

              ((and (member :uid expect)
                    (string= "uid" (nth 0 fields)))
               (setf expect '(:tfs :uid))
               (echo "uid [~8A] ~A~%"
                     (format-validity (nth 1 fields) 8)
                     (format-user-id (nth 9 fields))))

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

                 (echo "    TOFU validity: (~D/4) ~A, TOFU policy: ~A~%"
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
                   (echo "    ~D signature~:*~P" signature-count)
                   (when (and signature-first signature-last)
                     (cond
                       ((plusp (- signature-last signature-first))
                        (echo " in ~A" (format-time-interval
                                        (- signature-last signature-first)))
                        (echo ", first: ~A"
                              (format-time-stamp signature-first))
                        (echo ", last: ~A"
                              (format-time-stamp signature-last)))
                       ((= signature-last signature-first)
                        (echo " in ~A" (format-time-stamp signature-last))))
                     (echo "~%")))

                 (when (plusp encryption-count)
                   (echo "    ~D encryption~:*~P" encryption-count)
                   (when (and encryption-first encryption-last)
                     (cond
                       ((plusp (- encryption-last encryption-first))
                        (echo " in ~A"
                              (format-time-interval
                               (- encryption-last encryption-first)))
                        (echo ", first: ~A"
                              (format-time-stamp encryption-first))
                        (echo ", last: ~A"
                              (format-time-stamp encryption-last)))
                       ((= encryption-last encryption-first)
                        (echo " in ~A"
                              (format-time-stamp encryption-last))))
                     (echo "~%")))))))))
