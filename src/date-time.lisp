;; Author: Teemu Likonen <tlikonen@iki.fi>
;;
;; License: Creative Commons CC0 (public domain dedication)
;; https://creativecommons.org/publicdomain/zero/1.0/legalcode

(defpackage #:date-time
  (:use #:cl)
  (:export #:decoded-time-duration
           #:duration
           #:seconds-total
           #:days-total
           #:years
           #:months
           #:days
           #:days-after-years
           #:hours
           #:minutes
           #:seconds
           #:leap-year-p))

(in-package #:date-time)

(defclass duration ()
  ((seconds-total :type (integer 0 *) :reader seconds-total
                  :initarg :seconds-total)
   (days-total :type (integer 0 *) :reader days-total :initarg :days-total)
   (years :type (integer 0 *) :reader years :initarg :years)
   (months :type (integer 0 11) :reader months :initarg :months)
   (days :type (integer 0 30) :reader days :initarg :days)
   (days-after-years :type (integer 0 365) :reader days-after-years
                     :initarg :days-after-years)
   (hours :type (integer 0 23) :reader hours :initarg :hours)
   (minutes :type (integer 0 59) :reader minutes :initarg :minutes)
   (seconds :type (integer 0 59) :reader seconds :initarg :seconds)))

(defun format-duration-iso (duration &optional stream)
  (format stream "P~4,'0D-~2,'0D-~2,'0DT~2,'0D:~2,'0D:~2,'0D"
          (years duration) (months duration) (days duration)
          (hours duration) (minutes duration) (seconds duration)))

(defmethod print-object ((object duration) stream)
  (cond ((and (not *print-readably*)
              (not *print-escape*))
         (format-duration-iso object stream))
        (*print-readably*
         (format stream "#.~S"
                 (list 'make-instance
                       ''duration
                       :seconds-total (seconds-total object)
                       :days-total (days-total object)
                       :years (years object)
                       :months (months object)
                       :days (days object)
                       :days-after-years (days-after-years object)
                       :hours (hours object)
                       :minutes (minutes object)
                       :seconds (seconds object))))
        (t (print-unreadable-object (object stream :type t :identity t)
             (format-duration-iso object stream)))))

(defun leap-year-p (year)
  (flet ((divisible-by (n)
           (= 0 (rem year n))))
    (and (divisible-by 4)
         (or (divisible-by 400)
             (not (divisible-by 100))))))

(defun increase-month (year month)
  (check-type year integer)
  (check-type month (integer 1 12))

  (incf month)
  (when (> month 12)
    (setf month 1)
    (incf year))
  (values year month))

(defun increase-date (year month date)
  (check-type year integer)
  (check-type month (integer 1 12))
  (check-type date (integer 1 31))

  (incf date)
  (when (or (and (find month '(1 3 5 7 8 10 12))
                 (> date 31))
            (and (find month '(4 6 9 11))
                 (> date 30))
            (and (= 2 month)
                 (leap-year-p year)
                 (> date 29))
            (and (= 2 month)
                 (not (leap-year-p year))
                 (> date 28)))
    (setf date 1)
    (setf (values year month) (increase-month year month)))

  (values year month date))

(defun decoded-time-duration (utime1 utime2)
  "Return decoded time duration between Common Lisp universal times
UTIME1 and UTIME2. The function returns a DURATION object which can be
queried with these methods:

    SECONDS-TOTAL, DAYS-TOTAL: The number of seconds and full days,
    respectively, between UTIME1 and UTIME2. The value is a non-negative
    integer.

    YEARS, MONTHS, DAYS: The number of years, months (0-11) or
    days (0-30), respectively.

    DAYS-AFTER-YEARS: The number of full days (0-355) passed after full
    years.

    HOURS, MINUTES, SECONDS: The number of hours (0-23), minutes (0-59)
    and seconds (0-59) after the full days count.

DURATION object's printed form (as in CL:PRINC) is an ISO 8601 duration
string.

The second return value (integer) tells which of the two time arguments
is the most recent time. The value is 1 if UTIME1 is the most recent.
The value is 2 if UTIME2 is the most recent. The value is 0 if both
arguments are exactly the same time.

Years are calculated the \"human way\" using calendar dates so that a
full year is either 365 or 366 days. For example, the time durations
2018-06-01/2019-06-01 and 2019-06-01/2020-06-01 are both exactly one
year and zero days. In reality the latter duration is one day longer
because of the leap day 2020-02-29. Use DAYS-TOTAL value if you need the
exact day count.

Month durations follow the same \"human\" principle and full months are
28-31 days. For example durations 2019-02-01/2019-03-01,
2020-02-01/2020-03-01, 2020-04-01/2020-05-01 and 2020-05-01/2020-06-01
are all exactly one month and zero days even though the day durations
are actually 28, 29, 30 and 31, respectively. Use DAYS-TOTAL or
DAYS-AFTER-YEARS values if you need more exact day count."

  (check-type utime1 (integer 0 *))
  (check-type utime2 (integer 0 *))

  (if (= utime1 utime2)
      (values (make-instance 'duration :seconds-total 0 :days-total 0
                                       :years 0 :months 0 :days 0
                                       :days-after-years 0
                                       :hours 0 :minutes 0 :seconds 0)
              0)

      (let* ((most-recent (if (> utime1 utime2) 1 2))
             (u1 (min utime1 utime2))
             (u2 (max utime1 utime2))
             (seconds-per-day 86400)
             (seconds-total (- u2 u1))
             (days-total (truncate seconds-total seconds-per-day))
             (year2 (nth 5 (multiple-value-list
                            (decode-universal-time u2 0))))
             (years 0)
             (months 0)
             (days 0)
             (days-after-years 0)
             (hours 0)
             (minutes 0)
             (seconds 0))

        (multiple-value-bind (sec1 min1 hour1 date1 month1 year1)
            (decode-universal-time u1 0)

          ;; Years
          (setf u1 (encode-universal-time sec1 min1 hour1
                                          date1 month1 year2 0))
          (if (<= u1 u2)
              (progn
                (setf years (- year2 year1))
                (setf year1 year2))
              (progn
                (setf u1 (encode-universal-time sec1 min1 hour1
                                                date1 month1 (1- year2) 0))
                (setf years (- year2 year1 1))
                (setf year1 (1- year2))))

          (setf days-after-years (truncate (- u2 u1) seconds-per-day))

          ;; Months
          (loop
            :with y := year1
            :with m := month1
            :for new := (progn
                          (setf (values y m) (increase-month y m))
                          (encode-universal-time sec1 min1 hour1
                                                 date1 m y 0))
            :while (<= new u2)
            :do (setf year1 y
                      month1 m)
                (setf u1 new)
                (incf months))

          ;; Days
          (loop
            :with y := year1
            :with m := month1
            :with d := date1
            :for new := (progn
                          (setf (values y m d) (increase-date y m d))
                          (encode-universal-time sec1 min1 hour1
                                                 d m y 0))
            :while (<= new u2)
            :do (setf year1 y
                      month1 m
                      date1 d)
                (setf u1 new)
                (incf days))

          ;; Hours, mins, secs
          (setf seconds (- u2 u1))
          (setf hours (truncate seconds 3600))
          (decf seconds (* hours 3600))
          (setf minutes (truncate seconds 60))
          (decf seconds (* minutes 60))

          ;; Return
          (values
           (make-instance 'duration :seconds-total seconds-total
                                    :days-total days-total
                                    :years years :months months :days days
                                    :days-after-years days-after-years
                                    :hours hours :minutes minutes
                                    :seconds seconds)
           most-recent)))))
