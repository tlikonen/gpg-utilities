;; Author: Teemu Likonen <tlikonen@iki.fi>
;;
;; License: Creative Commons CC0 (public domain dedication)
;; https://creativecommons.org/publicdomain/zero/1.0/legalcode

(defpackage #:c-strings
  (:use #:cl)
  (:export #:unescape-c-string))

(in-package #:c-strings)

(defun unescape-c-string (string)
  "Parse a C language string and unescape \"\\\" escape sequences.
Return a new string.

The function assumes that the Common Lisp implementation uses Unicode
character codes."

  (with-output-to-string (out)
    (handler-case
        (with-input-from-string (in string)

          (flet ((read-chars (predicate max)
                   (with-output-to-string (chars)
                     (loop :repeat max
                           :for c := (peek-char nil in)
                           :do (if (funcall predicate c)
                                   (princ (read-char in) chars)
                                   (loop-finish)))))

                 (hex-char-p (char)
                   (digit-char-p char 16))

                 (octal-char-p (char)
                   (digit-char-p char 8))

                 (parse-int-base (base string)
                   (parse-integer string :radix base)))

            (loop
              :for char := (read-char in)
              :if (char= char #\\) :do

                (let ((sub-char (read-char in)))
                  (case sub-char
                    (#\a (princ #\Bel out))
                    (#\b (princ #\Backspace out))
                    (#\e (princ #\Esc out))
                    (#\f (princ #\Page out))
                    (#\n (princ #\Newline out))
                    (#\r (princ #\Return out))
                    (#\t (princ #\Tab out))
                    (#\v (princ #\Vt out))
                    (#\\ (princ #\\ out))
                    (#\' (princ #\' out))
                    (#\" (princ #\" out))
                    (#\? (princ #\? out))

                    (#\u
                     (let ((cp (read-chars #'hex-char-p 4)))
                       (when (= 4 (length cp))
                         (princ (code-char (parse-int-base 16 cp)) out))))

                    (#\U
                     (let ((cp (read-chars #'hex-char-p 8)))
                       (when (= 8 (length cp))
                         (princ (code-char (parse-int-base 16 cp)) out))))

                    (#\x
                     (let ((cp (read-chars #'hex-char-p 2)))
                       (when (plusp (length cp))
                         (princ (code-char (parse-int-base 16 cp)) out))))

                    (t
                     (unread-char sub-char in)
                     (let ((cp (read-chars #'octal-char-p 3)))
                       (when (plusp (length cp))
                         (princ (code-char (parse-int-base 8 cp)) out))))))

              :else :do (princ char out))))

      (end-of-file () nil))))
