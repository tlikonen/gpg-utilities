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

          (flet ((read-chars (stream predicate max)
                   (with-output-to-string (chars)
                     (loop :repeat max
                           :for c := (peek-char nil stream)
                           :do (if (funcall predicate c)
                                   (princ (read-char stream) chars)
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
                    (#\a (princ (code-char #x07) out))
                    (#\b (princ #\backspace out))
                    (#\e (princ (code-char #x1b) out))
                    (#\f (princ (code-char #x0c) out))
                    (#\n (princ #\newline out))
                    (#\r (princ #\return out))
                    (#\t (princ #\tab out))
                    (#\v (princ (code-char #x0b) out))
                    (#\\ (princ #\\ out))
                    (#\' (princ #\' out))
                    (#\" (princ #\" out))
                    (#\? (princ #\? out))

                    (#\u
                     (let ((cp (read-chars in #'hex-char-p 4)))
                       (when (= 4 (length cp))
                         (princ (code-char (parse-int-base 16 cp)) out))))

                    (#\U
                     (let ((cp (read-chars in #'hex-char-p 8)))
                       (when (= 8 (length cp))
                         (princ (code-char (parse-int-base 16 cp)) out))))

                    (#\x
                     (let ((cp (read-chars in #'hex-char-p 2)))
                       (when (plusp (length cp))
                         (princ (code-char (parse-int-base 16 cp)) out))))

                    (t
                     (unread-char sub-char in)
                     (let ((cp (read-chars in #'octal-char-p 3)))
                       (when (plusp (length cp))
                         (princ (code-char (parse-int-base 8 cp)) out))))))

              :else :do (princ char out))))

      (end-of-file () nil))))
