;; Author: Teemu Likonen <tlikonen@iki.fi>
;;
;; License: Creative Commons CC0 (public domain dedication)
;; https://creativecommons.org/publicdomain/zero/1.0/legalcode

(defpackage #:string-io
  (:use #:cl)
  (:export #:parse-quoted-word
           #:current-string #:new-string
           #:closing-quote-missing
           #:parsed-string
           #:escape-characters
           #:quote-string
           #:sql-string
           #:sql-like-escape
           #:unescape-c-string))

(in-package #:string-io)

(define-condition closing-quote-missing (end-of-file)
  ((string :reader parsed-string :initarg :string))
  (:report "The end of file was reached before closing quote character."))

(defun parse-quoted-word (string-or-stream
                          &key output-stream
                            (quote-char #\") (escape-char #\\)
                            (separator-chars '(#\space #\tab))
                            (unescape t))

  "Parse STRING-OR-STREAM and return the next word separated by
SEPARATOR-CHARS. If nothing could be parsed return NIL. If OUTPUT-STREAM
is a stream write also output to that stream.

If QUOTE-CHAR is a character then characters surrounded by QUOTE-CHARs
are part of a word. Also, if ESCAPE-CHAR is non-NIL then characters that
come right after ESCAPE-CHAR are part of a word.

If UNESCAPE is non-NIL remove ESCAPE-CHARs (when they are not escaped
itself) from the output; if UNESCAPE is NIL interpret ESCAPE-CHARs but
don't remove them.

If there is opening quote but the closing quote is missing signal
CLOSING-QUOTE-MISSING condition (subtype of CL:END-OF-FILE). The
condition object contains the stream and it can be read with
CL:STREAM-ERROR-STREAM function. The currently parsed string can be read
with PARSED-STRING function. There is CURRENT-STRING restart available.
When it is invoked the function continues and accepts the currently
parsed string (if any). Another restart NEW-STRING must be invoked with
a new string value which is then used and returned."

  (check-type output-stream (or stream null))
  (check-type quote-char (or character null))
  (check-type escape-char (or character null))
  (check-type separator-chars sequence)

  (labels ((separator-char-p (char)
             (find char separator-chars))
           (quote-char-p (char)
             (char= char quote-char))
           (escape-char-p (char)
             (char= char escape-char))
           (parse-stream (in out)
             (let ((quote nil))
               (handler-case
                   (loop
                     :with content := nil
                     :with esc := nil
                     :for char := (read-char in)

                     :do (cond
                           ((and (separator-char-p char)
                                 (not content)
                                 (not quote)
                                 (not esc)))
                           ((and (separator-char-p char)
                                 content
                                 (not quote)
                                 (not esc))
                            (return))
                           ((and escape-char
                                 (escape-char-p char)
                                 (not esc))
                            (setf esc t)
                            (unless unescape
                              (vector-push-extend char out)))
                           ((and quote-char
                                 (quote-char-p char)
                                 (not quote)
                                 (not esc))
                            (setf quote t))
                           ((and quote-char
                                 (quote-char-p char)
                                 quote
                                 (not esc))
                            (setf quote nil))
                           (t (vector-push-extend char out)
                              (setf content t)
                              (setf esc nil))))

                 (end-of-file (c)
                   (if quote (error 'closing-quote-missing
                                    :stream (stream-error-stream c)
                                    :string out)))))))

    (let ((output (make-array 20 :element-type 'character
                                 :adjustable t
                                 :fill-pointer 0)))
      (restart-case
          (etypecase string-or-stream
            (string (with-input-from-string (s string-or-stream)
                      (parse-stream s output)))
            (stream (parse-stream string-or-stream output)))
        (current-string ()
          :report "Continue and accept the current string.")
        (new-string (new)
          :report "Continue and supply a new string."
          (check-type new string)
          (setf output new)))

      (when (plusp (length output))
        (when output-stream
          (princ output output-stream))
        output))))

(defun escape-characters (string needs-escaping escape-char)
  "Return STRING which has the ESCAPE-CHAR before every character in
NEEDS-ESCAPING sequence."
  (check-type string string)
  (check-type needs-escaping sequence)
  (check-type escape-char character)
  (with-output-to-string (out)
    (loop :for char :across string
          :do (when (find char needs-escaping)
                (princ escape-char out))
              (princ char out))))

(defun quote-string (string &key output-stream
                              (quote-char #\") (escape-char #\\)
                              (needs-escaping (list quote-char escape-char)))

  "Put STRING inside quotes (QUOTE-CHAR).

If ESCAPE-CHAR is a non-NIL put it before every character in
NEEDS-ESCAPING sequence.

When OUTPUT-STREAM is a stream write the output to that stream."

  (check-type string string)
  (check-type output-stream (or stream null))
  (check-type quote-char character)
  (check-type escape-char (or character null))
  (check-type needs-escaping sequence)

  (let ((output
          (concatenate 'string
                       (string quote-char)
                       (if escape-char
                           (escape-characters string needs-escaping
                                              escape-char)
                           string)
                       (string quote-char))))

    (when output-stream (princ output output-stream))
    output))

(defun sql-string (string &key output-stream)
  "Return STRING as an SQL language string ('...') and escape all '
characters.

If OUTPUT-STREAM is a stream write the output to that stream."
  (quote-string string :output-stream output-stream
                       :quote-char #\'
                       :escape-char #\'
                       :needs-escaping "'"))

(defun sql-like-escape (string &key output-stream wild-before wild-after)
  "Return STRING as an SQL language string ('...') and escape all '
characters as well as all special characters of SQL LIKE operator. If
WILD-BEFORE or WILD-AFTER is non-NIL put LIKE operator's % wild card
character at the beginning or the end of the output string,
respectively. This function's output is only useful with SQL LIKE
operator.

If OUTPUT-STREAM is a stream write the output to that stream too."

  (check-type output-stream (or stream null))

  (let ((output
          (concatenate
           'string
           (sql-string (concatenate
                        'string
                        (if wild-before "%")
                        (escape-characters string "_%\\" #\\)
                        (if wild-after "%"))
                       :output-stream nil)
           " ESCAPE '\\'")))

    (when output-stream
      (princ output output-stream))
    output))

(defun unescape-c-string (string-or-stream &key output-stream)
  "Parse a C language STRING and unescape \"\\\" escape sequences.
Return a new string.

If OUTPUT-STREAM is a stream write the output to the stream too.

The function assumes that the Common Lisp implementation uses Unicode
character codes."

  (check-type string-or-stream (or string stream))
  (check-type output-stream (or stream null))

  (labels
      ((read-chars (in predicate max)
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
         (parse-integer string :radix base))

       (parse-stream (in out)
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

    (let ((output
            (with-output-to-string (out)
              (handler-case
                  (etypecase string-or-stream
                    (string (with-input-from-string (s string-or-stream)
                              (parse-stream s out)))
                    (stream (parse-stream string-or-stream out)))
                (end-of-file () nil)))))

      (when output-stream
        (princ output output-stream))
      output)))
