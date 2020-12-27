(defparameter *name* (nth 1 sb-ext:*posix-argv*))

(load "asdf.conf")
(load "quicklisp/setup.lisp")
(ql:quickload "gpg-utilities")

(with-open-file (*standard-output* (ensure-directories-exist *name*)
                                   :direction :output
                                   :if-does-not-exist :create
                                   :if-exists :supersede)
  (funcall (find-symbol "PRINT-USAGE"
                        (string-upcase (pathname-name *name*)))))
