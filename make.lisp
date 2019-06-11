(defparameter *sbcl* (nth 0 sb-ext:*posix-argv*))
(defparameter *cmd* (nth 1 sb-ext:*posix-argv*))
(defparameter *gpg* (or (nth 2 sb-ext:*posix-argv*) "gpg"))
(defparameter *lib* (sb-ext:native-pathname (nth 3 sb-ext:*posix-argv*)))

(load "asdf.conf")
(load "quicklisp/setup.lisp")
(ql:quickload "gpg-utilities")

(asdf:operate 'asdf:monolithic-deliver-asd-op "gpg-utilities")

(with-open-file (f (format nil "build/~A" *cmd*) :direction :output
                                                 :if-does-not-exist :create
                                                 :if-exists :supersede)
  (with-standard-io-syntax
    (let ((*print-pretty* t)
          (*print-right-margin* 78)
          (*print-case* :downcase))
      (format f "#!~A --script~%~@{~S~%~}"
              *sbcl*
              '(require "asdf")
              (list 'asdf:initialize-source-registry
                    (list 'quote
                          (list :source-registry
                                :ignore-inherited-configuration
                                (list :directory *lib*))))
              '(handler-case
                (asdf:operate 'asdf:monolithic-load-bundle-op "gpg-utilities")
                (serious-condition (c)
                 (format *error-output* "~A~%" c)
                 (sb-ext:exit :code 1)))
              (list 'setf 'common:*gpg-program* *gpg*)
              (list 'start:start *cmd*)))))
