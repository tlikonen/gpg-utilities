(defparameter *sbcl* (nth 0 sb-ext:*posix-argv*))
(defparameter *cmd* (nth 1 sb-ext:*posix-argv*))
(defparameter *gpg* (or (nth 2 sb-ext:*posix-argv*) "gpg"))
(defparameter *lib* (sb-ext:native-pathname (nth 3 sb-ext:*posix-argv*)))

(require "asdf")

(asdf:initialize-output-translations
 (list :output-translations
       :ignore-inherited-configuration
       (list (merge-pathnames "src/*.*")
             (merge-pathnames "build/*.*"))))

(asdf:initialize-source-registry
 (list :source-registry
       :ignore-inherited-configuration
       (list :directory (merge-pathnames "src/"))))

(let ((*compile-print* nil))
  (asdf:operate 'asdf:load-op "gpg-utilities")
  (asdf:operate 'asdf:monolithic-deliver-asd-op "gpg-utilities"))

(with-open-file (f (format nil "build/~A" *cmd*) :direction :output
                                                 :if-does-not-exist :create
                                                 :if-exists :supersede)
  (with-standard-io-syntax
    (let ((*print-pretty* t)
          (*print-right-margin* 78)
          (*print-case* :downcase))
      (format f "#!~A --script~%" *sbcl*)
      (format f "~{~S~%~}"
              (list '(require "asdf")
                    (list 'asdf:initialize-source-registry
                          (list 'quote
                                (list :source-registry
                                      :ignore-inherited-configuration
                                      (list :directory *lib*))))
                    '(asdf:operate 'asdf:monolithic-load-bundle-op
                      "gpg-utilities")
                    (list 'setf 'common:*gpg-program* *gpg*)
                    (list 'start:start *cmd*))))))
