;; sbcl --script run.lisp gpg-tofu ...

(require "asdf")

(asdf:initialize-source-registry
 (list :source-registry
       :ignore-inherited-configuration
       (list :directory (merge-pathnames "build/src/"))))

(handler-case
    (asdf:operate 'asdf:monolithic-load-bundle-op "gpg-utilities")
  (serious-condition (c)
    (format *error-output* "~A~%" c)
    (sb-ext:exit :code 1)))

(start:start (pop (rest sb-ext:*posix-argv*)))
