;;; -*-  Lisp -*-

(defpackage #:sb-unicode-strings-system
  (:use #:cl #:asdf))

(defsystem sb-unicode-strings
  :version "0.0.1"
  #+sb-building-contrib :pathname
  #+sb-building-contrib #p"SYS:CONTRIB;SB-UNICODE-STRINGS;"
  :components
  ((:file "defpackage")
   (:file "base-type" :depends-on ("defpackage"))))

(defmethod perform :after ((o load-op) (c (eql (find-system :sb-unicode-strings))))
  (provide 'sb-unicode-strings))

(defmethod perform ((o test-op) (c (eql (find-system :sb-unicode-strings))))
  (operate 'load-op 'sb-unicode-strings-tests)
  (operate 'test-op 'sb-unicode-strings-tests))

(defsystem sb-unicode-strings-tests
  :depends-on (sb-unicode-strings sb-rt)
  :version "0.0.1"
  :components ((:file "tests")))

(defmethod perform ((o test-op) (c (eql (find-system :sb-unicode-strings-tests))))
  (or (funcall (intern "DO-TESTS" (find-package "SB-RT")))
      (error "test-op failed")))
