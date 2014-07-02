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
  t)
  ;; (operate 'load-op 'sb-unicode-strihgs-tests)
  ;;(operate 'test-op 'sb-unicode-strings-tests))
