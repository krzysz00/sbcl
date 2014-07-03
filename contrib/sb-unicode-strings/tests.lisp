(defpackage :sb-unicode-strings-tests
  (:use #:sb-unicode-strings #:cl #:sb-rt))
(in-package :sb-unicode-strings-tests)

(enable-unicode-string-syntax *readtable*)
(deftest iteration
  (let ((ret t))
    (sb-sequence:dosequence (i #U"abc" ret)
      (setf ret (and ret (stringp i)))))
  t)

(deftest count
  (and
   (= (count #\a #U"abc") 1)
   (= (count #\a #U"àbc") 0)
   (= (count "a" #U"abc") 1)
   (= (count "à" #U"àbc") 1))
  t)

(deftest string-length (length (string->unicode-string "noël")) 4)

(deftest elt-setf
  (let ((string (string->unicode-string "abc")))
    (setf (elt string 0) #\d)
    (setf (elt string 1) "b")
    (setf (elt string 2) "ë")
    (mismatch string #U"dbë"))
  nil)

(deftest multiple-graphemes-error
  (handler-case
      (let ((string (string->unicode-string "abc")))
        (setf (elt string 0) "de") :good)
    (error () :error))
  :error)
