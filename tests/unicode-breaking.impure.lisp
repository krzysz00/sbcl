;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; While most of SBCL is derived from the CMU CL system, the test
;;;; files (like this one) were written from scratch after the fork
;;;; from CMU CL.
;;;;
;;;; This software is in the public domain and is provided with
;;;; absolutely no warranty. See the COPYING and CREDITS files for
;;;; more information.

(use-package :sb-unicode)

(defconstant +mul+ #+sb-unicode (code-char 215) #-sb-unicode #\*)
(defconstant +div+ #+sb-unicode (code-char 247) #-sb-unicode #\/)

(defun split-string (string delimiter)
  (loop for begin = 0 then (1+ end)
        for end = (position delimiter string) then (position delimiter string :start begin)
        collect (subseq string begin end)
        while end))

(defun grapheme-line-to-clusters (line)
  (let ((codepoints
         (remove "" (split-string (substitute #\Space +div+ line) #\Space)
                 :test #'string=))
         clusters cluster (nobreak t))
    (loop for i in codepoints do
         (if (string= i (string +mul+)) (setf nobreak t)
             (progn
               (unless nobreak
                 (push (nreverse cluster) clusters)
                 (setf cluster nil))
               (push (code-char (parse-integer i :radix 16)) cluster)
               (setf nobreak nil))))
    (when cluster (push (nreverse cluster) clusters))
    (setf clusters (nreverse (mapcar #'(lambda (x) (coerce x 'string)) clusters)))
    clusters))

(defun parse-codepoints (string &key (singleton-list t))
  (let ((list (mapcar
              (lambda (s) (parse-integer s :radix 16))
              (remove "" (split-string string #\Space) :test #'string=))))
    (if (not (or (cdr list) singleton-list)) (car list) list)))

(defun test-grapheme-line (line)
  (let ((relevant-portion (subseq line 0 (position #\# line))))
    (when (string/= relevant-portion "")
      (let ((string
             (coerce (mapcar
                      #'code-char
                      (parse-codepoints
                        (remove +mul+ (remove +div+ relevant-portion))))
                     'string)))
      (assert (equalp (graphemes string)
                      (grapheme-line-to-clusters relevant-portion)))))))

(defun test-graphemes ()
  (declare (optimize (debug 2)))
  (with-test (:name (:grapheme-breaking)
                    :skipped-on '(not :sb-unicode))
    (with-open-file (s "data/GraphemeBreakTest.txt" :external-format :utf8)
      (loop for line = (read-line s nil nil)
            while line
            do (test-grapheme-line (remove #\Tab line))))))

(test-graphemes)

