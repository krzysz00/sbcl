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

(defun split-string (string delimiter)
  (loop for begin = 0 then (1+ end)
        for end = (position delimiter string) then (position delimiter string :start begin)
        collect (subseq string begin end)
        while end))

(defun test-line (line)
  (destructuring-bind (%cp %name gc ccc bidi decomp-map
                       %decimal-digit %digit %numeric
                       %bidi-mirrored old-name old-comment
                       simple-up simple-down simple-title)
      (split-string line #\;)
    (declare (ignore decomp-map old-name old-comment simple-up
                     simple-down simple-title))
    (let* ((cp (parse-integer %cp :radix 16))
           (char (code-char cp))
           (name (if (position #\< %name) nil (substitute #\_ #\Space %name)))
           (char-from-name (name-char name))
           (decimal-digit (parse-integer %decimal-digit :junk-allowed t))
           (digit (parse-integer %digit :junk-allowed t))
           (numeric (if (string= %numeric "") nil (read-from-string %numeric)))
           (bidi-mirrored (string= %bidi-mirrored "Y")))
      (when char-from-name
        (assert (char= char char-from-name)))
      (assert (string= gc (general-category char)))
      (assert (= (parse-integer ccc) (combining-class char)))
      (assert (string= bidi (bidi-class char)))
      (assert (eql decimal-digit (decimal-digit char)))
      (assert (eql digit (digit-value char)))
      (assert (eql numeric (numeric-value char)))
      (assert (eql bidi-mirrored (mirrored-p char))))))

(defun test-property-reads ()
  (declare (optimize (debug 2)))
  (with-open-file (s (merge-pathnames
                      (make-pathname
                       :directory '(:relative :up "tools-for-build")
                       :name "UnicodeData" :type "txt")
                      (or *load-truename* *compile-file-truename*)))
    (with-test (:name (:unicode-properties)
                :skipped-on '(not :sb-unicode))
      (loop for line = (read-line s nil nil)
            while line
            do (test-line line)))))

(test-property-reads)
