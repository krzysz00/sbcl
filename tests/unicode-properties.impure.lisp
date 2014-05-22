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

;; Taken straight out of the common lisp cookbook
(defun replace-all (part replacement string &key (test #'char=))
"Returns a new string in which all the occurences of the part
is replaced with replacement."
    (with-output-to-string (out)
      (loop with part-length = (length part)
            for old-pos = 0 then (+ pos part-length)
            for pos = (search part string
                              :start2 old-pos
                              :test test)
            do (write-string string out
                             :start old-pos
                             :end (or pos (length string)))
            when pos do (write-string replacement out)
            while pos))) 

(defun test-line (line)
  (destructuring-bind (%cp %name %gc ccc %bidi decomp-map
                       %decimal-digit %digit %numeric
                       %bidi-mirrored old-name old-comment
                       simple-up simple-down simple-title)
      (split-string line #\;)
    (declare (ignore decomp-map old-name old-comment simple-up
                     simple-down simple-title))
    (let* ((cp (parse-integer %cp :radix 16))
           (char (code-char cp))
           (gc (intern (string-upcase %gc) "KEYWORD"))
           (bidi (intern (string-upcase %bidi) "KEYWORD"))
           (name (if (position #\< %name) nil (substitute #\_ #\Space %name)))
           (char-from-name (name-char name))
           (decimal-digit (parse-integer %decimal-digit :junk-allowed t))
           (digit (parse-integer %digit :junk-allowed t))
           (numeric (if (string= %numeric "") nil (read-from-string %numeric)))
           (bidi-mirrored (string= %bidi-mirrored "Y")))
      (when char-from-name
        (assert (char= char char-from-name)))
      (assert (eql gc (general-category char)))
      (assert (= (parse-integer ccc) (combining-class char)))
      (assert (eql bidi (bidi-class char)))
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

(defun codepoint-or-range (string)
  (flet ((parse (str) (parse-integer str :radix 16 :junk-allowed t)))
    (let ((parts (remove "" (split-string string #\.) :test #'string=)))
      (if (cdr parts)
          (loop for i from (parse (car parts)) to (parse (cadr parts)) collect i)
          (mapcar #'parse parts)))))

(defun test-property-line (fn line)
  (destructuring-bind (%codepoints value) (split-string line #\;)
    (let* ((codepoints (codepoint-or-range %codepoints))
           (property (remove #\Space value))
           (expected (intern
                      (string-upcase
                       (subseq property 0 (position #\# property)))
                      "KEYWORD")))

      (loop for i in codepoints do
           (unless (eql expected (funcall fn (code-char i)))
             (error "Character ~S has the wrong value for the tested property.
Wanted ~S, got ~S."
                    (code-char i) expected (funcall fn (code-char i))))))))

(defun test-hangul-syllable-type ()
  (declare (optimize (debug 2)))
  (with-open-file (s "data/HangulSyllableType.txt" :external-format :ascii)
    (with-test (:name (:hangul-syllable-type)
                :skipped-on '(not :sb-unicode))
      (loop for line = (read-line s nil nil)
            while line
            unless (or (string= "" line) (eql 0 (position #\# line)))
            do (test-property-line #'hangul-syllable-type line)))))

(test-hangul-syllable-type)

(defun test-east-asian-width ()
  (declare (optimize (debug 2)))
  (with-open-file (s "../tools-for-build/EastAsianWidth.txt"
                     :external-format :ascii)
    (with-test (:name (:east-asian-width)
                :skipped-on '(not :sb-unicode))
      (loop for line = (read-line s nil nil)
            while line
            unless (or (string= "" line) (eql 0 (position #\# line)))
            do (test-property-line #'east-asian-width line)))))

(test-east-asian-width)

(defun test-grapheme-break-type ()
  (declare (optimize (debug 2)))
  (with-open-file (s "data/GraphemeBreakProperty.txt"
                     :external-format :ascii)
    (with-test (:name (:grapheme-break-type)
                :skipped-on '(not :sb-unicode))
      (loop for line = (read-line s nil nil)
            while line
            unless (or (string= "" line) (eql 0 (position #\# line)))
            do (test-property-line #'sb-unicode::grapheme-break-type 
                                   (replace-all "SpacingMark" "SPACING-MARK"
                                                (substitute #\- #\_ line)))))))

(test-grapheme-break-type)

(defun test-word-break-type ()
  (declare (optimize (debug 2)))
  (with-open-file (s "data/WordBreakProperty.txt"
                     :external-format :ascii)
    (with-test (:name (:word-break-type)
                :skipped-on '(not :sb-unicode))
      (loop for line = (read-line s nil nil)
            while line
            unless (or (string= "" line) (eql 0 (position #\# line)))
            do (test-property-line #'sb-unicode::word-break-type
                                   (substitute #\- #\_ line))))))

(test-word-break-type)

(defun test-sentence-break-type ()
  (declare (optimize (debug 2)))
  (with-open-file (s "data/SentenceBreakProperty.txt"
                     :external-format :ascii)
    (with-test (:name (:sentence-break-type)
                :skipped-on '(not :sb-unicode))
      (loop for line = (read-line s nil nil)
            while line
            unless (or (string= "" line) (eql 0 (position #\# line)))
            do (test-property-line #'sb-unicode::sentence-break-type line)))))

(test-sentence-break-type)

