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
                       %bidi-mirrored %old-name old-comment
                       simple-up simple-down simple-title)
      (split-string line #\;)
    (declare (ignore decomp-map old-comment simple-up
                     simple-down simple-title))
    (let* ((cp (parse-integer %cp :radix 16))
           (char (code-char cp))
           (gc (intern (string-upcase %gc) "KEYWORD"))
           (bidi (intern (string-upcase %bidi) "KEYWORD"))
           ;; See `normalize-character-name` in ucd.lisp for a discussion
           ;; of U+1F5CF (PAGE) and the attendant standards-compliance issues2
           (name (unless (or (position #\< %name) (= cp #x1F5CF))
                   (substitute #\_ #\Space %name)))
           (old-name (unless (string= %old-name "")
                       (substitute #\_ #\Space %old-name)))
           (char-from-name (name-char name))
           (char-from-old-name
            (when old-name
              (name-char (concatenate 'string "UNICODE1_" old-name))))
           (decimal-digit (parse-integer %decimal-digit :junk-allowed t))
           (digit (parse-integer %digit :junk-allowed t))
           (numeric (if (string= %numeric "") nil (read-from-string %numeric)))
           (bidi-mirrored (string= %bidi-mirrored "Y")))
      (when char-from-name
        (assert (char= char char-from-name)))
      (when char-from-old-name
        (assert (char= char char-from-old-name)))
      (assert (eql gc (general-category char)))
      (assert (= (parse-integer ccc) (combining-class char)))
      (assert (eql bidi (bidi-class char)))
      (assert (eql decimal-digit (decimal-value char)))
      (assert (eql digit (digit-value char)))
      (assert (eql numeric (numeric-value char)))
      (assert (eql bidi-mirrored (mirrored-p char)))
      (assert (string= old-name (unicode-1-name char))))))

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

(defun test-script ()
  (declare (optimize (debug 2)))
  (with-open-file (s "../tools-for-build/Scripts.txt"
                     :external-format :ascii)
    (with-test (:name (:script)
                :skipped-on '(not :sb-unicode))
      (loop for line = (read-line s nil nil)
            while line
            unless (or (string= "" line) (eql 0 (position #\# line)))
            do (test-property-line #'script (substitute #\- #\_ line))))))

(test-script)

(defun test-proplist ()
  (declare (optimize (debug 2)))
  (with-open-file (s "../tools-for-build/PropList.txt"
                     :external-format :ascii)
    (with-test (:name (:proplist)
                :skipped-on '(not :sb-unicode))
      (loop for line = (read-line s nil nil)
         while line
         unless (or (string= "" line) (eql 0 (position #\# line)))
         do
           (destructuring-bind (%codepoints value) (split-string line #\;)
                 (let* ((codepoints (codepoint-or-range %codepoints))
                        (property
                         (intern (string-upcase
                                  (substitute
                                   #\- #\_
                                   (subseq (remove #\Space value) 0
                                           (position #\# (remove #\Space value)))))
                          "KEYWORD")))
                   (loop for i in codepoints do
                        (unless (proplist-p (code-char i) property)
                          (error "Character ~S should be ~S, but isn't."
                                 (code-char i) property)))))))))

(test-proplist)

(defun test-bidi-mirroring-glyph ()
  (declare (optimize (debug 2)))
  (with-open-file (s "../tools-for-build/BidiMirroring.txt"
                     :external-format :ascii)
    (with-test (:name (:bidi-mirroring-glyph)
                :skipped-on '(not :sb-unicode))
      (loop for line = (read-line s nil nil)
         while line
         unless (or (string= "" line) (eql 0 (position #\# line)))
         do
           (let* ((codepoints
                   (split-string (subseq line 0 (position #\# line)) #\;))
                  (chars
                   (mapcar
                    #'(lambda (s) (code-char (parse-integer s :radix 16)))
                    codepoints)))
             (unless (char= (bidi-mirroring-glyph (first chars)) (second chars))
               (error "The mirroring glyph of ~S is not ~S, but ~S"
                      (first chars) (second chars)
                      (bidi-mirroring-glyph (first chars)))))))))

(test-bidi-mirroring-glyph)

(defun test-grapheme-break-class ()
  (declare (optimize (debug 2)))
  (with-open-file (s "data/GraphemeBreakProperty.txt"
                     :external-format :ascii)
    (with-test (:name (:grapheme-break-class)
                :skipped-on '(not :sb-unicode))
      (loop for line = (read-line s nil nil)
            while line
            unless (or (string= "" line) (eql 0 (position #\# line)))
            do (test-property-line #'grapheme-break-class
                                   (replace-all "SpacingMark" "SPACING-MARK"
                                                (substitute #\- #\_ line)))))))

(test-grapheme-break-class)

(defun test-word-break-class ()
  (declare (optimize (debug 2)))
  (with-open-file (s "data/WordBreakProperty.txt"
                     :external-format :ascii)
    (with-test (:name (:word-break-class)
                :skipped-on '(not :sb-unicode))
      (loop for line = (read-line s nil nil)
            while line
            unless (or (string= "" line) (eql 0 (position #\# line)))
            do (test-property-line #'word-break-class
                                   (substitute #\- #\_ line))))))

(test-word-break-class)

(defun test-sentence-break-class ()
  (declare (optimize (debug 2)))
  (with-open-file (s "data/SentenceBreakProperty.txt"
                     :external-format :ascii)
    (with-test (:name (:sentence-break-class)
                :skipped-on '(not :sb-unicode))
      (loop for line = (read-line s nil nil)
            while line
            unless (or (string= "" line) (eql 0 (position #\# line)))
            do (test-property-line #'sentence-break-class line)))))

(test-sentence-break-class)

(defun test-line-break-class ()
  (declare (optimize (debug 2)))
  (with-open-file (s "../tools-for-build/LineBreakProperty.txt"
                     :external-format :ascii)
    (with-test (:name (:line-break-class)
                :skipped-on '(not :sb-unicode))
      (loop for line = (read-line s nil nil)
            while line
            unless (or (string= "" line) (eql 0 (position #\# line)))
            do (test-property-line #'line-break-class line)))))

(test-line-break-class)
