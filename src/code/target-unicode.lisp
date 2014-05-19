;;;; Unicode functions

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!UNICODE")

(eval-when (:compile-toplevel :load-toplevel :execute)
  ;; Lift some internal stuff from SB-IMPL to prevent piles of packkage prefixes
  (import 'SB!IMPL::**CHARACTER-MISC-DATABASE**)
  (import 'SB!IMPL::**CHARACTER-HIGH-PAGES**)
  (import 'SB!IMPL::**CHARACTER-LOW-PAGES**)
  (import 'SB!IMPL::**CHARACTER-DECOMPOSITIONS**)
  (import 'SB!IMPL::**CHARACTER-PRIMARY-COMPOSITIONS**)
  (import 'SB!IMPL::**CHARACTER-CASES**)
  (import 'SB!IMPL::**CHARACTER-CASE-PAGES**)
  (import 'SB!IMPL::MISC-INDEX)
  (import 'SB!IMPL::CLEAR-FLAG))

(defparameter **special-numerics**
  '#.(with-open-file (stream
                     (merge-pathnames
                      (make-pathname
                       :directory
                       '(:relative :up :up "output")
                       :name "numerics" :type "lisp-expr")
                      sb!xc:*compile-file-truename*)
                     :direction :input
                     :element-type 'character)
        (read stream)))

;;; Unicode property access
(defun reverse-ucd-indices (strings)
  (let ((hash (make-hash-table)))
    (loop for string in strings
          for index from 0
          do (setf (gethash index hash) string))
    hash))

;; WARNING: These have to be manually kept in sync with the values in ucd.lisp
(defparameter *general-categories*
  (reverse-ucd-indices
   '("Lu" "Ll" "Lt" "Lm" "Lo" "Cc" "Cf" "Co" "Cs" "Mc" "Me" "Mn" "Nd" "Nl"
     "No" "Pc" "Pd" "Pe" "Pf" "Pi" "Po" "Ps" "Sc" "Sk" "Sm" "So" "Zl" "Zp"
     "Zs")))


(defparameter *bidi-classes*
  (reverse-ucd-indices
   '("BN" "AL" "AN" "B" "CS" "EN" "ES" "ET" "L" "LRE" "LRO" "NSM" "ON"
     "PDF" "R" "RLE" "RLO" "S" "WS" "LRI" "RLI" "FSI" "PDI")))

(defun general-category (character)
  #!+sb-doc
  "Returns the general category of a character as it appears in UnicodeData.txt"
  (gethash (sb!impl::ucd-general-category character) *general-categories*))

(defun bidi-class (character)
  #!+sb-doc
  "Returns the bidirectional class of a character"
  (gethash
   (aref **character-misc-database** (+ 1 (misc-index character)))
   *bidi-classes*))

(defun combining-class (character)
  #!+sb-doc
  "Returns the canonical combining class (CCC) of a character"
  (aref **character-misc-database** (+ 2 (misc-index character))))

(defun decimal-digit (character)
  #!+sb-doc
  "Returns the decimal digit value associated with `character` or NIL if
there is no such value.

The only characters in Unicode with a decimal digit value are those
that are part of a range of characters that encode the digits 0-9.
Because of this, `(decimal-digit c) <=> (digit-char-p c 10)` in
#+sb-unicode builds"
  (sb!impl::ucd-decimal-digit character))

(defun digit-value (character)
  #!+sb-doc
  "Returns the Unicode digit value of a character or NIL if it doesn't exist.

Digit values are guaranteed to be integers between 0 and 9 inclusive.
All characters with decimal digit values have the same digit value,
but there are characters (such as digits of number systems without a 0 value)
that have a digit value but no decimal digit value"
  (let ((%digit (clear-flag 6
                            (aref **character-misc-database**
                                  (+ 3 (misc-index character))))))
    (if (< %digit 10) %digit nil)))

(defun numeric-value (character)
  #!+sb-doc
  "Returns the numeric value of a character or NIL if there is no such value.

Numeric value is the most general of the Unicode numeric properties.
The only constraint on the numeric value is that it be a rational number."
  (cdr (or (assoc (char-code character) **special-numerics**)
           (cons nil (digit-value character)))))

(defun mirrored-p (character)
  #!+sb-doc
  "Returns T if the given character needs to be mirrored in bidirectional text.
Otherwise, returns NIL."
  (logbitp 5 (aref **character-misc-database**
                    (+ 5 (misc-index character)))))

(defun hangul-syllable-type (character)
  #!+sb-doc
  "Returns the Hangul syllable type of the given character.
The syllable type can be one of :L, :V, :T, :LV, or :LVT.
If the character is not a Hangul syllable or Jamo, returns NIL"
  (let ((cp (char-code character)))
    (cond
      ((or
        (and (<= #x1100 cp) (<= cp #x115f))
        (and (<= #xa960 cp) (<= cp #xa97c))) :L)
      ((or
        (and (<= #x1160 cp) (<= cp #x11a7))
        (and (<= #xd7B0 cp) (<= cp #xd7C6))) :V)
      ((or
        (and (<= #x11a8 cp) (<= cp #x11ff))
        (and (<= #xd7c8 cp) (<= cp #xd7fb))) :T)
      ((and (<= #xac00 cp) (<= cp #xd7a3))
       (if (= 0 (rem (- cp #xac00) 28)) :LV :LVT)))))


;;; Implements UAX#15: Normalization Forms
(defun char-decomposition-info (char)
  (let ((value (aref **character-misc-database**
                     (+ 4 (misc-index char)))))
    (values (clear-flag 7 value) (logbitp 7 value))))

(defun char-decomposition (char length)
  ;; Caller should have gotten length from char-decomposition-info
  (let* ((cp (char-code char))
         (cp-high (ash cp -8))
         (decompositions **character-decompositions**)
         (high-page (aref **character-high-pages** cp-high))
         (index (unless (logbitp 15 high-page) ;; Hangul syllable
                  (aref **character-low-pages**
                        (+ 1 (* 2 (+ (ldb (byte 8 0) cp) (ash high-page 8)))))))
         (entry (when index (loop for i from 0 below length
                               collecting (aref decompositions (+ i index)))))
         (result (make-string length)))
    (if (= length 1)
        (string (code-char (car entry)))
        (progn
          (if (<= #xac00 cp #xd7a3)
              ;; see Unicode 6.2, section 3-12
              (let* ((sbase #xac00)
                     (lbase #x1100)
                     (vbase #x1161)
                     (tbase #x11a7)
                     (lcount 19)
                     (vcount 21)
                     (tcount 28)
                     (ncount (* vcount tcount))
                     (scount (* lcount ncount))
                     (sindex (- cp sbase))
                     (lindex (floor sindex ncount))
                     (vindex (floor (mod sindex ncount) tcount))
                     (tindex (mod sindex tcount)))
                (declare (ignore scount))
                (setf (char result 0) (code-char (+ lbase lindex)))
                (setf (char result 1) (code-char (+ vbase vindex)))
                (if (> tindex 0)
                  (setf (char result 2) (code-char (+ tbase tindex)))
                  (setf result (subseq result 0 2)))) ; Remove trailing #\Nul
              (loop for i from 0 for code in entry
                 do (setf (char result i) (code-char code))))
          result))))

(defun decompose-char (char)
  (let ((info (char-decomposition-info char)))
    (if (= info 0)
        (string char)
        (char-decomposition char info))))

(defun decompose-string (string &optional (kind :canonical))
  (declare (type (member :canonical :compatibility) kind))
  (flet ((canonical (char)
           (multiple-value-bind (len compat) (char-decomposition-info char)
             (and (/= len 0) (not compat))))
         (compat (char)
           (/= 0 (char-decomposition-info char))))
    (let (result
          (fun (ecase kind
                 (:canonical #'canonical)
                 (:compatibility #'compat))))
      (do* ((start 0 (1+ end))
            (end (position-if fun string :start start)
                 (position-if fun string :start start)))
           ((null end) (push (subseq string start end) result))
        (unless (= start end)
          (push (subseq string start end) result))
        ;; FIXME: this recursive call to DECOMPOSE-STRING is necessary
        ;; for correctness given our direct encoding of the
        ;; decomposition data in UnicodeData.txt.  It would, however,
        ;; be straightforward enough to perform the recursion in table
        ;; construction, and then have this simply revert to a single
        ;; lookup.  (Wait for tests to be hooked in, then implement).
        (push (decompose-string (decompose-char (char string end)) kind)
              result))
      (apply 'concatenate 'string (nreverse result)))))

(defun sort-combiners (string)
  (let (result (start 0) first-cc first-non-cc)
    (tagbody
     again
       (setf first-cc (position 0 string :key #'combining-class :test #'/= :start start))
       (when first-cc
         (setf first-non-cc (position 0 string :key #'combining-class :test #'= :start first-cc)))
       (push (subseq string start first-cc) result)
       (when first-cc
         (push (stable-sort (subseq string first-cc first-non-cc) #'< :key #'combining-class) result))
       (when first-non-cc
         (setf start first-non-cc first-cc nil first-non-cc nil)
         (go again)))
    (apply 'concatenate 'string (nreverse result))))

(defun composition-hangul-syllable-type (cp)
  (cond
    ((and (<= #x1100 cp) (<= cp #x1112)) :L)
    ((and (<= #x1161 cp) (<= cp #x1175)) :V)
    ((and (<= #x11a8 cp) (<= cp #x11c2)) :T)
    ((and (<= #xac00 cp) (<= cp #.(+ #xac00 11171)))
     (if (= 0 (rem (- cp #xac00) 28)) :LV :LVT))))

(defun primary-composition (char1 char2)
  (let ((c1 (char-code char1))
        (c2 (char-code char2)))
    (cond
      ((gethash (dpb (char-code char1) (byte 21 21) (char-code char2))
                **character-primary-compositions**))
      ((and (eql (composition-hangul-syllable-type c1) :L)
            (eql (composition-hangul-syllable-type c2) :V))
       (let ((lindex (- c1 #x1100))
             (vindex (- c2 #x1161)))
         (code-char (+ #xac00 (* lindex 588) (* vindex 28)))))
      ((and (eql (composition-hangul-syllable-type c1) :LV)
            (eql (composition-hangul-syllable-type c2) :T))
       (code-char (+ c1 (- c2 #x11a7)))))))

;;; This implements a sequence data structure, specialized for
;;; efficient deletion of characters at an index, along with tolerable
;;; random access.  The purpose is to support the canonical
;;; composition algorithm from Unicode, which involves replacing (not
;;; necessarily consecutive) pairs of code points with a single code
;;; point (e.g. [#\e #\combining_acute_accent] with
;;; #\latin_small_letter_e_with_acute).  The data structure is a list
;;; of three-element lists, each denoting a chunk of string data
;;; starting at the first index and ending at the second.
;;;
;;; Actually, the implementation isn't particularly efficient, and
;;; would probably benefit from being rewritten in terms of displaced
;;; arrays, which would substantially reduce copying.
;;;
;;; (also, generic sequences.  *sigh*.)
(defun lref (lstring index)
  (dolist (l lstring)
    (when (and (<= (first l) index)
               (< index (second l)))
      (return (aref (third l) (- index (first l)))))))

(defun (setf lref) (newchar lstring index)
  (dolist (l lstring)
    (when (and (<= (first l) index)
               (< index (second l)))
      (return (setf (aref (third l) (- index (first l))) newchar)))))

(defun llength (lstring)
  (second (first (last lstring))))

(defun lstring (lstring)
  (let ((result (make-string (llength lstring))))
    (dolist (l lstring result)
      (replace result (third l) :start1 (first l) :end1 (second l)))))

(defun ldelete (lstring index)
  (do* ((ls lstring (cdr ls))
        (l (car ls) (car ls))
        so-fars)
       ((and (<= (first l) index)
             (< index (second l)))
        (append
         (nreverse so-fars)
         (cond
           ((= (first l) index)
            (list (list (first l) (1- (second l)) (subseq (third l) 1))))
           ((= index (1- (second l)))
            (list (list (first l) (1- (second l)) (subseq (third l) 0 (1- (length (third l)))))))
           (t
            (list
             (list (first l) index
                   (subseq (third l) 0 (- index (first l))))
             (list index (1- (second l))
                   (subseq (third l) (1+ (- index (first l))))))))
         (mapcar (lambda (x) (list (1- (first x)) (1- (second x)) (third x)))
                 (cdr ls))))
    (push l so-fars)))

(defun canonically-compose (string)
  (labels ()
    (let* ((result (list (list 0 (length string) string)))
           (previous-starter-index (position 0 string :key #'combining-class))
           (i (and previous-starter-index (1+ previous-starter-index))))
      (when (or (not i) (= i (length string)))
        (return-from canonically-compose string))
      (tagbody
       again
         (when (and (>= (- i previous-starter-index) 2)
                    ;; test for Blocked (Unicode 3.11 para. D115)
                    ;;
                    ;; (assumes here that string has sorted combiners,
                    ;; so can look back just one step)
                    (>= (combining-class (lref result (1- i)))
                        (combining-class (lref result i))))
           (when (= (combining-class (lref result i)) 0)
             (setf previous-starter-index i))
           (incf i)
           (go next))

         (let ((comp (primary-composition (lref result previous-starter-index)
                                          (lref result i))))
           (cond
             (comp
              (setf (lref result previous-starter-index) comp)
              (setf result (ldelete result i)))
             (t
              (when (= (combining-class (lref result i)) 0)
                (setf previous-starter-index i))
              (incf i))))
       next
         (unless (= i (llength result))
           (go again)))
      (if (= i (length string))
          string
          (lstring result)))))

(defun normalize-string (string &optional (form :nfd))
  "Normalize `string` to the Uninoce normalization form `form`.
   Acceptable values for `form` are `:nfd`, `:nfc`, `:nfkd`, and `:nfkc`"
  (declare (type (member :nfd :nfkd :nfc :nfkc) form))
  #!-sb-unicode
  (etypecase string
    ((array nil (*)) string)
    (string
     (ecase form
       ((:nfc :nfkc) string)
       ((:nfd :nfkd) (error "Cannot normalize to ~A form in #-SB-UNICODE builds" form)))))
  #!+sb-unicode
  (etypecase string
    (base-string string)
    ((array character (*))
     (ecase form
       ((:nfc)
        (canonically-compose (sort-combiners (decompose-string string))))
       ((:nfd)
        (sort-combiners (decompose-string string)))
       ((:nfkc)
        (canonically-compose (sort-combiners (decompose-string string :compatibility))))
       ((:nfkd)
        (sort-combiners (decompose-string string :compatibility)))))
    ((array nil (*)) string)))


;;; Unicode case algorithms
;; FIXME: Make these parts less redundant (macro?)
(defparameter **special-titlecases**
  '#.(with-open-file (stream
                     (merge-pathnames
                      (make-pathname
                       :directory
                       '(:relative :up :up "output")
                       :name "titlecases" :type "lisp-expr")
                      sb!xc:*compile-file-truename*)
                     :direction :input
                     :element-type 'character)
        (read stream)))

(defparameter **special-casefolds**
  '#.(with-open-file (stream
                     (merge-pathnames
                      (make-pathname
                       :directory
                       '(:relative :up :up "output")
                       :name "foldcases" :type "lisp-expr")
                      sb!xc:*compile-file-truename*)
                     :direction :input
                     :element-type 'character)
        (read stream)))

(defun has-case-p (char)
  ;; Bit 6 is the Unicode case flag, as opposed to the Common Lisp one
  (logbitp 6 (aref **character-misc-database** (+ 5 (misc-index char)))))

(defun char-uppercase (char)
  (if (has-case-p char)
      (let ((cp (car (gethash (char-code char) **character-cases**))))
        (if (atom cp) (list (code-char cp)) (mapcar #'code-char cp)))
      (list char)))

(defun char-lowercase (char)
  (if (has-case-p char)
      (let ((cp (cdr (gethash (char-code char) **character-cases**))))
        (if (atom cp) (list (code-char cp)) (mapcar #'code-char cp)))
      (list char)))

(defun char-titlecase (char)
  (unless (has-case-p char) (return-from char-titlecase (list char)))
  (let* ((cp (char-code char))
         (value (assoc cp **special-titlecases**)))
    (if value
        (if (atom (cdr value))
            (list (code-char (cdr value)))
            (mapcar #'code-char (cdr value)))
        (char-uppercase char))))

(defun char-foldcase (char)
  (unless (has-case-p char) (return-from char-foldcase (list char)))
  (let* ((cp (char-code char))
         (value (assoc cp **special-casefolds**)))
    (if value
        (if (atom (cdr value))
            (list (code-char (cdr value)))
            (mapcar #'code-char (cdr value)))
        (char-lowercase char))))

(defun string-somethingcase (fn string)
  (let ((result))
    (loop for char across string
       for cased = (funcall fn char)
       do (loop for c in cased do (push c result)))
    (setf result (nreverse result))
    (coerce result 'string)))

#!-sb-fluid
(declaim (inline uppercase lowercase casefold))

(defun uppercase (string)
  #!+sb-doc
  "Returns the full uppercase of string according to the Unicode standard.
The result string is not guaranteed to have the same length as the input."
  (string-somethingcase #'char-uppercase string))

(defun lowercase (string)
  #!+sb-doc
  "Returns the full lowercase of string according to the Unicode standard.
The result string is not guaranteed to have the same length as the input."
  (string-somethingcase #'char-lowercase string))

;; TODO: Implement titlecase (requires word breaking algorithm)

(defun casefold (string)
  #!+sb-doc
  "Returns the full casefolding of string according to the Unicode standard.
Casefolding remove case information in a way that allaws the results to be used
for case-insensitive comparisons.
The result string is not guaranteed to have the same length as the input."
  (string-somethingcase #'char-foldcase string))


;;; Unicode break algorithms

(defun between (lower-bound item upper-bound)
  (and (<= lower-bound item) (<= item upper-bound)))

(defun grapheme-break-type (char)
  (let ((cp (when char (char-code char)))
        (gc (when char (general-category char)))
        (special-extend
         '(#x09BE #x09D7 #x0B3E #x0B57 #x0BBE #x0BD7 #x0CC2 #x0CD5 #x0CD6
           #x0D3E #x0D57 #x0DCF #x0DDF #x200C #x200D #x302E #x302F #xFF9E
           #xFF9F #x1D165 #x1D16E #x1D16F #x1D170 #x1D171 #x1D172))
        (not-spacing-mark
         '(#x102B #x102C #x1038 #x1062 #x1063 #x1064 #x1067 #x1068 #x1069
           #x106A #x106B #x106C #x106D #x1083 #x1087 #x1088 #x1089 #x108A
           #x108B #x108C #x108F #x109A #x109B #x109C #x19B0 #x19B1 #x19B2
           #x19B3 #x19B4 #x19B8 #x19B9 #x19BB #x19BC #x19BD #x19BE #x19BF
           #x19C0 #x19C8 #x19C9 #x1A61 #x1A63 #x1A64 #xAA7B)))
    (cond
      ((not char) nil)
      ((= cp 10) :LF)
      ((= cp 13) :CR)
      ((or (member gc '("Mn" "Me") :test #'string=)
           (member cp special-extend)) :extend)
      ((or (member gc '("Zl" "Zp" "Cc" "Cs" "Cf"))
           ;; From Cn and Default_Ignorable_Code_Point
           (member cp '(#x2065 #xE0000))
           (between #xFFF0 cp #xFFF8)
           (between #xE0002 cp #xE001F)
           (between #xE0080 cp #xE00FF)) :control)
      ((between #x1F1E6 cp #x1F1FF) :regional-indicator)
      ((and (or (string= gc "Mc")
                (member cp '(#x0E33 #x0EB3)))
            (not (member cp not-spacing-mark))) :spacing-mark)
      (t (hangul-syllable-type char)))))

(defun graphemes (string)
  #!+sb-doc
  "Breaks the given string into graphemes acording to the default
grapheme breaking rules specified in UAX #29"
  (let* ((chars (coerce string 'list)) clusters (cluster (list (car chars))))
    (do ((first (car chars) second)
         (tail (cdr chars) (when tail (cdr tail)))
         (second (cadr chars) (when tail (cadr tail))))
        ((not first) (nreverse (mapcar #'(lambda (l) (coerce l 'string)) clusters)))
      (flet ((brk () (push (nreverse cluster) clusters) (setf cluster (list second)))
             (nobrk () (push second cluster)))
        (let ((c1 (grapheme-break-type first))
              (c2 (grapheme-break-type second)))
          (cond
            ((and (eql c1 :cr) (eql c2 :lf)) (nobrk))
            ((or (member c1 '(:control :cr :lf))
                 (member c2 '(:control :cr :lf))) (brk))
             ((or (and (eql c1 :l) (member c2 '(:l :v :lv :lvt)))
                  (and (or (eql c1 :v) (eql c1 :lv))
                       (or (eql c2 :v) (eql c2 :t)))
                  (and (eql c2 :t) (or (eql c1 :lvt) (eql c1 :t))))
              (nobrk))
             ((and (eql c1 :regional-indicator) (eql c2 :regional-indicator)) (nobrk))
             ((or (eql c2 :extend) (eql c2 :spacing-mark) (eql c1 :prepend)) (nobrk))
             (t (brk))))))))
