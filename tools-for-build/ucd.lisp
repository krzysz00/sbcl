(in-package "SB-COLD")

;;; Common functions

(defparameter *output-directory*
  (merge-pathnames
   (make-pathname :directory '(:relative :up "output"))
   (make-pathname :directory (pathname-directory *load-truename*))))

(defun split-string (line character)
  (loop for prev-position = 0 then (1+ position)
     for position = (position character line :start prev-position)
     collect (subseq line prev-position position)
     do (unless position
          (loop-finish))))

(defun parse-codepoints (string &key (singleton-list t))
  "Gets a list of codepoints out of 'aaaa bbbb cccc', stripping surrounding space"
  (let ((list (mapcar
              (lambda (s) (parse-integer s :radix 16))
              (remove "" (split-string string #\Space) :test #'string=))))
    (if (not (or (cdr list) singleton-list)) (car list) list)))


(defun parse-codepoint-range (string)
  "Parse the Unicode syntax DDDD|DDDD..DDDD into an inclusive range (start end)"
  (destructuring-bind (start &optional empty end) (split-string string #\.)
    (let* ((head (parse-integer start :radix 16))
           (tail (if end
                     (parse-integer end :radix 16 :end (position #\Space end))
             head)))
      (list head tail))))

(defun init-indices (strings)
  (let ((hash (make-hash-table :test #'equal)))
    (loop for string in strings
       for index from 0
       do (setf (gethash string hash) index))
    hash))

(defun clear-flag (bit integer)
  (logandc2 integer (ash 1 bit)))


;;; Output storage globals
(defstruct ucd misc decomp)

(defparameter *unicode-character-database*
  (make-pathname :directory (pathname-directory *load-truename*)))

(defparameter *unicode-names* (make-hash-table))

(defparameter *decompositions*
  (make-array 10000 :element-type '(unsigned-byte 24) :fill-pointer 0
              :adjustable t)) ; 10000 is not a significant number

(defparameter *decomposition-corrections*
  (with-open-file (s (make-pathname :name "NormalizationCorrections" :type "txt"
                                    :defaults *unicode-character-database*))
    (loop with result = nil
       for line = (read-line s nil nil) while line
       do (when (position #\; line)
            (destructuring-bind (cp old-decomp correction version)
                (split-string line #\;)
              (declare (ignore old-decomp version))
              (push (cons (parse-integer cp :radix 16)
                          (parse-integer correction :radix 16))
                    result)))
       finally (return result)))
  "List of decompsotions that were amended in Unicode corrigenda")

(defparameter *compositions* (make-hash-table :test #'equal))
(defparameter *composition-exclusions*
  (with-open-file (s (make-pathname :name "CompositionExclusions" :type "txt"
                                    :defaults *unicode-character-database*))
    (loop with result = nil
       for line = (read-line s nil nil) while line
       when (and (> (length line) 0) (char/= (char line 0) #\#))
       do (push (parse-integer line :end (position #\Space line) :radix 16)
                result) finally (return result)))
  "Characters that are excluded from composition according to UAX#15")

(defparameter *different-titlecases* nil)
(defparameter *different-casefolds* nil)

(defparameter *case-mapping*
  (with-open-file (s (make-pathname :name "SpecialCasing" :type "txt"
                                    :defaults *unicode-character-database*))
    (loop with hash = (make-hash-table)
       for line = (read-line s nil nil) while line
       unless (or (not (position #\# line)) (= 0 (position #\# line)))
       do (destructuring-bind (%cp %lower %title %upper &optional context comment)
              (split-string line #\;)
            (unless (and context comment)
              (let ((cp (parse-integer %cp :radix 16))
                    (lower (parse-codepoints %lower :singleton-list nil))
                    (title (parse-codepoints %title :singleton-list nil))
                    (upper (parse-codepoints %upper :singleton-list nil)))
                (setf (gethash cp hash) (cons upper lower))
                (unless (equal title upper) (push (cons cp title) *different-titlecases*)))))
         finally (return hash)))
  "Maps cp -> (cons uppercase|(uppercase ...) lowercase|(lowercase ...))")

(defparameter *misc-table* (make-array 400 :fill-pointer 0)
"Holds the entries in the Unicode database's miscellanious array, stored as lists.
These lists have the form (gc-index bidi-index ccc digit decomposition-info flags).
Flags is a bit-bashed integer containing cl-both-case-p, has-case-p, and
bidi-mirrored-p. Length should be adjusted when the standard changes.")
(defparameter *misc-hash* (make-hash-table :test #'equal)
"Maps a misc list to its position in the misc table.")

(defparameter *different-numerics* nil)

(defparameter *ucd-entries* (make-hash-table))

;; Mappings of the general categories and bidi classes to integers
;; Letter classes go first to optimize certain cl character type checks
;; BN is the first BIDI class so that unallocated characters are BN
;; Uppercase in the CL sense must have GC = 0, lowercase must GC = 1
(defparameter *general-categories*
  (init-indices '("Lu" "Ll" "Lt" "Lm" "Lo" "Cc" "Cf" "Co" "Cs" "Mc"
                  "Me" "Mn" "Nd" "Nl" "No" "Pc" "Pd" "Pe" "Pf" "Pi"
                  "Po" "Ps" "Sc" "Sk" "Sm" "So" "Zl" "Zp" "Zs")))
(defparameter *bidi-classes*
  (init-indices '("BN" "AL" "AN" "B" "CS" "EN" "ES" "ET" "L" "LRE" "LRO"
                  "NSM" "ON" "PDF" "R" "RLE" "RLO" "S" "WS" "LRI" "RLI"
                  "FSI" "PDI")))

(defvar *block-first* nil)


;;; Unicode data file parsing
(defun hash-misc (gc-index bidi-index ccc digit decomposition-info flags)
  (let* ((list (list gc-index bidi-index ccc digit decomposition-info flags))
         (index (gethash list *misc-hash*)))
    (or index
        (progn
          (setf (gethash list *misc-hash*)
                (fill-pointer *misc-table*))
          (vector-push list *misc-table*)))))

(defun complete-misc-table ()
  (let* ((unallocated-misc
          ;; unallocated characters have a GC index of 31 (not colliding
          ;; with any other GC), aren't digits, aren't interestingly bidi,
          ;; and don't decompose, combine, or have case.
          '(31 0 0 128 0 0))
         (unallocated-index (apply #'hash-misc unallocated-misc))
         (unallocated-ucd (make-ucd :misc unallocated-index :decomp 0)))
    (loop for code-point from 0 to #x10FFFF do ; Flood-fil unallocated codepoints
         (unless (second (multiple-value-list (gethash code-point *ucd-entries*)))
           (setf (gethash code-point *ucd-entries*) unallocated-ucd)))))

(defun fixup-compositions ()
  (flet ((fixup (k v)
           (declare (ignore v))
           (let* ((cp (car k))
                  (ucd (gethash cp *ucd-entries*))
                  (misc (aref *misc-table* (ucd-misc ucd)))
                  (ccc (third misc)))
             ;; we can do everything in the first pass except for
             ;; accounting for decompositions where the first
             ;; character of the decomposition is not a starter.
             (when (/= ccc 0)
               (remhash k *compositions*)))))
    (maphash #'fixup *compositions*)))

(defun add-jamo-information (line table)
  (let* ((split (split-string line #\;))
         (code (parse-integer (first split) :radix 16))
         (syllable (string-trim '(#\Space)
                                (subseq (second split) 0 (position #\# (second split))))))
    (setf (gethash code table) syllable)))

(defun fixup-hangul-syllables ()
  ;; "Hangul Syllable Composition, Unicode 5.1 section 3-12"
  (let* ((sbase #xac00)
         (lbase #x1100)
         (vbase #x1161)
         (tbase #x11a7)
         (scount 11172)
         (lcount 19)
         (vcount 21)
         (tcount 28)
         (ncount (* vcount tcount))
         (table (make-hash-table)))
    (declare (ignore lcount))
    (with-open-file (*standard-input*
                     (make-pathname :name "Jamo" :type "txt"
                                    :defaults *unicode-character-database*))
      (loop for line = (read-line nil nil)
            while line
            if (position #\; line)
            do (add-jamo-information line table)))
    (dotimes (sindex scount)
      (let* ((l (+ lbase (floor sindex ncount)))
             (v (+ vbase (floor (mod sindex ncount) tcount)))
             (tee (+ tbase (mod sindex tcount)))
             (code-point (+ sbase sindex))
             (name (format nil "HANGUL_SYLLABLE_~A~A~:[~A~;~]"
                           (gethash l table) (gethash v table)
                           (= tee tbase) (gethash tee table))))
        (setf (gethash code-point *unicode-names*) name)))))

(defun normalize-character-name (name)
  (when (find #\_ name)
    (error "Bad name for a character: ~A" name))
  (unless (or (zerop (length name)) (find #\< name) (find #\> name))
    (substitute #\_ #\Space name)))

;;;   3400  --  4DB5  : cjk ideograph extension a ;Lo;0;L;;;;;N;;;;;
;;;   AC00  --  D7A3  : hangul syllables ;Lo;0;L;;;;;N;;;;;
;;;   D800  --  F8FF  : surrogates and private use
;;;  20000  --  2A6D6 : cjk ideograph extension b ;Lo;0;L;;;;;N;;;;;
;;;  F0000  --  FFFFD : private use
;;; 100000  --  10FFFD: private use
(defun encode-ucd-line (line code-point)
  (destructuring-bind (name general-category canonical-combining-class
                            bidi-class decomposition-type-and-mapping
                            decimal-digit digit numeric bidi-mirrored
                            unicode-1-name iso-10646-comment simple-uppercase
                            simple-lowercase simple-titlecase)
      line
    (declare (ignore unicode-1-name iso-10646-comment))
    (if (and (> (length name) 8)
             (string= ", First>" name :start2 (- (length name) 8)))
        (progn
          (setf *block-first* code-point)
          nil)
        (let* ((gc-index (or (gethash general-category *general-categories*)
                             (error "unknown general category ~A"
                                    general-category)))
               (bidi-index (or (gethash bidi-class *bidi-classes*)
                               (error "unknown bidirectional class ~A"
                                      bidi-class)))
               (ccc (parse-integer canonical-combining-class))
               (digit-index (if (string= "" digit) 128 ; non-digits have high bit
                                (let ((%digit (parse-integer digit)))
                                  (if (string= digit decimal-digit)
                                      ;; decimal-digit-p is in bit 6
                                      (+ (ash 1 6) %digit) %digit))))
               (upper-index (unless (string= "" simple-uppercase)
                              (parse-integer simple-uppercase :radix 16)))
               (lower-index (unless (string= "" simple-lowercase)
                              (parse-integer simple-lowercase :radix 16)))
               (title-index (unless (string= "" simple-titlecase)
                              (parse-integer simple-titlecase :radix 16)))
               (cl-both-case-p (or (and (= gc-index 0) lower-index)
                                   (and (= gc-index 1) upper-index)))
               (bidi-mirrored-p (string= bidi-mirrored "Y"))
               (decomposition-info 0)
               (decomposition-index 0))
          (when (and (not cl-both-case-p)
                     (< gc-index 2))
            (format t "~A~%" name))

          (when (string/= "" decomposition-type-and-mapping)
            (let* ((compatibility-p (position #\> decomposition-type-and-mapping))
                   (decomposition
                    (parse-codepoints
                     (subseq decomposition-type-and-mapping
                             (if compatibility-p (1+ compatibility-p) 0)))))
              (when (assoc code-point *decomposition-corrections*)
                (setf decomposition
                      (list (cdr (assoc code-point *decomposition-corrections*)))))
              (setf decomposition-info
                    (+ (length decomposition) (if compatibility-p 128 0)))
              (unless (logbitp 7 decomposition-info)
                ;; Primary composition excludes:
                ;; * singleton decompositions;
                ;; * decompositions of non-starters;
                ;; * script-specific decompositions;
                ;; * later-version decompositions;
                ;; * decompositions whose first character is a
                ;;   non-starter.
                ;; All but the last case can be handled here;
                ;; for the fixup, see FIXUP-COMPOSITIONS
                (when (and (> decomposition-info 1)
                           (= ccc 0)
                           (not (member code-point *composition-exclusions*)))
                  (unless (= decomposition-info 2)
                    (error "canonical decomposition unexpectedly long"))
                  (setf (gethash (cons (first decomposition)
                                       (second decomposition))
                                 *compositions*)
                        code-point)))
              (setf decomposition-index
                    (prog1
                        (fill-pointer *decompositions*)
                      (loop for i in decomposition do
                           (vector-push-extend i *decompositions*))))))
          ;; Hangul decomposition; see Unicode 6.2 section 3-12
          (when (= code-point #xd7a3)
            ;; KLUDGE: The decomposition-length for Hangul syllables in the
            ;; misc database will be a bit of a lie. It doesn't really matter
            ;; since the only purpose of the length is to index into the
            ;; decompositions array (which Hangul decomposition doesn't use).
            ;; The decomposition index is 0 because we won't be going into the
            ;; array
            (setf decomposition-info 3))

          (unless (gethash code-point *case-mapping*) ; Exclude codepoints from SpecialCasing
            (when (string/= simple-uppercase simple-titlecase)
              (push (cons code-point title-index) *different-titlecases*))
            (and (or upper-index lower-index)
                 (setf (gethash code-point *case-mapping*)
                       (cons
                        (or upper-index code-point)
                        (or lower-index code-point)))))

          (when (string/= digit numeric)
            (push (cons code-point numeric) *different-numerics*))

          (when (> ccc 255)
            (error "canonical combining class too large ~A" ccc))
          (let* ((flags (+ (if cl-both-case-p (ash 1 7) 0)
                           (if (gethash code-point *case-mapping*) (ash 1 6) 0)
                           (if bidi-mirrored-p (ash 1 5) 0)))
                 (misc-index (hash-misc gc-index bidi-index ccc digit-index
                                        decomposition-info flags))
                 (result (make-ucd :misc misc-index
                                   :decomp decomposition-index)))
            (when (and (> (length name) 7)
                       (string= ", Last>" name :start2 (- (length name) 7)))
              (loop for point from *block-first* to code-point do
                   (setf (gethash point *ucd-entries*) result)))
            (values result (normalize-character-name name)))))))

(defun slurp-ucd-line (line)
  (let* ((split-line (split-string line #\;))
         (code-point (parse-integer (first split-line) :radix 16)))
    (multiple-value-bind (encoding name)
        (encode-ucd-line (cdr split-line) code-point)
      (setf (gethash code-point *ucd-entries*) encoding
            (gethash code-point *unicode-names*) name))))

;;; this fixes up the case conversion discrepancy between CL and
;;; Unicode: CL operators depend on char-downcase / char-upcase being
;;; inverses, which is not true in general in Unicode even for
;;; characters which change case to single characters.
(defun second-pass ()
  (loop for code-point being the hash-keys in *case-mapping*
     using (hash-value (upper . lower))
     for misc-index = (ucd-misc (gethash code-point *ucd-entries*))
     for (gc bidi ccc digit decomp flags) = (aref *misc-table* misc-index)
     when (logbitp 7 flags) do
       (when (or (not (atom upper)) (not (atom lower))
                 (and (= gc 0)
                      (not (equal (car (gethash lower *case-mapping*)) code-point)))
                 (and (= gc 1)
                      (not (equal (cdr (gethash upper *case-mapping*)) code-point))))
         (let* ((new-flags (clear-flag 7 flags))
                (new-misc (hash-misc gc bidi ccc digit decomp new-flags)))
           (setf (ucd-misc (gethash code-point *ucd-entries*)) new-misc)))))

(defun fixup-casefolding ()
  (with-open-file (s (make-pathname :name "CaseFolding" :type "txt"
                                    :defaults *unicode-character-database*))
    (loop for line = (read-line s nil nil)
       while line
       unless (or (not (position #\; line)) (equal (position #\# line) 0))
       do (destructuring-bind (original type mapping comment)
              (split-string line #\;)
            (let ((cp (parse-integer original :radix 16))
                  (fold (parse-codepoints mapping :singleton-list nil)))
              (declare (ignore comment))
              (unless (or (string= type " S") (string= type " T"))
                (when (not (equal (cdr (gethash cp *case-mapping*)) fold))
                  (push (cons cp fold) *different-casefolds*))))))))

(defun slurp-ucd ()
  (with-open-file (*standard-input*
                   (make-pathname :name "UnicodeData"
                                  :type "txt"
                                  :defaults *unicode-character-database*)
                   :direction :input)
    (loop for line = (read-line nil nil)
          while line
          do (slurp-ucd-line line)))
  (second-pass)
  (fixup-compositions)
  (fixup-hangul-syllables)
  (complete-misc-table)
  (fixup-casefolding)
  nil)


;;; PropList.txt
(defparameter **proplist-properties** nil
  "A list of properties extracted from PropList.txt")

(defun parse-property (stream &optional name)
  (let (result)
    (loop for line = (read-line stream nil nil)
       while (and line (not (position #\= line)))
       for entry = (subseq line 0 (position #\# line))
       when (and entry (string/= entry ""))
       do (push (parse-codepoint-range (car (split-string entry #\;))) result))
    (setf result (nreverse result))
    (when name
      (push name **proplist-properties**)
      (push result **proplist-properties**))))

(defun slurp-proplist ()
  (with-open-file (s (make-pathname :name "PropList"
                                    :type "txt"
                                    :defaults *unicode-character-database*)
                     :direction :input)
    (parse-property s) ;; Initial comments
    (parse-property s :whitespace)
    (parse-property s) ;; Bidi_Control
    (parse-property s) ;; Join_Control
    (parse-property s) ;; Dash
    (parse-property s) ;; Hyphen
    (parse-property s) ;; Quotation_Mark
    (parse-property s) ;; Terminal_Punctuation
    (parse-property s) ;; Other_Math
    (parse-property s) ;; Hex_Digit
    (parse-property s) ;; ASCII_Hex_Digit
    (parse-property s :other-alphabetic)
    (parse-property s :ideographic)
    (parse-property s) ;; Diacritic
    (parse-property s) ;; Extender
    (parse-property s :other-lowercase)
    (parse-property s :other-uppercase)
    (parse-property s) ;; Noncharacter_code_point
    (parse-property s :other-grapheme-extend)
    (parse-property s) ;; IDS_Binary_Operator
    (parse-property s) ;; IDS_Trinary_Operator
    (parse-property s) ;; Radical
    (parse-property s) ;; Unified_Ideograph
    (parse-property s) ;; Other_Default_Ignorable_Code_Point
    (parse-property s) ;; Deprecated
    (parse-property s) ;; Soft_Dotted
    (parse-property s) ;; Logical_Order_Exception
    (parse-property s) ;; Other_ID_Start
    (parse-property s) ;; Other_ID_Continue
    (parse-property s :sterm)
    (parse-property s) ;; Variation_Selector
    (parse-property s) ;; Pattern_White_Space
    (parse-property s) ;; Pattern_Syntax
    (setf **proplist-properties** (nreverse **proplist-properties**))
    (values)))


;;; Output code
(defun write-codepoint (code-point stream)
  (write-byte (ldb (byte 8 16) code-point) stream)
  (write-byte (ldb (byte 8 8) code-point) stream)
  (write-byte (ldb (byte 8 0) code-point) stream))

(defun output-misc-data ()
  (with-open-file (stream (make-pathname :name "ucdmisc"
                                         :type "dat"
                                         :defaults *output-directory*)
                          :direction :output
                          :element-type '(unsigned-byte 8)
                          :if-exists :supersede
                          :if-does-not-exist :create)
    (loop for (gc-index bidi-index ccc digit decomposition-info flags)
       across *misc-table*
       ;; three bits spare here
       do (write-byte gc-index stream)
       ;; three bits spare here
         (write-byte bidi-index stream)
         (write-byte ccc stream)
       ;; bits 0-3 encode [0,9], bit 7 is for non-digit status,
       ;; bit 6 is the decimal-digit flag. Two bits spare
         (write-byte digit stream)
         (write-byte decomposition-info stream)
         (write-byte flags stream)))) ; 5 bits still available for flags

(defun output-ucd-data ()
  (with-open-file (high-pages (make-pathname :name "ucdhigh"
                                             :type "dat"
                                             :defaults *output-directory*)
                              :direction :output
                              :element-type '(unsigned-byte 8)
                              :if-exists :supersede
                              :if-does-not-exist :create)
    (with-open-file (low-pages (make-pathname :name "ucdlow"
                                              :type "dat"
                                              :defaults *output-directory*)
                               :direction :output
                               :element-type '(unsigned-byte 8)
                               :if-exists :supersede
                               :if-does-not-exist :create)
      ;; Output either the index into the misc array (if all the points in the
      ;; high-page have the same misc value) or an index into the law-pages
      ;; array / 256. For indexes into the misc array, set bit 15 (high bit).
      ;; We should never have that many misc entries, so that's not a problem.

      ;; If Unicode ever allocates an all-decomposing <First>/<Last> block (the
      ;; only way to get a high page that outputs as the same and has a
      ;; non-zero decomposition-index, which there's nowhere to store now),
      ;; find me, slap me with a fish, and have fun fixing this mess.
      (loop with low-pages-index = 0
         for high-page from 0 to (ash #x10FFFF -8)
         for uniq-ucd-entries = nil do
           (loop for low-page from 0 to #xFF do
                (pushnew
                 (gethash (+ low-page (ash high-page 8)) *ucd-entries*)
                 uniq-ucd-entries :test #'equalp))
           (flet ((write-2-byte (int stream)
                    (write-byte (ldb (byte 8 8) int) stream)
                    (write-byte (ldb (byte 8 0) int) stream)))
             (case (length uniq-ucd-entries)
               (0 (error "Somehow, a high page has no codepoints in it."))
               (1 (write-2-byte (+ (ash 1 15) (ucd-misc (car uniq-ucd-entries))) high-pages))
               (t (loop for low-page from 0 to #xFF
                     for cp = (+ low-page (ash high-page 8))
                     for entry = (gethash cp *ucd-entries*) do
                       (write-2-byte (ucd-misc entry) low-pages)
                       (write-2-byte (ucd-decomp entry) low-pages)
                     finally (write-2-byte low-pages-index high-pages)
                       (incf low-pages-index)))))
         finally (assert (< low-pages-index (ash 1 15))) (print low-pages-index)))))

(defun output-decomposition-data ()
  (with-open-file (stream (make-pathname :name "decomp" :type "dat"
                                         :defaults *output-directory*)
                          :direction :output
                          :element-type '(unsigned-byte 8)
                          :if-exists :supersede
                          :if-does-not-exist :create)
    (loop for cp across *decompositions* do
         (write-codepoint cp stream)))
  (print (length *decompositions*)))

(defun output-composition-data ()
  (with-open-file (stream (make-pathname :name "comp" :type "dat"
                                         :defaults *output-directory*)
                          :direction :output
                          :element-type '(unsigned-byte 8)
                          :if-exists :supersede :if-does-not-exist :create)
    (maphash (lambda (k v)
               (write-codepoint (car k) stream)
               (write-codepoint (cdr k) stream)
               (write-codepoint v stream))
             *compositions*)))

(defun output-case-data ()
  (let (casing-pages points-with-case)
    (with-open-file (stream (make-pathname :name "case" :type "dat"
                                           :defaults *output-directory*)
                            :direction :output
                            :element-type '(unsigned-byte 8)
                            :if-exists :supersede :if-does-not-exist :create)
      (loop for cp being the hash-keys in *case-mapping*
           do (push cp points-with-case))
      (setf points-with-case (sort points-with-case #'<))
      (loop for cp in points-with-case
         for (upper . lower) = (gethash cp *case-mapping*) do
           (pushnew (ash cp -6) casing-pages)
           (write-codepoint cp stream)
           (write-byte (if (atom upper) 0 (length upper)) stream)
           (if (atom upper) (write-codepoint upper stream)
               (map 'nil (lambda (c) (write-codepoint c stream)) upper))
           (write-byte (if (atom lower) 0 (length lower)) stream)
           (if (atom lower) (write-codepoint lower stream)
               (map 'nil (lambda (c) (write-codepoint c stream)) lower))))
    (setf casing-pages (sort casing-pages #'<))
    (with-open-file (stream (make-pathname :name "casepages" :type "lisp-expr"
                                           :defaults *output-directory*)
                            :direction :output
                            :if-exists :supersede :if-does-not-exist :create)
      (with-standard-io-syntax
        (let ((*print-pretty* t)) (print casing-pages stream))))))

(defun output ()
  (output-misc-data)
  (output-ucd-data)
  (output-decomposition-data)
  (output-composition-data)
  (output-case-data)
  (with-open-file (*standard-output*
                   (make-pathname :name "misc-properties" :type "lisp-expr"
                                  :defaults *output-directory*)
                   :direction :output
                   :if-exists :supersede
                   :if-does-not-exist :create)
    (with-standard-io-syntax
      (let ((*print-pretty* t))
        (prin1 **proplist-properties**))))
  (with-open-file (f (make-pathname :name "ucd-names" :type "lisp-expr"
                                    :defaults *output-directory*)
                     :direction :output
                     :if-exists :supersede
                     :if-does-not-exist :create)
    (with-standard-io-syntax
      (write-string ";;; Do not edit by hand: generated by ucd.lisp" f)
      (maphash (lambda (code name)
                 (when name
                   (print code f)
                   (prin1 name f)))
               *unicode-names*))
    (setf *unicode-names* nil))
  (with-open-file (*standard-output*
                   (make-pathname :name "numerics"
                                  :type "lisp-expr"
                                  :defaults *output-directory*)
                   :direction :output
                   :if-exists :supersede
                   :if-does-not-exist :create)
    (with-standard-io-syntax
      (let ((*print-pretty* t))
        (prin1 (mapcar #'(lambda (x) (cons (car x) (read-from-string (cdr x))))
                       *different-numerics*)))))
  (with-open-file (*standard-output*
                   (make-pathname :name "titlecases"
                                  :type "lisp-expr"
                                  :defaults *output-directory*)
                   :direction :output
                   :if-exists :supersede
                   :if-does-not-exist :create)
    (with-standard-io-syntax
      (let ((*print-pretty* t))
        (prin1 *different-titlecases*))))
  (with-open-file (*standard-output*
                   (make-pathname :name "foldcases"
                                  :type "lisp-expr"
                                  :defaults *output-directory*)
                   :direction :output
                   :if-exists :supersede
                   :if-does-not-exist :create)
    (with-standard-io-syntax
      (let ((*print-pretty* t))
        (prin1 *different-casefolds*))))
  (values))
