(in-package #:sb-unicode-strings)

;;; Class definition
(defclass unicode-string (sequence standard-object)
  ((%vector :initarg :vector :accessor %vector)))

(defun string->unicode-string (string)
  "Converts STRING to a Unicode string"
  (make-instance
   'unicode-string
   :vector (graphemes (normalize-string string) :as-vector t)))

(defun unicode-string->string (uni-string)
  "Converts UNI-STRING to a ANSI Common LIsp string"
  (let ((ret (make-array (length uni-string) :element-type 'character
                         :adjustable t :fill-pointer 0)))
    (loop for cluster across (%vector uni-string) do
         (loop for char across cluster do (vector-push-extend char ret)))
    (coerce ret 'string)))


;;; Sequence protocal
(defmethod sb-sequence:length ((str unicode-string))
  (sb-sequence:length (%vector str)))

(defmethod sb-sequence:elt ((str unicode-string) index)
  (aref (%vector str) index))

(defmethod (setf sb-sequence:elt) ((new-value character) (str unicode-string) index)
  (setf (aref (%vector str) index) (normalize-string (coerce (list new-value) 'string))))

(defmethod (setf sb-sequence:elt) ((new-value string) (str unicode-string) index)
  (let ((value (graphemes (normalize-string new-value))))
    (when (cdr value) (error "New value ~s contains more than one grapheme" new-value))
    (setf (aref (%vector str) index) (car value))))

(defmethod (setf sb-sequence:elt) (new-value (str unicode-string) index)
  (error 'type-error :expected-type '(or string character) :datum new-value))

(defmethod sb-sequence:adjust-sequence ((str unicode-string) length &rest args &key initial-element initial-contents)
  (declare (ignore initial-element initial-contents))
  (setf
   (%vector str)
   (apply #'sb-sequence:adjust-sequence (%vector str) length args)))

(defmethod sb-sequence:make-sequence-like ((str unicode-string) length &rest args &key initial-element initial-contents)
  (declare (ignore initial-element initial-contents))
  (make-instance
   'unicode-string
   :vector
   (apply #'sb-sequence:make-sequence-like (%vector str) length args)))

(defmethod sb-sequence:canonize-test ((s unicode-string) test test-not)
  (declare (ignore s))
  (cond
    (test (if (functionp test) test (fdefinition test)))
    (test-not (if (functionp test-not)
                  (complement test-not)
                  (complement (fdefinition test-not))))
    (t #'(lambda (x y)
           (when (characterp x) (setf x (coerce (list x) 'string)))
           (when (characterp y) (setf y (coerce (list y) 'string)))
           (string= (normalize-string x) (normalize-string y))))))


;;; Input and output
(defmethod print-object ((object unicode-string) stream)
  (format stream "#U~s" (unicode-string->string object)))

(defun unicode-string-reader (stream subchar arg)
  (declare (ignore subchar arg))
  (let ((string (read stream t t t)))
    (string->unicode-string string)))

(defun enable-unicode-string-syntax (readtable)
  "Adds the #U\"string\" syntax to READTABLE"
  (set-dispatch-macro-character #\# #\U #'unicode-string-reader readtable))
(enable-unicode-string-syntax *readtable*)

(defmethod make-load-form ((self unicode-string) &optional environment)
  (make-load-form-saving-slots self :environment environment))
