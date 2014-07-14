(in-package #:sb-unicode-strings)

;;; Class definition
(defclass unicode-string (sequence standard-object)
  ;; Vector of strings (grapheme clusters)
  ((%vector :initarg :vector :accessor %vector))
  (:default-initargs :vector (sb-int:missing-arg)))

(defun %make-backing-array (length &rest args &key initial-element initial-contents)
  (declare (ignore initial-element initial-contents))
  (apply #'make-array length :element-type 'string :fill-pointer 0 args))

(declaim (ftype (function (string) unicode-string)
                string->unicode-string))
(defun string->unicode-string (string)
  "Converts STRING to a Unicode string"
  (make-instance
   'unicode-string
   :vector (graphemes (normalize-string string) :as-vector t)))

(declaim (ftype (function (unicode-string) string)
                string->unicode-string))
(defun unicode-string->string (uni-string)
  "Converts UNI-STRING to a ANSI Common LIsp string"
  (let ((ret (make-array (length uni-string) :element-type 'character
                         :adjustable t :fill-pointer 0)))
    (loop for cluster across (%vector uni-string) do
         (loop for char across cluster do (vector-push-extend char ret)))
    (coerce ret 'string)))


;;; Sequence protocal
(defmethod sb-sequence:length ((str unicode-string))
  (length (%vector str)))

(defmethod sb-sequence:elt ((str unicode-string) index)
  (aref (%vector str) index))

(defmethod (setf sb-sequence:elt) ((new-value character) (str unicode-string) index)
  (setf (aref (%vector str) index) (normalize-string (string new-value))))

(defmethod (setf sb-sequence:elt) ((new-value string) (str unicode-string) index)
  (let ((graphemes (graphemes (normalize-string new-value))))
    (when (rest graphemes)
      (error "New value ~s contains more than one grapheme" new-value))
    (setf (aref (%vector str) index) (first graphemes))))

(defmethod (setf sb-sequence:elt) (new-value (str unicode-string) index)
  (error 'type-error :expected-type '(or string character) :datum new-value))

(defmethod sb-sequence:adjust-sequence ((str unicode-string) length &rest args
                                        &key initial-element initial-contents)
  (declare (ignore initial-element initial-contents))
  (setf (%vector str) (apply #'adjust-array (%vector str) length args))
  str)

(defmethod sb-sequence:make-sequence-like ((str unicode-string) length &rest args
                                           &key initial-element initial-contents)
  (declare (ignore initial-element initial-contents))
  (make-instance 'unicode-string :vector
                 (apply #'%make-backing-array length args)))

(defmethod sb-sequence:canonize-test ((s unicode-string) test test-not)
  (declare (ignore s))
  (cond
    (test (if (functionp test) test (fdefinition test)))
    (test-not (if (functionp test-not)
                  (complement test-not)
                  (complement (fdefinition test-not))))
    (t #'(lambda (x y)
           (string=
            ;; If x or y are already strings, STRING does nothing.
            ;; If they're characters, makes them singleton strings.
            (normalize-string (string x))
            (normalize-string (string y)))))))


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
