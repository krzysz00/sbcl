(in-package #:sb-unicode-strings)

(defclass unicode-string (sequence)
  ((vector :initarg :vector :accessor %vector)))

(defun string->unicode-string (string)
  (make-instance
   'unicode-string
   :vector (graphemes (normalize-string string) :as-vector t)))

(defun unicode-string->string (uni-string)
  (let ((ret (make-array (length uni-string) :element-type 'character
                         :adjustable t :fill-pointer 0)))
    (loop for cluster in (%vector uni-string) do
         (loop for char in cluster do (vector-push-extend char ret)))
    (coerce ret 'string)))

(defmethod sb-sequence:length ((str unicode-string))
  (sb-sequence:length (%vector str)))

(defmethod sb-sequence:elt ((str unicode-string) index)
  (aref (%vector str) index))

(defmethod (setf sb-sequence:elt) ((new-value character) (str unicode-string) index)
  (setf (aref (%vector str) index) (coerce (list new-value) 'string)))

(defmethod (setf sb-sequence:elt) ((new-value string) (str unicode-string) index)
  (setf (aref (%vector str) index) new-value))

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

(defmethod print-object ((object unicode-string) stream)
  (print (%vector object) stream))
