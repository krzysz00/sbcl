(defpackage :sb-unicode-strings
  (:use :cl :sb-unicode)
  (:export
   #:string->unicode-string
   #:unicode-string->string
   #:unicode-string
   #:enable-unicode-string-syntax))
