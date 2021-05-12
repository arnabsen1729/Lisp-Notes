; without new line, also quotes are vanished
(format t "Hello ~%")                ; simple string
(format t "She replied \"Yes\" ~%")  ; escaping " (back slash)
(format t "This \\ is backslash ~%") ; escaping \ (back slash)


; Passing NIL to the first argument
(format nil "hello~%") ; line won't print anything rather return a string
; that string can then be printed, this is just for demonstration purpose
(format t (format nil "hello~%"))

; using the ~a directive

(format t "The value is: ~a ~%" 10)
(format t "The value is: ~a ~%" "foo")
(format t "The value is: ~a ~%" (list 1 2 3))

; using the ~s directive

(format t "The value is: ~s ~%" 10)
(format t "The value is: ~s ~%" "foo")
(format t "The value is: ~s ~%" (list 1 2 3))

; using the ~d directive

(format t "The value is: ~d ~%" 10000000)
(format t "The value is: ~:d ~%" 10000000)

; using the ~f directive

(format t "The value of PI is: ~f ~%" pi)
(format t "The value of PI is: ~,4f ~%" pi)
(format t "The value of PI is: ~e ~%" pi)
