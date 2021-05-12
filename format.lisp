; without new line, also quotes are vanished
(format t "Hello ~%")                ; simple string
(format t "She replied \"Yes\" ~%")  ; escaping " (back slash)
(format t "This \\ is backslash ~%") ; escaping \ (back slash)


; Passing NIL to the first argument
(format nil "hello~%") ; line won't print anything rather return a string
; that string can then be printed, this is just for demonstration purpose
(format t (format nil "hello~%"))
