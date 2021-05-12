(defvar *myvar* 3
  "this is documentation line, 3 is the initial value here"
)

(defun display(x) (format t "The variable here stores: ~d ~%" x))

(display 2)

(let ((x 10) (y 20) z)
  (display x)
  (display y)
  (display z)
)


(display *myvar*) ; value of a global variable

(defun increement()
  (format t "original value: ~d ~%" *myvar*)
  (setf *myvar* (+ 1 *myvar*))
  (format t "increemented value: ~d ~%" *myvar*)
)

(display *myvar*)
(increement)      ; increases the myvar value by 1
(display *myvar*)
