(defun hello-word() (format t "Hello World ~%"))

(hello-word)

(defun sum(x y)
  "This is a documentation line, the sum function takes two arg and returns their sum"
  (format t "Sum of ~d and ~d is ~d ~%" x y (+ x y))
  (+ x y) ; sum will be returned
)

(format t "Calling sum fun returns: ~d ~%" (sum 1 2))
