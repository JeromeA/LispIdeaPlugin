
#+sbcl
(format t "Hello, sbcl!~%")

#+(or :sbcl :clisp)
(format t "I see you!~%")
