(multiple-value-bind (var1 var2) (values "value1" "value2")
    (format t "Values are ~a and ~a.~%" var1 var2))

(multiple-value-bind)

(multiple-value-bind (var))

(multiple-value-bind not-a-list "value")

(multiple-value-bind (var "not a variable") "value"
    var)
