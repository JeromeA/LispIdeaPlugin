
(handler-case (eval (read))
    (error (e) (format t "Error: ~A" e)))

(handler-case)
(handler-case nil invalid-error-clause)
(handler-case nil ())
(handler-case nil (error))
(handler-case nil (error invalid-variable-definition))
(handler-case nil (error (invalid variable definition)))
