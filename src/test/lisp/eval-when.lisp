
(eval-when (:compile-toplevel :load-toplevel :execute)
    (format t "Executed.~%"))

(eval-when)
(eval-when 12)
(eval-when (:other-situation))
