(cond
    ((= 1 1) (format t "All good.~%"))
    ((= 2 2) (format t "Still good.~%")
             (format t "More goodness.~%"))
    ((= 3 3))
    (t (format t "Last one.~%")))

(cond 12)

(cond "Not a pair")
