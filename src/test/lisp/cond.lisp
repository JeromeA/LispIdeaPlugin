(cond
    ((= 1 1) (format t "All good.~%"))
    ((= 2 2) (format t "Still good.~%")
             (format t "More goodness.~%"))
    (t (format t "Last one.~%")))

(cond 12)

(cond "Not a pair")

(cond (still-not-a-pair))
