
(do-symbols (var))
(do-symbols (var :cl-user 0)
    (format t "~A~%" var))

(do-symbols)
(do-symbols invalid)
(do-symbols ())
(do-symbols (12))
