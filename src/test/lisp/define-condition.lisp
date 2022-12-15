
(define-condition condition1 () ())

(define-condition condition2
    ()
    (slot1
     (slot2)
     (slot3 :reader myreader)))


(define-condition condition3
    (condition1 condition2)
    ())

(define-condition)
(define-condition missing-params)
(define-condition condition4 (missing-slots))
(define-condition 12 () ())
