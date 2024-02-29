
(defclass myclass1
          ()
          ((slot1 :reader slot1-reader)
           (slot2 :initarg :initslot2 :initform (+ 2 3))))

(defclass myclass2
          ()
          ((slot3 :initarg 12 )
           (slot4 :reader 13)))
