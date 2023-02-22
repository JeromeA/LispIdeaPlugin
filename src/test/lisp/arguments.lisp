(defun f1 ())
(f1)
(f1 1)

(defun f2 (a))
(f2 1)
(f2)
(f2 1 2)

(defun f3 (a &optional b))
(f3 1)
(f3 1 2)
(f3)
(f3 1 2 3)

(defun f5 (a &rest body))
(f5 1)
(f5 1 2 3 4)

(defun f6 (a &key b c))
(f6 1 :b 2 :c 3)
(f6 :b 2)
(f6 1 :b 2 3)
