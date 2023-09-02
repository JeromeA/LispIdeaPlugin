(defpackage #:my-package
    (:use #:cl)
    (:export :function3 #:function3 "FUNCTION3"))

(defun function3 ())

(in-package #:my-package)

(defpackage)
(defpackage #:my-package1
    (use #:cl)
    (:export 12))
