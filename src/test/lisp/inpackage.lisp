
(defpackage "PACKAGE1"
            (:use "CL"))

(in-package :package1)
(in-package #:package1)
(in-package "PACKAGE1")

(defun function1 () nil)
(function1)
(package1:function1)

(defun package1:function2 () nil)
(function2)
(package1:function2)
