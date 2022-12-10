(defpackage "PACKAGE1"
            (:use "CL"))

(in-package :package1)
(in-package #:package1)
(in-package "PACKAGE1")

(defun function1 () nil)

(function1)
