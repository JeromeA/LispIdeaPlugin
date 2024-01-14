
(defpackage "PACKAGE11" (:use "CL"))
(defpackage "PACKAGE12" (:use "CL"))
(defpackage "PACKAGE13" (:use "CL"))

(in-package :package11)

(defun function4 () nil)
(function4)

(in-package #:package12)

(defun function4 () nil)
(function4)
(package11:function4)
