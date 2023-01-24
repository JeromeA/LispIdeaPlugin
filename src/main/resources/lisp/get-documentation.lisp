
(in-package #:lisp-idea-plugin)

(defun get-documentation ()
    "Scan and display the documentation of all the symbols exported by COMMON-LISP package."
    (do-external-symbols (symbol :cl)
        (let ((description (with-output-to-string (*standard-output*)
                               (describe symbol))))
            (when (search "names a compiled function:" description)
                (format t "~A====~%" description)))))
