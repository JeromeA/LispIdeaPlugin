
(in-package #:lisp-idea-plugin)

(defun evaluate-expression (expression-string)
    "Evaluate the given expression string and return its results."
    ; TODO: add a package parameter that defaults to cl-user.
    (let ((error "") result stdout stderr)
        (with-input-from-string (input-stream expression-string)
            (setf stdout (with-output-to-string (*standard-output*)
                (setf stderr (with-output-to-string (*error-output*)
                    (setf result (multiple-value-list (handler-case (eval (read input-stream))
                                                          (error (e) (setf error e)
                                                                     "")))))))))
        (format nil "--Result--~%~{~A~%~}--Error--~%~A~%--stdout--~%~A~%--stderr--~%~A~%--~%" result error stdout stderr)))
