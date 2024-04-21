
(in-package #:idea-server)

(defclass mixer-stream (fundamental-character-output-stream)
    ((stream :initarg :stream :reader stream-of)
     (prefix :initarg :prefix :reader prefix-of)
     (sufix :initarg :sufix :reader sufix-of)))

(defmethod stream-write-char ((stream mixer-stream) char)
    (write-string (prefix-of stream) (stream-of stream))
    (write-char char (stream-of stream))
    (write-string (sufix-of stream) (stream-of stream)))

(defmethod stream-write-string ((stream mixer-stream) string &optional start end)
    (write-string (prefix-of stream) (stream-of stream))
    (write-string string (stream-of stream) :start (or start 0) :end end)
    (write-string (sufix-of stream) (stream-of stream)))

(defun evaluate-expression (expression-string stream id)
    "Evaluate the given expression string and return its results."
    (let* (error
           result
           (*standard-output* (make-instance 'mixer-stream
                                             :stream stream
                                             :prefix (format nil "stdout~%")
                                             :sufix (format nil "~%--~A--~%" id)))
           (*error-output* (make-instance 'mixer-stream :stream stream
                                          :prefix (format nil "stderr~%")
                                          :sufix (format nil "~%--~A--~%" id))))
        (format stream "--~A--~%" id)
        (with-input-from-string (input-stream expression-string)
            (setf result (multiple-value-list (handler-case (eval (read input-stream))
                                                  (error (e) (setf error e)
                                                             "")))))
        (when error
            (format stream "error~%~A~%--~A--~%" error id))
        (format stream "result~%~{~A~%~}--~A--~%" result id)))
