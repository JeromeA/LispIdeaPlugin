
(in-package #:lisp-idea-plugin)

(defparameter *server-address* '(127 0 0 1))
(defparameter *server-port* 0)

(defun get-tcp-port (socket)
    (nth-value 1 (sb-bsd-sockets:socket-name socket)))

(defun make-server-socket ()
    (let ((socket (make-instance 'sb-bsd-sockets:inet-socket
                                 :type :stream :protocol :tcp)))
        (setf (sb-bsd-sockets:sockopt-reuse-address socket) t)
        (sb-bsd-sockets:socket-bind socket *server-address* *server-port*)
        (sb-bsd-sockets:socket-listen socket 1)
        (format t "Server is listening on port ~A~%" (get-tcp-port socket))
        socket))

(defun accept-stream (socket)
    "Accept a connection from the given socket and return it as a stream."
    (sb-bsd-sockets:socket-make-stream (sb-bsd-sockets:socket-accept socket) :input t :output t :buffering :none))

(defun read-until-separator (stream separator)
    (format nil "~{~a~%~}"
            (loop for line = (read-line stream)
                  while (not (equal line separator))
                  collect line)))

(defun repl (stream)
    (loop (write-string (evaluate-expression (read-until-separator stream "--"))
                        stream)))

(defun run-server ()
    (let ((socket (make-server-socket)))
        (loop (let ((stream (accept-stream socket)))
                  (format t "Handling incoming stream~%")
                  (handler-case
                      (loop (repl stream))
                      (error (e) (format t "Error: ~A~%" e)))))))
