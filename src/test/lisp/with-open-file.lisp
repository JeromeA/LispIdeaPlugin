(with-open-file (stream "filename")
    (format stream "Hello, world!~%"))

(with-open-file invalid-stream)
(with-open-file (invalid-stream-specification))
