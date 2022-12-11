(with-input-from-string (stream "content")
    (read stream))

(with-input-from-string invalid-stream)
(with-input-from-string (invalid-stream-specification))
