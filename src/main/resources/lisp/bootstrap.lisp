;;;; Loads and starts the Lisp Idea server from its system definition, which is assumed to be in the same directory.

(asdf:load-asd (merge-pathnames "idea-server.asd"
                                (make-pathname :directory (pathname-directory *load-truename*))))
(asdf:load-system :idea-server)

