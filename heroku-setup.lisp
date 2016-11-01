(in-package :cl-user)

(print ">>> Building system....")

(require 'asdf)
(load (merge-pathnames "web-util/web-util.asd" *build-dir*))
(load (merge-pathnames "ignorama.asd" *build-dir*))
(load (merge-pathnames "load.lisp" *build-dir*))

(defun heroku-toplevel ()
  (let ((port (parse-integer (heroku-getenv "PORT"))))
    (format t "Listening on port ~A~%" port)
    (funcall (symbol-function (find-symbol "START" (find-package "HUNCHENTOOT")))
             (setf hunchentoot:*acceptor* (funcall 'make-instance (find-symbol "EASY-ACCEPTOR" (find-package "HUNCHENTOOT")) :port port)))
    (setf (hunchentoot:acceptor-document-root hunchentoot:*acceptor*) "./"))
  (loop (sleep 60)))

(print ">>> Done building system")
