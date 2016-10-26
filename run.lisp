(in-package :cl-user)

(load "load.lisp")

(hunchentoot:start (make-instance 'hunchentoot:easy-acceptor :port 4242))
