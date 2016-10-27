(in-package :cl-user)

(load "load.lisp")
(load "js/script.lisp")

(setf hunchentoot:*acceptor* (hunchentoot:start (make-instance 'hunchentoot:easy-acceptor :port 4242)))
(setf (hunchentoot:acceptor-document-root hunchentoot:*acceptor*) "./")
