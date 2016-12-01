(in-package :cl-user)

(load "load.lisp")
(load "env.lisp")

(defun r ()
  (load "load.lisp"))

(setf hunchentoot:*acceptor* (hunchentoot:start (make-instance 'hunchentoot:easy-acceptor
                                                               :port net.ignorama.web:*port*)))
(setf (hunchentoot:acceptor-document-root hunchentoot:*acceptor*) "static/")
