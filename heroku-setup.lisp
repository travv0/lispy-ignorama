(in-package :cl-user)

(print ">>> Building system....")

(require 'asdf)
(load (merge-pathnames "ignorama.asd" *build-dir*))
(load (merge-pathnames "load.lisp"*build-dir*))

(in-package :net.ignorama.web)
(defun heroku-toplevel ()
  (clack:clackup (lack:builder
                  (:static
                   :path "/static/"
                   :root #p"static/")
                  *app*) :port *port*))

;;; Redefine / extend heroku-toplevel here if necessary.

(print ">>> Done building system")
