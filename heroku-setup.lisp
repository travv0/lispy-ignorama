(in-package :cl-user)

(print ">>> Building system....")

(load (merge-pathnames "ignorama.asd" *build-dir*))

(asdf:load-system :ignorama)

;;; Redefine / extend heroku-toplevel here if necessary.

(print ">>> Done building system")
