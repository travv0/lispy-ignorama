(in-package :cl-user)

(print ">>> Building system....")

(require 'asdf)
(load (merge-pathnames "ignorama.asd" *build-dir*))

;;; Redefine / extend heroku-toplevel here if necessary.

(print ">>> Done building system")
