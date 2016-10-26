(in-package :cl-user)

(print ">>> Building system....")

(require "asdf")
(asdf:load-system :ignorama)

;;; Redefine / extend heroku-toplevel here if necessary.

(print ">>> Done building system")
