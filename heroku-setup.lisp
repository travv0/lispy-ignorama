(in-package :cl-user)

(print ">>> Building system....")

(require 'asdf)
(load (merge-pathnames "ignorama.asd" *build-dir*))
(load (merge-pathnames "load.lisp"*build-dir*))

(defun heroku-toplevel ()
  (let ((port (parse-integer (heroku-getenv "PORT"))))
    (funcall (symbol-function (find-symbol "CLACKUP" (find-package "CLACK"))) '(:port port))))
;; (clack:clackup (lack:builder
;; (:static
;;  :path "/static/"
;;  :root #p"static/")
;; net.ignorama.web:*app*)) :port port)

;;; Redefine / extend heroku-toplevel here if necessary.

(print ">>> Done building system")
