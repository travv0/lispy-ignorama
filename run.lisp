;; -*- coding:utf-8 -*-
(in-package :cl-user)
(import '(lack.builder:builder))

(load "load.lisp")

(in-package :net.ignorama.web)
(clack:clackup (lack:builder
		(:static
		 :path "/static/"
		 :root #p"static/")
		*app*))
