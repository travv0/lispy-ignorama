;; -*- coding:utf-8 -*-
(in-package :cl-user)

(load "load.lisp")
(import '(lack.builder:builder))

(in-package :net.ignorama.web)
(clack:clackup (lack:builder
    (:static
     :path "/static/"
     :root #p"static/")
    *app*) :port *port*)
