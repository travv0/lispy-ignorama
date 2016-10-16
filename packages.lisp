(in-package :cl-user)

(load "~/quicklisp/setup.lisp")

(ql:quickload "aserve")
(ql:quickload "monkeylib-html")

(defpackage :com.ignorama.web
  (:use :cl :net.aserve :monkeylib-html))
