(in-package :cl-user)

(load "~/quicklisp/setup.lisp")

(ql:quickload "aserve")
(ql:quickload "monkeylib-html")
(ql:quickload "cl-dbi")

(defpackage :com.ignorama.web
  (:use :cl :net.aserve :cl-dbi :monkeylib-html))
