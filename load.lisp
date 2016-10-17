;; -*- coding:utf-8 -*-
(in-package :cl-user)

(SETF SB-IMPL::*DEFAULT-EXTERNAL-FORMAT* :UTF-8)

(load "asdf.lisp")
(load "~/quicklisp/setup.lisp")

(ql:quickload "aserve")
(ql:quickload "monkeylib-html")
(ql:quickload "cl-dbi")
(ql:quickload "local-time")

(defpackage :net.ignorama.web
  (:use :cl :net.aserve :cl-dbi :monkeylib-html :local-time))

(load "forum.lisp")

(net.aserve:start :port 2001)
