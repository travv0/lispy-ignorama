;; -*- coding:utf-8 -*-
(in-package :cl-user)

(SETF SB-IMPL::*DEFAULT-EXTERNAL-FORMAT* :UTF-8)

(load "~/quicklisp/setup.lisp")

(ql:quickload "aserve")
(ql:quickload "monkeylib-html")
(ql:quickload "cl-dbi")

(defpackage :net.ignorama.web
  (:use :cl :net.aserve :cl-dbi :monkeylib-html))

(load "forum.lisp")
