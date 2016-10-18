;; -*- coding:utf-8 -*-
(in-package :cl-user)

(SETF SB-IMPL::*DEFAULT-EXTERNAL-FORMAT* :UTF-8)

(load "~/quicklisp/setup.lisp")
(ql:quickload "aserve")
(ql:quickload "cl-dbi")
(ql:quickload "monkeylib-html")
(ql:quickload "local-time")

(require "asdf")
(asdf:load-system :ignorama)
