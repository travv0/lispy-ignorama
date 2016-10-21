;; -*- coding:utf-8 -*-
(in-package :cl-user)

(SETF SB-IMPL::*DEFAULT-EXTERNAL-FORMAT* :UTF-8)

(load "~/quicklisp/setup.lisp")
(ql:quickload "clack")
(ql:quickload "lack-middleware-static")
(ql:quickload "clack-errors")
(ql:quickload "ningle")
(ql:quickload "cl-dbi")
(ql:quickload "cl-who")
(ql:quickload "local-time")
(ql:quickload "uuid")

(require "asdf")
(asdf:load-system :ignorama)
