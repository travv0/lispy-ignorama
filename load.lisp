;; -*- coding:utf-8 -*-
(in-package :cl-user)

(SETF SB-IMPL::*DEFAULT-EXTERNAL-FORMAT* :UTF-8)

(load "~/quicklisp/setup.lisp")
(ql:quickload "clack")
(ql:quickload "lack-middleware-static")
(ql:quickload "clack-errors")
(ql:quickload "ningle")
(ql:quickload "cl-dbi")
(ql:quickload "spinneret")
(ql:quickload "local-time")
(ql:quickload "uuid")
(ql:quickload "parenscript")

(setf parenscript:*js-string-delimiter* #\')

(require "asdf")
(asdf:load-system :ignorama)
