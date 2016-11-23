;; -*- coding:utf-8 -*-
(in-package :cl-user)

(SETF SB-IMPL::*DEFAULT-EXTERNAL-FORMAT* :UTF-8)

(ql:quickload "ignorama")

(setf parenscript:*js-string-delimiter* #\')
(setf *print-pretty* nil)

(require "asdf")
(asdf:load-system :ignorama)
