;; -*- coding:utf-8 -*-
(in-package :cl-user)

(SETF SB-IMPL::*DEFAULT-EXTERNAL-FORMAT* :UTF-8)

(ql:quickload "cl-dbi")
(ql:quickload "dbd-postgres")
(ql:quickload "spinneret")
(ql:quickload "local-time")
(ql:quickload "uuid")
(ql:quickload "quri")
(ql:quickload "hunchentoot")
(ql:quickload "cl-ppcre")
(ql:quickload "parenscript")
(ql:quickload "web-util")

(setf parenscript:*js-string-delimiter* #\')
(setf *print-pretty* nil)

(require "asdf")
(asdf:load-system :ignorama)
