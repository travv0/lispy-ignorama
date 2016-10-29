;; -*- coding:utf-8 -*-
(in-package :cl-user)

(SETF SB-IMPL::*DEFAULT-EXTERNAL-FORMAT* :UTF-8)

(ql:quickload "cl-dbi")
(ql:quickload "dbd-postgres")
(ql:quickload "spinneret")
(ql:quickload "local-time")
(ql:quickload "uuid")
(ql:quickload "parenscript")
(ql:quickload "quri")
(ql:quickload "hunchentoot")

(setf *print-pretty* nil)
(setf parenscript:*js-string-delimiter* #\')

(require "asdf")
(asdf:load-system :ignorama)
