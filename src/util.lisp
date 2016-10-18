;; -*- coding:utf-8 -*-
(in-package :net.ignorama.web)

;;; utility functions and macros
(defun random-elt (choices)
  "Choose an element from a list at random."
  (elt choices (random (length choices))))

(defun universal-to-unix (time)
  (if time
      (timestamp-to-unix (universal-to-timestamp time))))

(defmacro echo (html)
  `(format (request-reply-stream request) "~a" ,html))
