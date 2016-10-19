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

(defmethod print-object ((object hash-table) stream)
  (format stream "#HASH{~{~{(~a : ~a)~}~^ ~}}"
          (loop for key being the hash-keys of object
	     using (hash-value value)
	     collect (list key value))))

(defmacro query-param (param)
  `(cdr (assoc ,param (request-query request) :test #'equal)))

(defun get-user-status (user)
  nil)
