;; -*- coding:utf-8 -*-
(in-package :net.ignorama.web)

;;; utility functions and macros
(defun random-elt (choices)
  "Choose an element from a list at random."
  (elt choices (random (length choices))))

(defun universal-to-unix (time)
  (if time
      (timestamp-to-unix (universal-to-timestamp time))))

(defmethod print-object ((object hash-table) stream)
  (format stream "#HASH{~{~{(~a : ~a)~}~^ ~}}"
          (loop for key being the hash-keys of object
             using (hash-value value)
             collect (list key value))))

(defun get-user-status (user)
  (let* ((q (prepare *db*
                     "SELECT UserStatusDesc
                      FROM `admin` A
                      JOIN UserStatuses US ON A.UserStatusID = US.UserStatusID
                      WHERE Username = ?"))
         (result (execute q user))
         (user-status (fetch result)))
    (getf user-status :|UserStatusDesc|)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmacro get-thread-title (thread-id)
    `(let* ((q (prepare *db*
                        "SELECT ThreadSubject
                         FROM threads
                         WHERE ThreadID = ?"))
            (result (execute q ,thread-id))
            (thread-subject (fetch result)))
       (getf thread-subject :|ThreadSubject|))))

(defmacro set-cookie (name value)
  `(format nil "~a=~a; path=/" ,name ,value))

(defun thread-locked-p (thread-id)
  (let* ((q (prepare *db*
                     "SELECT Locked
                      FROM `threads`
                      WHERE ThreadID = ?"))
         (result (execute q thread-id))
         (locked (fetch result)))
    (if (equal (getf locked :|Locked|) 1) t)))

;; TODO: this function
(defun is-op-p (thread-id)
  t)

;; TODO: this function
(defun logged-in-p ()
  t)
