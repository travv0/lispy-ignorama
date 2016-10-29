;; -*- coding:utf-8 -*-
(in-package :net.ignorama.web)

(defmacro execute-query-loop (row query (&optional params) &body body)
  `(let* ((q (prepare *conn* ,query))
          ,(if params
               `(result (execute q ,params))
               `(result (execute q))))
     (loop for ,row = (fetch result)
        while ,row do
          ,@body)))

(defmacro execute-query-one (row query (&optional params) &body body)
  `(let* ((q (prepare *conn* ,query))
          ,(if params
               `(result (execute q ,params))
               `(result (execute q)))
          (,row (fetch result)))
     ,@body))

(defun random-elt (choices)
  "Choose an element from a list at random."
  (elt choices (random (length choices))))

(defmethod print-object ((object hash-table) stream)
  (format stream "#HASH{~{~{(~a : ~a)~}~^ ~}}"
          (loop for key being the hash-keys of object
             using (hash-value value)
             collect (list key value))))

(defun get-user-status (user)
  (execute-query-one user-status
      "SELECT UserStatusRank
       FROM admin A
       JOIN UserStatuses US ON A.UserStatusID = US.UserStatusID
       WHERE lower(Username) = lower(?)" (user)
    (getf user-status :|userstatusrank|)))

(defun get-thread-title (thread-id)
  (execute-query-one thread-subject
      "SELECT ThreadSubject FROM threads WHERE ThreadID = ?" (thread-id)
    (getf thread-subject :|threadsubject|)))

(defun thread-locked-p (thread-id)
  (execute-query-one locked
      "SELECT Locked FROM threads WHERE ThreadID = ?" (thread-id)
    (getf locked :|locked|)))

(defun is-op-p (thread-id)
  (execute-query-one op "SELECT ThreadIP, ModName
                               FROM threads
                               JOIN posts ON threads.ThreadID = posts.ThreadID
                               WHERE threads.ThreadID = ?
                               LIMIT 1" (thread-id)
    (let ((username (getf op :|modname|))
          (ip (getf op :|threadip|)))
      (or (and username
               (equalp username
                      (get-session-var 'username)))
          (equal ip (real-remote-addr))))))

(defun logged-in-p ()
  (get-session-var 'username))

(defun user-authority-check-p (required-rank)
  (execute-query-one rank
      "SELECT UserStatusRank FROM UserStatuses WHERE UserStatusDesc = ?" (required-rank)
    (let ((status (get-session-var 'userstatus)))
      (if status
          (<= status
              (getf rank :|userstatusrank|))))))

(defun get-session-var (session-var)
  (let ((session (gethash (cookie-in *session-id-cookie-name*) *sessions*)))
    (if session
        (gethash session-var session))))
