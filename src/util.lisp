;; -*- coding:utf-8 -*-
(in-package :net.ignorama.web)

(defun print-debug-to-log (text)
  (format
   t
   "~%~%~%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~%~%~%~a~%~%~%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~%~%~%"
   text))

(defmacro execute-query-loop (row query params &body body)
  `(let* ((q (prepare *conn* ,query))
          ,(if params
               `(result (execute q ,@params))
               `(result (execute q))))
     (loop for ,row = (fetch result)
        while ,row do
          ,@body)))

(defmacro execute-query-one (row query params &body body)
  `(let* ((q (prepare *conn* ,query))
          ,(if params
               `(result (execute q ,@params))
               `(result (execute q)))
          (,row (fetch result)))
     ,@body))

(defmacro execute-query-modify (query params)
  `(let* ((q (prepare *conn* ,query))
          ,(if params
               `(result (execute q ,@params))
               `(result (execute q))))))

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
       FROM users U
       JOIN UserStatuses US ON U.UserStatusID = US.UserStatusID
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
  (execute-query-one op "SELECT PostIP, UserName
                               FROM threads
                               JOIN posts ON threads.ThreadID = posts.ThreadID
                               JOIN users ON posts.UserID = users.UserID
                               WHERE threads.ThreadID = ?
                               LIMIT 1" (thread-id)
    (let ((username (getf op :|username|))
          (ip (getf op :|postip|)))
      (or (and username
               (equalp username
                       (get-session-var 'username)))
          (equal ip (real-remote-addr))))))

(defun logged-in-p ()
  (get-session-var 'username))

(defun user-authority-check-p (required-rank)
  (execute-query-one rank
      "SELECT UserStatusRank FROM UserStatuses WHERE lower(UserStatusDesc) = lower(?)" (required-rank)
    (let ((status (user-status-id)))
      (if status
          (<= status
              (getf rank :|userstatusrank|))))))

(defun get-session-var (session-var)
  (let ((session (gethash (cookie-in *session-id-cookie-name*) *sessions*)))
    (if session
        (gethash session-var session))))

(defun index-params-by-type (type)
  (cond ((equal type "search")
         (let ((search (get-parameter "search")))
           (values (format nil "Search for \"~a\""
                           (empty-string-if-nil search))
                   (format nil "(SELECT 1
                                 FROM posts
                                 WHERE posts.ThreadID = IndexThreads.ThreadID
                                   AND PostContent LIKE '%~a%'
                                 LIMIT 1) = 1
                                 OR ThreadSubject LIKE '%~a%'"
                           (empty-string-if-nil search)
                           (empty-string-if-nil search)))))
        (t "")))

(defun empty-string-if-nil (value)
  (if (not value)
      ""
      value))

(defun zero-if-nil (value)
  (if (not value)
      0
      value))

(defun is-null (x)
  (equal x :null))

(defun user-status-id ()
  (let ((user-status (get-session-var 'userstatus)))
    (if (not user-status)
        (execute-query-one status "SELECT MAX(UserStatusID) AS UserStatusID FROM UserStatuses" ()
          (getf status :|userstatusid|))
        user-status)))
