;; -*- coding:utf-8 -*-
(in-package :net.ignorama.web)

;;; utility functions and macros
(defun random-elt (choices)
  "Choose an element from a list at random."
  (elt choices (random (length choices))))

(defmethod print-object ((object hash-table) stream)
  (format stream "#HASH{~{~{(~a : ~a)~}~^ ~}}"
          (loop for key being the hash-keys of object
                using (hash-value value)
                collect (list key value))))

(defun get-user-status (user)
  (with-db conn
           (let* ((q (prepare conn
                              "SELECT UserStatusRank
                               FROM admin A
                               JOIN UserStatuses US ON A.UserStatusID = US.UserStatusID
                               WHERE lower(Username) = lower(?)"))
                  (result (execute q user))
                  (user-status (fetch result)))
             (getf user-status :|userstatusrank|))))

(defun get-thread-title (thread-id)
  (with-db conn
           (let* ((q (prepare conn
                              "SELECT ThreadSubject FROM threads WHERE ThreadID = ?"))
                  (result (execute q thread-id))
                  (thread-subject (fetch result)))
             (getf thread-subject :|threadsubject|))))

(defun thread-locked-p (thread-id)
  (with-db conn
           (let* ((q (prepare conn
                              "SELECT Locked FROM threads WHERE ThreadID = ?"))
                  (result (execute q thread-id))
                  (locked (fetch result)))
             (getf locked :|locked|))))

;; TODO: this function
(defun is-op-p (thread-id)
  t)

;; TODO: this function
(defun logged-in-p ()
  t)

(defun user-authority-check-p (required-rank)
  (with-db conn
    (let ((session (gethash (cookie-in "sessionid") *sessions*)))
         (if session
             (let* ((q (prepare conn
                                "SELECT UserStatusRank FROM UserStatuses WHERE UserStatusDesc = ?"))
                    (result (execute q required-rank))
                    (rank (fetch result)))
               (<= (gethash 'userstatus
                            (gethash (cookie-in "sessionid")
                                     *sessions*))
                   (getf rank :|userstatusrank|)))))))
