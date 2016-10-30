(in-package :net.ignorama.web)

(publish-page b/login
  (let ((user-status nil))
    ;; if no status, user doesn't exist
    (if (setf user-status (get-user-status (post-parameter "username")))
        (let ((session-id nil))
          ;; find an id not in use and set it to session-id
          (loop while (gethash
                       (setf session-id (write-to-string (make-v4-uuid)))
                       *sessions*))

          (setf (gethash session-id *sessions*) (make-hash-table :test 'equal))

          ;; make life easier by making sure username in session is capitalized like in the DB
          (execute-query-one user
              "SELECT UserID, UserName FROM users WHERE lower(UserName) = lower(?)"
              ((post-parameter "username"))
            (setf (gethash 'username (gethash session-id *sessions*)) (getf user :|username|))
            (setf (gethash 'userid (gethash session-id *sessions*)) (getf user :|userid|)))

          (setf (gethash 'userstatus (gethash session-id *sessions*)) user-status)
          (setf (gethash 'userlastactive (gethash session-id *sessions*)) (get-universal-time))

          (set-cookie *session-id-cookie-name*
                      :value session-id
                      :path "/"
                      :expires (+ (get-universal-time) (* 10 365 24 60 60)))
          (redirect "/"))
        (redirect "/login-failed"))))

(publish-page b/submit-post
  (progn (if (thread-locked-p (get-parameter "thread"))
             (redirect "/locked"))
         (execute-query-one post
             "INSERT INTO posts (
                ThreadID,
                UserID,
                Anonymous,
                PostContent,
                PostTime,
                PostIP,
                PostRevealedOP,
                Bump
              )
              VALUES (
                ?,                  --ThreadID
                ?,                  --UserID
                ?,                  --Anonymous
                ?,                  --PostContent
                current_timestamp,  --PostTime
                ?,                  --PostIP
                ?,                  --PostRevealedOP
                ?                   --Bump
              )
              RETURNING PostID"
             ((get-parameter "thread")
              (if (get-session-var 'userid)
                  (get-session-var 'userid)
                  :null)
              (post-parameter "anonymous")
              (post-parameter "postcontent")
              (real-remote-addr)
              (post-parameter "reveal-op")
              (post-parameter "bump"))

           (redirect (format nil "/view-thread?post=~d"
                             (getf post :|postid|))))
         (redirect "/error")))

(publish-page b/submit-thread
  (execute-query-one thread
      "INSERT INTO threads (
         ThreadSubject,
         TagID,
         Stickied,
         Locked,
         Deleted,
         Banned,
         LastEditBy
       )
       VALUES (
         ?,     --ThreadSubject,
         ?,     --TagID,
         false, --Stickied,
         false, --Locked,
         false, --Deleted,
         false, --Banned,
         ''     --LastEditBy
       )
       RETURNING ThreadID"
      ((post-parameter "subject")
       (post-parameter "tag"))

    (execute-query-one post
        "INSERT INTO posts (
           ThreadID,
           UserID,
           Anonymous,
           PostContent,
           PostTime,
           PostIP,
           PostRevealedOP,
           Bump
         )
         VALUES (
           ?,                  --ThreadID
           ?,                  --UserID
           ?,                  --Anonymous
           ?,                  --PostContent
           current_timestamp,  --PostTime
           ?,                  --PostIP
           true,               --PostRevealedOP
           true                --Bump
         )"
        ((getf thread :|threadid|)
         (if (get-session-var 'userid)
             (get-session-var 'userid)
             :null)
         (post-parameter "anonymous")
         (post-parameter "body")
         (real-remote-addr))
      (redirect (format nil
                        "/view-thread?thread=~d"
                        (getf thread :|threadid|))))))

(publish-page b/logout
  (remhash (cookie-in *session-id-cookie-name*) *sessions*)
  (redirect "/"))

;; (publish-page b/submit-tags
;;   (if (logged-in-p)
;;       (progn ())))
