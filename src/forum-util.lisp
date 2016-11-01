(in-package :net.ignorama.web)

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
                         LEFT JOIN users ON posts.UserID = users.UserID
                         WHERE threads.ThreadID = ?
                         ORDER BY PostID
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
      "SELECT UserStatusRank FROM UserStatuses WHERE lower(UserStatusDesc) = lower(?)"
      (required-rank)
    (let ((status (user-status-id)))
      (if status
          (<= status
              (getf rank :|userstatusrank|))))))

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

(defun user-status-id ()
  (let ((user-status (get-session-var 'userstatus)))
    (if (not user-status)
        (execute-query-one status "SELECT MAX(UserStatusID) AS UserStatusID FROM UserStatuses" ()
          (getf status :|userstatusid|))
        user-status)))

(defparameter *replacements*
  '(;; newlines
    ("\\n" "<br />")
    ;; images
    ("\\[img\\](.*?)\\[\\/img\\]"
     "<a target='_blank' href='\\1'><img href='\\1' src='\\1' class='img img-responsive' style='max-height: 480px;'></img></a>")
    ;; webm
    ("\\[webm\\](.*?)\\[\\/webm\\"
     "<video preload='none' controls='controls' class='img img-responsive'><source type='video/webm' src='\\1'></video>")
    ;; bold and italics
    ("\\[i\\](.*?)\\[\\/i\\]" "<i>\\1</i>")
    ("\\[b\\](.*?)\\[\\/b\\]" "<b>\\1</b>")
    ("\\[u\\](.*?)\\[\\/u\\]" "<u>\\1</u>")
    ;; spoilers
    ("\\[spoiler\\](.*?)\\[\\/spoiler\\]"
     "<span class='spoiler' style='background-color:#333;'>\\1</span>")
    ;; replies
    ("\\[reply[=| ]([0-9]+)\\]\\R*\\[\\/reply\\]"
     "<a href='javascript:viewPost(\\1);'><b>\\1</b></a>")
    ("\\[reply post=([0-9]+) user=(.*?)\\]\\R*\\[\\/reply\\]"
     "<a href='javascript:viewPost(\\1);'><b>\\2</b></a>")
    ("\\[reply user=(.*?) post=([0-9]+)\\]\\R*\\[\\/reply\\]"
     "<a href='javascript:viewPost(\\2);'><b>\\1</b></a>")
    ("&gt;&gt;([0-9]+)"
     "<a href='javascript:viewPost(\\1);'><b>\\1</b></a>")
    ("\\[reply post=([0-9]+) user=(.*?)\\]\\R*(.*?)\\R*\\[\\/reply\\]\\R?"
     "<div style='padding: 5px;border: 1px solid #DDD;background-color:#F5F5F5'><b><a href='javascript:viewPost(\\1);'>\\2</a> said:</b><br/>\\3</div>")
    ("\\[reply user=(.*?) post=([0-9]+)\\]\\R*(.*?)\\R*\\[\\/reply\\]\\R?"
     "<div style='padding: 5px;border: 1px solid #DDD;background-color:#F5F5F5'><b><a href='javascript:viewPost(\\2);'>\\1</a> said:</b><br/>\\3</div>")
    ("\\[reply[=| ]([0-9]+)\\]\\R*(.*?)\\R*\\[\\/reply\\]\\R?"
     "<div style='padding: 5px;border: 1px solid #DDD;background-color:#F5F5F5'><b><a href='javascript:viewPost(\\1);'>\\1</a> said:</b><br/>\\2</div>")
    ;; quotes
    ("\\[quote\\]\\R?(.*?)\\R?\\[\\/quote\\]\\R?"
     "<div style='padding: 5px;border: 1px solid #DDD;background-color:#F5F5F5'><b>Quote:</b><br/>\\1</div>")
    ;; code
    ("\\[code\\]\\R*(.*?)\\R*\\[\\/code\\]" "<pre><code>\\1</code></pre>")
    ;; colored text
    ("\\[color=(.*?)\\](.*?)\\[\\/color\\]" "<span style='color:\\1'>\\2</span>")
    ;; url
    ("\\[url=(http(s?):\\/\\/)?(.*?)\\](.*?)\\[\\/url\\]"
     "<a target='_blank' href='http\\2://\\3'>\\4</a>")
    ;; youtube embed
    ("[a-zA-Z\\/\\/:\\.]*(youtube.com\\/watch\\?v=|youtu.be\\/)([a-zA-Z0-9\\-_]+)([a-zA-Z0-9\\/\\*\\-\\_\\?\\&\\;\\%\\=\\.]*)"
     "<div class='flex-video widescreen'><iframe width='560' height='315' src='//www.youtube.com/embed/\\2' frameborder='0' allowfullscreen></iframe></div>")))

(defun format-post (post)
  (with-html
    (:raw
     (let ((post (html-escape post)))
       (dolist (replacement-pair *replacements*)
         (destructuring-bind (regex replacement) replacement-pair
           (setf post (regex-replace-all
                       (create-scanner regex
                                       :case-insensitive-mode t
                                       :multi-line-mode t)
                       post replacement))))
       post))))
