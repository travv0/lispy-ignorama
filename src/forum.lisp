(in-package :net.ignorama.web)

;;; site setup
(defun threads-query (condition &optional order-by)
  (let ((user-column (if (logged-in-p) "userid" "userip"))
        (user-value (if (logged-in-p) (get-session-var 'userid) (real-remote-addr))))
    (format nil "SELECT *
               FROM IndexThreads
               WHERE (~a)
                 AND (IsGlobal = true
                  OR (UserStatusID >= ~d
                      AND tagid IN (SELECT tagid
                                    FROM selectedtags
                                    WHERE ~a = '~a')))
                 AND threadid NOT IN (SELECT threadid
                                      FROM hidden
                                      WHERE ~a = '~a')
               ~a
               LIMIT ~d"
            (if condition
                condition
                :true)
            (user-status-id)
            user-column user-value
            user-column user-value
            (if order-by
                order-by
                "")
            *index-row-limit*)))

(defun hidden-query ()
  (let ((user-column (if (logged-in-p) "userid" "userip"))
        (user-value (if (logged-in-p) (get-session-var 'userid) (real-remote-addr))))
    (format nil "SELECT *
               FROM IndexThreads
               WHERE (IsGlobal = true
                      OR (UserStatusID >= ~d
                          AND tagid IN (SELECT tagid
                                        FROM selectedtags
                                        WHERE ~a = '~a')))
                 AND threadid IN (SELECT threadid
                                  FROM hidden
                                  WHERE ~a = '~a')
               LIMIT ~d"
            (user-status-id)
            user-column user-value
            user-column user-value
            *index-row-limit*)))

(defun tags-query ()
  (let ((user-status-id (user-status-id)))
    (format nil "SELECT TagID,
                        TagName,
                        (SELECT true
                         FROM selectedtags
                         WHERE (~a = '~a'
                                OR IsGlobal = true)
                           AND selectedtags.tagid = tags.tagid) AS selected,
                        IsGlobal
          FROM tags
          WHERE (
           IsActive = true AND
           UserStatusID >= ~d AND
           IsGlobal = false
          )
             OR (
           IsGlobal = true AND
           ~d <= (SELECT UserStatusID
            FROM UserStatuses
            WHERE UserStatusDesc = 'Admin')
          )
          ORDER BY UserStatusID ASC,
             TagName"
            (if (logged-in-p) "userid" "userip")
            (if (logged-in-p) (get-session-var 'userid) (real-remote-addr))
            user-status-id
            user-status-id)))

(defparameter *rightlinks* '("Following" "Hidden" "Rules" "Bans" "Settings"))

;;; stuff to go in the <head> tags (minus <title>)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *head*
    `((:meta :charset "UTF-8")
      (:meta :name "viewport"
             :content "width=device-width, initial-scale=1, maximum-scale=1")

      (:link :rel "shortcut icon"
             :type "image/png"
             :href *favicon-path*)

      (:link :rel "stylesheet"
             :href "//maxcdn.bootstrapcdn.com/bootstrap/3.2.0/css/bootstrap.min.css")
      (:link :rel "stylesheet"
             :href "//maxcdn.bootstrapcdn.com/bootstrap/3.2.0/css/bootstrap-theme.min.css")
      (:link :rel "stylesheet"
             :href "//maxcdn.bootstrapcdn.com/font-awesome/4.2.0/css/font-awesome.min.css")

      (:link :rel "stylesheet"
             :href "/style.css")

      (:script :src "//code.jquery.com/jquery-1.11.0.min.js")
      (:script :src "//maxcdn.bootstrapcdn.com/bootstrap/3.2.0/js/bootstrap.min.js")
      (:script :src "/script.js")
      (:script :src "/js/script.js"))))

(defun generate-sociallinks (sites)
  (let ((result '(progn)))
    (dolist (site sites)
      (setf result (append result (list (sociallink (first site)
                                                    (second site)
                                                    (third site))))))
    result))

(defun generate-rightlinks (links)
  (let ((result '(progn)))
    (dolist (link links)
      (setf result (append result (list (rightlink link)))))
    result))

(defun generate-dropdown-links (links)
  (let ((result '(progn)))
    (dolist (link links)
      (setf result (append result
                           (list (with-html
                                   (:div
                                    (:a :class "dropdown-item"
                                        :href (concatenate 'string
                                                           "/"
                                                           (string-downcase link))
                                        link)))))))
    result))

(defun generate-dropdown-links-social (sites)
  (let ((result '(progn)))
    (dolist (site sites)
      (destructuring-bind (type url &optional name) site
        (setf result (append result
                             (list (with-html
                                     (:div (:a :class "dropdown-item"
                                               :target "_blank"
                                               :href url
                                               (site-symbol-to-name type
                                                                    name)))))))))
    result))

(defmacro defhtml (name params &body body)
  `(defun ,name ,params
     (with-html
       ,@body)))

(defhtml rightlinks (rightlinks sociallinks)
  (row (col 12
         (desktop-only (rightlinks-desktop rightlinks sociallinks))
         (mobile-only (rightlinks-mobile rightlinks sociallinks))))
  (row (col 12
         (login-links))))

(defhtml rightlinks-desktop (rightlinks sociallinks)
  (desktop-only
    (:div :style "float: right;"
          (generate-sociallinks sociallinks)
          (generate-rightlinks rightlinks))))

(defhtml rightlinks-mobile (rightlinks sociallinks)
  (row (col 12
         (:div :style "float: right;" :class "btn-group"
               (:a :style "margin: 5px;" :class "btn btn-default btn-sm dropdown-toggle"
                   :data-toggle "dropdown"
                   "Menu " (:span :class "caret"))
               (:ul :class "dropdown-menu pull-right"
                    (generate-dropdown-links rightlinks)
                    (generate-dropdown-links-social sociallinks))))))

(defhtml login-links ()
  (:div :style "float: right;"
        (if (logged-in-p)
            (:div :style "color: white;"
                  (format nil "Logged in as ~a " (get-session-var 'username))
                  (:a :class "header-link" :href "/b/logout"
                      "(logout)"))
            (:div (:a :class "header-link"
                      :href "/create-account" "Sign up")
                  (:span :class "header-link" :style "color: white;" "/")
                  (:a :class "header-link"
                      :href "/login" "Log in")))))

;;; page skeleton
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *header*
    '((row :class "header"
        (col 12

          ;; logo and slogans
          (:div :class "container"
                (row
                  (desktop-only
                    (col 6 :style "padding-top: 4px;"
                      (:a :href "/"
                          (:img :src *logo-path*))
                      (if *slogans*
                          (:b :class "slogan"
                              :style "position: absolute; top: 15px;"
                              (:raw (random-elt *slogans*))))))
                  (mobile-only
                    (col 3 :style "padding-top: 15px;"
                      (:a :href "/"
                          (:img :src *small-logo-path*))))

                  ;; right links
                  (:span :class "col-xs-9 col-sm-6"
                         (rightlinks *rightlinks* *sociallinks*)))))))))

;;; The basic format that every viewable page will follow.
(defmacro standard-page ((&key title) &body body)
  `(with-html-string
     (:doctype)
     (:html
      (:head (:title (concatenate 'string
                                  ,title
                                  (if (equal ,title "")
                                      ""
                                      " - ")
                                  *site-name*))
             ,@*head*)
      (:body
       (:div :class "main"
             ,@*header*
             (:div :class "container"
                   (unless (equal ,title "")
                     (:h2 :class "page-title"
                          ,title))
                   ,@body))))))

;;; this macro creates and publishes page <name> at https://your-site.com/<name>
(defmacro publish-page (name &body body)
  `(hunchentoot:define-easy-handler (,name
                                     :uri ,(string-downcase
                                            (if (equal 'index name)
                                                "/"
                                                (concatenate 'string "/" (symbol-name name)))))
       ()
     (setf (hunchentoot:content-type*) "text/html")
     (let ((*conn* *conn*))
       (with-db *conn*
         ,@body))))

(publish-page index
  (multiple-value-bind (title condition order-by)
      (index-params-by-type (get-parameter "f"))
    (standard-page
        (:title title)
      (:body (index-buttons)
             (log-message* "NOTE" "Stats for generating threads for main page:")
             (log-message* "NOTE"
                           (with-output-to-string (*trace-output*)
                             (time (threads-table (if (equal (get-parameter "f") "hidden")
                                                      (hidden-query)
                                                      (threads-query condition order-by))))))
             (:div :style "padding-top: 15px;"
                   (:raw *fake-copyright*))))))

(defhtml index-buttons ()
  ;; dropdown only displays correctly when I wrap all the buttons in this div
  (row :class "dropdown"
    (col 12 :style "margin-top: 5px; margin-bottom: 5px;"
      (if (or *allow-anonymity*
              (logged-in-p))
          (:button :class "btn btn-default btn-sm threads"
                   :style "margin: 3px;"
                   :onclick "window.location='new-thread'"
                   "New Thread"))

      (:form :style "float: right;"
             :action "b/apply-tags"
             :method "post"

             (:input :type "submit"
                     :style "margin: 3px;"
                     :class "btn btn-default btn-sm threads"
                     :value "Apply Boards")

             (tags-filter-dropdown))

      (:span  :style "float: right; margin: 3px;"
              (desktop-only (search-box))))))

(defhtml search-box ()
  (:form :action "/"
         :method "get"
         :class "searchform"
         (:input :class "searchbox"
                 :name "search"
                 :type "textbox")
         (:input :type "hidden"
                 :name "f"
                 :value "search")
         (:button :class "btn btn-default btn-sm searchbutton"
                  :type "submit"
                  (:span :class "glyphicon glyphicon-search"))))

(defhtml threads-table (query)
  (execute-query-loop thread query ()
    (:div :style ""
          (thread-row (getf thread :|threadid|)
                      (getf thread :|postid|)
                      (getf thread :|threadsubject|)
                      (getf thread :|tag|)
                      (getf thread :|postcount|)
                      (getf thread :|latestposttime|)
                      (getf thread :|locked|)
                      (getf thread :|stickied|)))))

(defhtml thread-row (id op-id subject tag post-count latest-post-time locked stickied)
  (row
    (col 12 :class (format nil "thread~a"
                           (if (and (following-thread-p id
                                                        (get-session-var 'userid)
                                                        (real-remote-addr))
                                    (> (get-max-post-in-thread id)
                                       (get-last-seen-post id
                                                           (get-session-var 'userid)
                                                           (real-remote-addr))))
                               " newposts"
                               ""))
      (thread-row-dropdown id op-id :locked locked :stickied stickied)
      (print-link-to-thread id subject :locked locked :stickied stickied)
      (if (following-thread-p id (get-session-var 'userid) (real-remote-addr))
          (:a :href (format nil "b/unfollow?thread=~d&f=~a"
                            id (get-parameter "f"))
              :title "Unfollow thread"
              :style "padding-right: 5px;"
              (:b :class "unfollow-link glyphicon glyphicon-eye-close")))
      (:br)
      (:span :style "font-size: 12px; color: gray;"
             (print-user-name-and-ip op-id)
             (:br)
             (:raw
              (join-string-list
               (list
                (format nil "Board: ~a" tag)
                (format nil "Replies: ~a" post-count)
                (format nil "Last reply: ~a" (with-html-string
                                               (:span :class "time"
                                                      (format-date-for-display latest-post-time)))))
               " | "))))))

(defun format-date-for-display (date)
  (local-time:to-rfc3339-timestring
   (local-time:parse-timestring
    (substitute #\T #\  date)
    :fail-on-error nil)))

(defhtml thread-row-dropdown (thread-id op-id &key (locked nil) (stickied nil))
  (:div :style "float: right;" :class "btn-group"
        (:a :class "btn btn-default btn-xs dropdown-toggle"
            :data-toggle "dropdown"
            (:span :class "caret"))
        (:ul :class "dropdown-menu pull-right"
             (:li (:a :href (format nil "b/hide-thread?thread=~d" thread-id)
                      (if (equal (get-parameter "f") "hidden")
                          "Unhide thread"
                          "Hide thread")))
             (when (user-authority-check-p "Moderator")
               (:li (:a :href (format nil "edit-post?post=~d" op-id)
                        "Edit OP"))
               (:li (:a :href (format nil "ban-post?post=~d" op-id)
                        "Ban OP")))
             (when (user-authority-check-p "Admin")
               (:li (:a :href (format nil "a/thread-ban?thread=~d" thread-id)
                        "Thread ban"))
               (:li (:a :href (format nil "a/delete-thread?thread=~d" thread-id)
                        "Delete thread"))
               (:li (:a :href (format nil "a/purge-thread?thread=~d" thread-id)
                        "Purge thread"))
               (:li (:a :href (format nil "a/purge-duplicate?thread=~d" thread-id)
                        "Purge duplicate (no ban)")))
             (when (user-authority-check-p "Moderator")
               (:li (:a :href (format nil "a/sticky-thread?thread=~d" thread-id)
                        (if stickied
                            "Unsticky thread"
                            "Sticky thread")))
               (:li (:a :href (format nil "a/lock-thread?thread=~d" thread-id)
                        (if locked
                            "Unlock thread"
                            "Lock thread")))))))

(defhtml print-user-name-and-ip (post-id)
  (multiple-value-bind (user-name ip)
      (print-username post-id)
    (:raw (concatenate 'string user-name (when ip
                                           (format nil " (~a)" ip))))))

(defun thread-url (id)
  (format nil "view-thread?thread=~d" id))

(publish-page following
  (redirect "/?f=following"))

(publish-page hidden
  (redirect "/?f=hidden"))

(defmacro posts-table (query &rest params)
  `(with-html
     (execute-query-loop post ,query (,@params)
       (post-row (getf post :|postid|)
                 (getf post :|posttime|)
                 (getf post :|postcontent|)
                 (getf post :|posteditcontent|)
                 (new-post-p (getf post :|threadid|)
                             (getf post :|postid|))))))

(defhtml post-row (id time content edit-content new-post)
  (row
    (col 12 :class (format nil "thread~a"
                           (if (not new-post) " seen" ""))
      (:div :style "margin-bottom: -15px;"
            (:span :style "font-size: 12px; color: gray;"
                   (let ((options (print-post-options id)))
                     (if (not (equal options ""))
                         (:raw (join-string-list
                                (list (with-html-string
                                        (:b (print-user-name-and-ip id)))
                                      (print-post-options id))
                                " | "))
                         (:b (print-user-name-and-ip id))))
                   (:span :style "float: right;"
                          :class "time"
                          (format-date-for-display time))))
      (:br)
      (:div (when (nil-if-empty-string (empty-string-if-null edit-content))
              (format-post edit-content)
              (:hr)
              (:b "Original post:")
              (:br))
            (format-post content)))))

(defun new-post-p (thread-id post-id)
  (let ((last-seen (get-last-seen-post thread-id
                                       (get-session-var 'userid)
                                       (real-remote-addr))))
    (if last-seen
        (> post-id last-seen)
        t)))

(defhtml thread-buttons ()
  (let ((thread-id (get-parameter "thread")))
    (when thread-id
      (if (and (thread-locked-p thread-id)
               (not (user-authority-check-p "Moderator")))
          (:button :class "btn btn-default btn-sm"
                   :disabled t
                   "Locked")
          (if (or *allow-anonymity*
                  (logged-in-p))
              (:button :class "btn btn-default btn-sm"
                       :onclick (format nil "window.location='new-reply?thread=~d'"
                                        (get-parameter "thread"))
                       "Reply")))))
  (:button :class "btn btn-default btn-sm"
           :onclick "window.location='/'"
           "Main Page")

  (pagination))

(defhtml thread-dropdown ()
  (:span :style "float: right;" :class "btn-group"
         (:a :class "btn btn-default btn-sm dropdown-toggle"
             :data-toggle "dropdown"
             :href "#"
             (:span :class "caret"))
         (:ul :class "dropdown-menu pull-right"
              "TODO - add stuff here")))

(publish-page view-thread
  (let ((post-id (get-parameter "post"))
        (thread-id (get-parameter "thread"))
        (user-id (get-parameter "user"))
        (user-ip (get-parameter "ip"))
        (page (get-parameter "page")))
    ;; if passed "post" parameter, redirect to appropriate thread and highlight post
    (if post-id
        (execute-query-one thread
            "SELECT ThreadID FROM posts WHERE PostID = ?"
            (post-id)
          (redirect (format nil
                            "/view-thread?thread=~d&highlight=~d#post~d"
                            (getf thread :|threadid|)
                            post-id
                            post-id))))

    ;; don't let people view post history if they aren't a moderator
    (unless (or (and (or user-id user-ip)
                     (user-authority-check-p "Moderator"))
                (and thread-id
                     (not (or user-id user-ip))))
      (redirect "/"))

    (if (not (and (equal (empty-string-if-nil thread-id) "")
                  (equal (empty-string-if-nil user-id) "")
                  (equal (empty-string-if-nil user-ip) "")))
        (standard-page
            (:title (cond (thread-id (get-thread-title thread-id))
                          (user-id (execute-query-one
                                       user
                                       "SELECT UserName
                                        FROM users
                                        WHERE UserID = ?"
                                       (user-id)
                                     (getf user :|username|)))
                          (user-ip user-ip)))
          (:body (:div :style "margin-bottom: 5px;"
                       (thread-buttons)
                       (thread-dropdown))
                 (posts-table
                  (format nil "SELECT posts.*
                               FROM posts
                               WHERE True
                               ~a
                               ORDER BY PostTime ~a
                               LIMIT ?
                               OFFSET ?"
                          (concatenate 'string
                                       (unless (equal (empty-string-if-nil thread-id) "")
                                         (format nil " AND ThreadID = ~d"
                                                 (dbi.driver:escape-sql *conn* thread-id)))
                                       (unless (equal (empty-string-if-nil user-id) "")
                                         (format nil " AND UserID = ~d"
                                                 (dbi.driver:escape-sql *conn* user-id)))
                                       (unless (equal (empty-string-if-nil user-ip) "")
                                         (format nil " AND PostIP = '~d'"
                                                 (dbi.driver:escape-sql *conn* user-ip))))
                          (if (or user-id user-ip)
                              "DESC"
                              "ASC"))
                  *posts-per-page*
                  (if page
                      (* (- (parse-integer
                             page) 1)
                         *posts-per-page*)
                      0))
                 (:div :style "margin-top: 5px; margin-bottom: 5px;"
                       (thread-buttons))
                 (:div :class "fake-copyright"
                       (:raw *fake-copyright*))
                 (if thread-id
                     (:script (view-thread-js))))
          (when (nil-if-empty-string thread-id)
            (follow-thread thread-id (get-session-var 'userid) (real-remote-addr))
            (set-last-seen-post thread-id
                                (get-session-var 'userid)
                                (real-remote-addr)
                                (if page (parse-integer page) 1))))

        (redirect "/"))))

(publish-page login
  (standard-page
      (:title "Login")
    (:body
     (:form :method "POST" :action "/b/login"
            (:div (:input :name "username"
                          :type "text"
                          :required t))
            (:div (:input :name "password"
                          :type "password"
                          :required t))
            (:div (:input :name "Submit1" :type "submit" :value "Submit")
                  (:input :type "button"
                          :value "Main Page"
                          :onclick "window.location='../'"))))))

(publish-page create-account
  (standard-page
      (:title "Create Account")
    (:body
     (:form :id "loginForm" :method "POST" :action "/b/create-account"
            (:div (:input :id "username" :name "username" :type "text" :required t))
            (:div (:input :id "password" :name "password" :type "password" :required t))
            (:div (:input :type "submit"
                          :class "btn btn-sm btn-default"
                          :value "Submit")
                  (:input :type "button"
                          :value "Main Page"
                          :class "btn btn-sm btn-default"
                          :onclick "window.location='../'"))))))

(publish-page b/create-account
  (let ((user-name (post-parameter "username"))
        (password (post-parameter "password")))
    (if (not (get-user-status user-name))
        (create-user user-name password)
        (with-html-string (:span "user already exists")))))

(defun create-user (user-name password)
  (execute-query-modify
   "INSERT INTO users (
        username,
        passhash,
        lastlogin,
        userstatusid
    )
    VALUES (
        ?,
        ?,
        current_timestamp,
        (SELECT userstatusid FROM userstatuses WHERE userstatusdesc = 'User')
    )"
   (user-name (signature password)))
  (login-user user-name password))

(defhtml image-upload-form ()
  (:form :class "col-xs-12"
         :id "uploadForm"
         (:input :id "upload"
                 :onchange "updateFilename();"
                 :type "file"
                 :name "upload")
         (:input :id "uploadsubmit"
                 :type "submit"
                 :value "Upload"
                 :class "invisiblebutton")
         (:input :id "filename"
                 :type "hidden"
                 :name "filename"
                 :value "none"))
  (:script :src "/uploadfile.js"))

(defhtml reply-buttons ()
  (:span :class "reply button-row checkboxes"
         (if (is-op-p (get-parameter "thread"))
             (:input :name "reveal-op"
                     :type "checkbox"
                     "Reveal OP Status? ")
             (:input :name "bump"
                     :type "checkbox"
                     "Bump! "))
         (if (and *allow-anonymity*
                  (logged-in-p))
             (:input :name "anonymous"
                     :type "checkbox"
                     "Post Anonymously")))
  (:span :class "reply button-row buttons"
         (:input :id "submitbutton"
                 :class "btn btn-default btn-sm"
                 :name "Submit"
                 :type "submit"
                 :value "Submit")
         (:input :type "button"
                 :class "btn btn-default btn-sm"
                 :value "Back"
                 :onclick (format nil "window.location='./view-thread?thread=~d'"
                                  (get-parameter "thread")))))

(publish-page new-reply
  (when (banned-p)
    (redirect "/banned"))
  (if (and (thread-locked-p (get-parameter "thread"))
           (not (user-authority-check-p "Moderator")))
      (redirect "/locked")
      (standard-page
          (:title "New Reply")
        (:body
         (:form :action (format nil "b/submit-post?thread=~d"
                                (get-parameter "thread"))
                :method "post"
                (:textarea :id "postfield"
                           :name "postcontent"
                           :rows "7"
                           :class "col-xs-12"
                           :required t)
                (reply-buttons))
         (image-upload-form)))))

(publish-page locked
  (standard-page
      (:title "Thread Locked")
    (:body (:p "And it's surely better that way.")
           (:p (:button :class "btn btn-default btn-sm"
                        :onclick "window.location='./'"
                        "Main Page")))))

(defhtml tags-dropdown ()
  (:div :class "tagsdropdown" ("Tag: ")
        (:select :id "tagdropdown"
                 :name "tag"
                 :required t
                 (:option :value ""
                          "- Select a tag - ")
                 (execute-query-loop tag (tags-query) ()
                   (when (or (nil-if-null (getf tag :|selected|))
                             (getf tag :|isglobal|))
                     (:option :value (getf tag :|tagid|)
                              (getf tag :|tagname|)))))))

(publish-page new-thread
  (when (banned-p)
    (redirect "/banned"))
  (standard-page
      (:title "New Thread")
    (:body
     (:form :action "b/submit-thread"
            :method "post"
            (:div :class "row"
                  (:div :class "col-xs-3 col-sm-2 col-md-1 subject-label"
                        "Subject:")
                  (:input :name "subject"
                          :type "text"
                          :id "subjectfield"
                          :class "col-xs-9 col-sm-10 col-md-11"
                          :required t))
            (:div :class "row"
                  (:textarea :id "postfield"
                             :name "body"
                             :rows 7
                             :cols 50
                             :class "col-xs-12"
                             :required t))
            (:div :class "row"
                  (tags-dropdown)
                  (:div :class "rightbuttons bottom"
                        (:input :id "submitbutton"
                                :class "btn btn-default btn-sm"
                                :name "Submit"
                                :type "submit"
                                :value "Submit")
                        (:input :onclick "window.location='/'"
                                :class "btn btn-default btn-sm"
                                :type "button"
                                :value "Back"))))
     (:div :class "row"
           (image-upload-form)))))

;;; pages that just do backend stuff then redirect
(publish-page b/login
  (let ((username (post-parameter "username"))
        (password (post-parameter "password")))
    (set-password-if-unset username password)
    (login-user username password)))

(defun login-user (username password)
  (let ((user-status nil))
    ;; if no status, user doesn't exist
    (if (and (setf user-status (get-user-status username))
             (is-correct-password-p username password))
        (let ((session-id nil))
          ;; find an id not in use and set it to session-id
          (loop while (gethash
                       (setf session-id (write-to-string (make-v4-uuid)))
                       *sessions*))

          (setf (gethash session-id *sessions*) (make-hash-table :test 'equal))

          ;; make life easier by making sure username in session is capitalized like in the DB
          (execute-query-one user
              "SELECT UserID, UserName FROM users WHERE lower(UserName) = lower(?)"
              (username)
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

(defun is-correct-password-p (user-name password)
  (execute-query-one user
      "SELECT passhash
       FROM users
       WHERE lower(username) = lower(?)" (user-name)
    (equal (signature password) (getf user :|passhash|))))

(defun set-password-if-unset (user-name password)
  (execute-query-one user
      "SELECT passhash
       FROM users
       WHERE lower(username) = lower(?)" (user-name)
    (when (or (equal (getf user :|passhash|) :null)
              (equal (getf user :|passhash|) ""))
      (execute-query-modify
       "UPDATE users
        SET passhash = ?
        WHERE lower(username) = lower(?)"
       ((signature password)
        user-name)))))

(publish-page b/submit-post
  (when (banned-p)
    (redirect "/banned"))
  (unless (or *allow-anonymity*
              (logged-in-p))
    (redirect "/"))

  (progn (if (and (thread-locked-p (get-parameter "thread"))
                  (not (user-authority-check-p "Moderator")))
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
              (let ((anonymous (post-parameter "anonymous")))
                (if *allow-anonymity* anonymous nil))
              (post-parameter "postcontent")
              (real-remote-addr)
              (post-parameter "reveal-op")
              (post-parameter "bump"))

           (redirect (format nil "/view-thread?post=~d"
                             (getf post :|postid|))))
         (redirect "/error")))

(publish-page b/submit-thread
  (when (banned-p)
    (redirect "/banned"))
  (unless (or *allow-anonymity*
              (logged-in-p))
    (redirect "/"))
  (unless (has-right-to-tag-p (post-parameter "tag"))
    (redirect "/"))
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

    (execute-query-one _
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
      (declare (ignore _))
      (redirect (format nil
                        "/view-thread?thread=~d"
                        (getf thread :|threadid|))))))

(defun has-right-to-tag-p (tag-id)
  (execute-query-one tag
      "SELECT true AS hasright
       FROM tags
       WHERE ((
             IsActive = true AND
             UserStatusID >= ? AND
             IsGlobal = false
            )
               OR (
             IsGlobal = true AND
              ? <= (SELECT UserStatusID
              FROM UserStatuses
              WHERE UserStatusDesc = 'Admin')
            )
       )
       AND tagid = ?"
      ((get-session-var 'userstatus)
       (get-session-var 'userstatus)
       tag-id)
    (getf tag :|hasright|)))

(publish-page b/logout
  (remhash (cookie-in *session-id-cookie-name*) *sessions*)
  (redirect "/"))

(defun site-symbol-to-fontawesome-class (site-symbol)
  (concatenate 'string
               "fa fa-"
               (string-downcase (symbol-name site-symbol))))

(defun site-symbol-to-name (site-symbol custom)
  (if custom
      custom
      (string-capitalize (symbol-name site-symbol))))

(defhtml sociallink (site url &optional custom-name)
  (:span :style "padding: 3px;"
         (link (:span :class (site-symbol-to-fontawesome-class site))
               url
               :new-tab t
               :tooltip (if custom-name
                            custom-name
                            (string-capitalize site)))))

(defhtml rightlink (label)
  (:span :class "header-link" (link label
                                    (concatenate 'string
                                                 "/"
                                                 (string-downcase label)))))

(defun print-username (post-id)
  (execute-query-one user "SELECT UserName,
                                  users.UserID,
                                  Anonymous,
                                  PostIP
                           FROM posts
                           LEFT JOIN users ON posts.UserID = users.UserID
                           WHERE PostID = ?"
      (post-id)
    (cond ((user-authority-check-p "Moderator")
           (if (not (null-p (getf user :|username|)))
               (values (with-html-string
                         (:a :href (format nil
                                           "view-thread?user=~a"
                                           (getf user :|userid|))
                             (getf user :|username|)))
                       (with-html-string
                         (:a :href (format nil
                                           "view-thread?ip=~a"
                                           (getf user :|postip|))
                             (getf user :|postip|))))
               (with-html-string
                 (:a :href (format nil
                                   "view-thread?ip=~a"
                                   (getf user :|postip|))
                     (getf user :|postip|)))))
          (t (if (and (and (not *force-anonymity*)
                           (not (getf user :|anonymous|)))
                      (not (null-p (getf user :|username|))))
                 (getf user :|username|)
                 *nameless-name*)))))

(defun print-post-options (post-id)
  (execute-query-one user "SELECT UserName,
          Anonymous,
          PostRevealedOP,
          Bump,
          PostIP,
          ThreadID
         FROM posts
         LEFT JOIN users ON posts.UserID = users.UserID
         WHERE PostID = ?" (post-id)
    (execute-query-one op "SELECT PostIP
         FROM posts
         WHERE ThreadID = ?
         ORDER BY PostID
         LIMIT 1"
        ((getf user :|threadid|))
      (let ((options '())
            (op-revealed (getf user :|postrevealedop|))
            (username (getf user :|username|))
            (anonymous (getf user :|anonymous|))
            (bump (getf user :|bump|))
            (post-ip (getf user :|postip|))
            (op-post-ip (getf op :|postip|))
            (we-are-moderator (user-authority-check-p "Moderator")))
        (if (and we-are-moderator
                 anonymous
                 (not (null-p username)))
            (setf options (cons "Anonymous" options)))
        (cond (op-revealed
               (setf options (cons "OP" options)))
              ((and we-are-moderator
                    (equal post-ip op-post-ip))
               (setf options (cons
                              (with-html-string
                                (:span :class "faded-text"
                                       "OP"))
                              options)))
              (bump
               (setf options (cons "Bump" options))))

        (if options
            (join-string-list options " | ")
            "")))))

(defhtml print-link-to-thread (thread-id thread-title &key locked stickied)
  (execute-query-one op
      "SELECT CONCAT(LEFT(PostContent, 200),
     CASE
          WHEN LENGTH(PostContent) > 200 THEN '...'
          ELSE ''
     END) AS PostContent
    FROM posts WHERE ThreadID = ?
    ORDER BY PostTime ASC" (thread-id)
    (if stickied
        (progn (:span :class "thread-icon glyphicon glyphicon-bookmark")
               (" ")))
    (if locked
        (progn (:span :class "thread-icon glyphicon glyphicon-lock")
               (" ")))
    (:a :title (getf op :|postcontent|)
        :href
        (concatenate 'string
                     "view-thread?thread="
                     (write-to-string thread-id))
        thread-title)))

(defhtml tags-filter-dropdown ()
  (:a :class "dropdown-toggle btn btn-default btn-sm"
      :style "margin-left: 3px;"
      :data-toggle "dropdown"
      "Boards " (:b :class "caret"))
  (:ul :class "dropdown-menu dropdown-menu-form pull-right"
       :role "menu"
       (execute-query-loop tag (tags-query) ()
         (unless (getf tag :|isglobal|)
           (with-html (:li (:label
                            (:input :type "checkbox"
                                    :name (getf tag :|tagid|)
                                    :checked (nil-if-null (getf tag :|selected|)))
                            (getf tag :|tagname|))))))))

(defhtml pagination ()
  (let ((thread-id (get-parameter "thread"))
        (user-id (get-parameter "user"))
        (user-ip (get-parameter "ip")))
    (execute-query-one thread
        (format nil "SELECT count(1) AS PostCount
                   FROM posts
                   WHERE True
                   ~a"
                (concatenate 'string
                             (unless (equal (empty-string-if-nil thread-id) "")
                               (format nil " AND ThreadID = ~d"
                                       (dbi.driver:escape-sql *conn* thread-id)))
                             (unless (equal (empty-string-if-nil user-id) "")
                               (format nil " AND UserID = ~d"
                                       (dbi.driver:escape-sql *conn* user-id)))
                             (unless (equal (empty-string-if-nil user-ip) "")
                               (format nil " AND PostIP = '~d'"
                                       (dbi.driver:escape-sql *conn* user-ip)))))
        ()
      ;; mobile
      (let ((num-of-pages (ceiling (/ (getf thread :|postcount|)
                                      *posts-per-page*)))
            (page (parse-integer (if (get-parameter "page")
                                     (get-parameter "page")
                                     "1")))
            (thread-param (cond (thread-id (format nil "&thread=~d" thread-id))
                                (user-id (format nil "&user=~d" user-id))
                                (user-ip (format nil "&ip=~d" user-ip)))))
        (when (> num-of-pages 1)
          (:div :class "visible-xs-inline"
                :style "float: right;"
                (:a :class "btn btn-sm btn-default"
                    :href (format nil
                                  (concatenate 'string
                                               "view-thread?page=~d"
                                               thread-param)
                                  (- page 1))
                    ("<"))
                (:select :name "Page"
                         :class "pagination"
                         :onchange "goToPage(this)"
                         (do ((i 1 (1+ i)))
                             ((> i num-of-pages))
                           (:option :value (stringify i)
                                    :selected (= i page)
                                    i)))
                (:a :class "btn btn-sm btn-default"
                    :href (format nil
                                  (concatenate 'string
                                               "view-thread?page=~d"
                                               thread-param)
                                  (+ page 1))
                    (">")))

          ;; non-mobile
          (let ((start-page (- page 1)))
            (:ul :style "float: right; margin: 3px;"
                 :class "pagination pagination-sm hidden-xs"
                 ;; if on page higher than 3, it'll look like
                 ;; < 1 ... (- 1 page) page (+ 1 page) ... num-of-pages >
                 (if (>= page 3)
                     (:li :class (if (= page start-page) "active")
                          (:a :href (concatenate 'string
                                                 "view-thread?page=1"
                                                 thread-param)
                              1)))
                 (if (>= page 4)
                     (:li :class "disabled"
                          (:a :href "#" "...")))
                 (do ((i 1 (1+ i))
                      (j start-page (1+ j)))
                     ((or (> i 3)
                          (> j num-of-pages)))
                   (if (and (> j 0)
                            (<= j num-of-pages))
                       (:li :class (if (= j page) "active")
                            (:a :href (format nil (concatenate 'string
                                                               "view-thread?page=~d"
                                                               thread-param)
                                              j)
                                j))))
                 (if (< page (- num-of-pages 2))
                     (:li :class "disabled"
                          (:a :href "#" "...")))
                 (if (< page (- num-of-pages 1))
                     (:li :class (if (= page num-of-pages) "active")
                          (:a :href (format nil (concatenate 'string
                                                             "view-thread?page=~d"
                                                             thread-param)
                                            num-of-pages)
                              num-of-pages))))))))))

(publish-page b/unfollow
  (let ((thread-id (get-parameter "thread"))
        (f (get-parameter "f")))
    (unfollow-thread thread-id (get-session-var 'userid) (real-remote-addr))
    (redirect (format nil "/~a" (if (not (equal f "NIL"))
                                    (format nil "?f=~a" f)
                                    "")))))

(publish-page b/hide-thread
  (let ((thread-id (get-parameter "thread"))
        (user-id (get-session-var 'userid))
        (user-ip (real-remote-addr)))
    (if (hiding-thread-p thread-id user-id user-ip)
        (progn (unhide-thread thread-id user-id user-ip)
               (redirect "/?f=hidden"))
        (progn (hide-thread thread-id user-id user-ip)
               (redirect "/")))))

(defun hide-thread (thread-id user-id user-ip)
    (execute-query-modify
     (format nil
             "INSERT INTO hidden (
                  ~a,
                  threadid
              )
              VALUES (
                  ?,
                  ?
              )"
             (if (logged-in-p) "userid" "userip"))
     ((if (logged-in-p) user-id user-ip)
      thread-id)))

(defun hiding-thread-p (thread-id user-id user-ip)
  (execute-query-one hidden
      (format nil
              "SELECT true AS ishiding
               FROM hidden
               WHERE ~a = ?
               AND threadid = ?"
              (if (logged-in-p) "userid" "userip"))
      ((if (logged-in-p) user-id user-ip)
       thread-id)
    (getf hidden :|ishiding|)))

(defun unhide-thread (thread-id user-id user-ip)
  (execute-query-modify
   (format nil
           "DELETE FROM hidden
            WHERE ~a = ?
            AND threadid = ?"
           (if (logged-in-p) "userid" "userip"))
   ((if (logged-in-p) user-id user-ip)
    thread-id)))

(defun follow-thread (thread-id user-id user-ip)
  (unless (following-thread-p thread-id user-id user-ip)
    (execute-query-modify
     (format nil
             "INSERT INTO following (
                  ~a,
                  threadid
              )
              VALUES (
                  ?,
                  ?
              )"
             (if (logged-in-p) "userid" "userip"))
     ((if (logged-in-p) user-id user-ip)
      thread-id))))

(defun following-thread-p (thread-id user-id user-ip)
  (execute-query-one following
      (format nil
              "SELECT true AS isfollowing
               FROM following
               WHERE ~a = ?
               AND threadid = ?"
              (if (logged-in-p) "userid" "userip"))
      ((if (logged-in-p) user-id user-ip)
       thread-id)
    (getf following :|isfollowing|)))

(defun set-last-seen-post (thread-id user-id user-ip page)
  (let ((last-seen (get-last-seen-post thread-id user-id user-ip)))
    (if (or (null-p last-seen)
            (< last-seen
               (get-max-post-on-page thread-id page)))
        (execute-query-modify
         (format nil
                 "UPDATE following
                  SET lastseenpost = (SELECT MAX(postid)
                                      FROM (SELECT postid
                                            FROM posts
                                            WHERE threadid = ?
                                            ORDER BY posttime ASC
                                            OFFSET ?
                                            LIMIT ?) t1)
                  WHERE threadid = ?
                    AND ~a = ?"
                 (if (logged-in-p) "userid" "userip"))
         (thread-id
          (* *posts-per-page* (- page 1))
          *posts-per-page*
          thread-id
          (if (logged-in-p) user-id user-ip))))))

(defun unfollow-thread (thread-id user-id user-ip)
  (execute-query-modify
   (format nil
           "DELETE FROM following
            WHERE ~a = ?
            AND threadid = ?"
           (if (logged-in-p) "userid" "userip"))
   ((if (logged-in-p) user-id user-ip)
    thread-id)))

(defun get-last-seen-post (thread-id user-id user-ip)
  (execute-query-one post
      (format nil
              "SELECT lastseenpost
               FROM following
               WHERE ~a = ?
                 AND threadid = ?"
              (if (logged-in-p) "userid" "userip"))
      ((if (logged-in-p) user-id user-ip)
       thread-id)
    (getf post :|lastseenpost|)))

(defun get-max-post-on-page (thread-id page)
  (execute-query-one post
      "SELECT MAX(postid) AS MaxPostID
       FROM (SELECT postid
                    FROM posts
                    WHERE threadid = ?
                    ORDER BY posttime ASC
                    OFFSET ?
                    LIMIT ?) t1"
      (thread-id
       (* *posts-per-page* (- page 1))
       *posts-per-page*)
    (getf post :|maxpostid|)))

(defun get-max-post-in-thread (thread-id)
  (execute-query-one post
      "SELECT MAX(postid) AS MaxPostID
       FROM posts
       WHERE threadid = ?"
      (thread-id)
    (getf post :|maxpostid|)))

(publish-page b/apply-tags
  (let ((tags (loop for tag in (post-parameters*)
                    collect (car tag))))
    (set-tags tags (get-session-var 'userid) (real-remote-addr)))
  (redirect "/"))

(defun set-tags (tags user-id user-ip)
  (clear-tags-for-user user-id user-ip)
  (loop for tagid in tags
        do (execute-query-modify
            (format nil
                    "INSERT INTO selectedtags (
                         ~a,
                         tagid
                     )
                     VALUES (
                         ?,
                         ?
                     )"
                    (if (logged-in-p) "userid" "userip"))
            ((if (logged-in-p) user-id user-ip)
             tagid))))

(defun clear-tags-for-user (user-id user-ip)
  (execute-query-modify
   (format nil
           "DELETE FROM selectedtags
            WHERE ~a = ?"
           (if (logged-in-p) "userid" "userip"))
   ((if (logged-in-p) user-id user-ip))))

;;; FIXME: i have no idea how password encryption works so i just copied this
;;;        from somewhere, probably needs improved
(defconstant +hash-size+ 32)
(defconstant +encoded-hash-size+ (* 5/4 +hash-size+))

(defvar *signing-key*)

(defun randomize-signing-key ()
  (setf *signing-key*
        (map-into (make-array +hash-size+ :element-type '(unsigned-byte 8))
                  (lambda () 123))))

(defun signature (string &key (start 0))
  (unless (boundp '*signing-key*)
    (log-message* :warn "Signing key is unbound.  Using Lisp's RANDOM function to initialize it.")
    (randomize-signing-key))
  (let ((state (sha3:sha3-init :output-bit-length (* 8 +hash-size+))))
    (sha3:sha3-update state (babel:string-to-octets string :start start))
    (sha3:sha3-update state *signing-key*)
    (binascii:encode-base85 (sha3:sha3-final state))))

(publish-page ban-post
  (unless (user-authority-check-p "Moderator")
    (redirect "/"))
  (let ((post-id (get-parameter "post")))
    (execute-query-one user
        "SELECT CASE WHEN users.userid IS NOT NULL THEN users.username
                     ELSE postip
                END AS username
         FROM posts
         LEFT JOIN users ON posts.userid = users.userid
         WHERE postid = ?"
        (post-id)
      (standard-page
          (:title (format nil "Ban User ~a" (getf user :|username|)))
        (:body
         (:form :method "post"
                :action (format nil "a/ban-post?post=~d" post-id)
                (row (col 12
                       "Ban until: " (:input :name "bandate"
                                             :id "datepicker"
                                             :type "text"
                                             :required t)))
                (row (col 12
                       "Ban reason:" (:br)
                       (:textarea :name "banreason"
                                  :rows 7
                                  :cols 50
                                  :required t)))
                (:input :class "btn btn-default btn-sm"
                        :type "submit"
                        :value "Submit")))))))

(publish-page a/ban-post
  (unless (user-authority-check-p "Moderator")
    (redirect "/"))

  (ban-user-by-post (get-parameter "post")
                    (post-parameter "bandate")
                    (post-parameter "banreason"))
  (redirect "/"))

(defun ban-user-by-post (post-id ban-end-time ban-reason)
  (execute-query-one post
      "SELECT userid,
              postip
       FROM posts
       WHERE postid = ?"
      (post-id)
    (execute-query-modify
     "INSERT INTO bans (
          bannerid,
          banneeid,
          banneeip,
          bantime,
          banend,
          banreason,
          postid
      )
      VALUES (
          ?,
          ?,
          ?,
          current_timestamp,
          ?,
          ?,
          ?
      )"
     ((get-session-var 'userid)
      (getf post :|userid|)
      (getf post :|postip|)
      ban-end-time
      ban-reason
      post-id))))

(defun banned-p ()
  (execute-query-one ban
      "SELECT true AS banned
       FROM bans
       WHERE (banneeid = ? OR banneeip = ?)
         AND banend > ?"
      ((get-session-var 'userid)
       (real-remote-addr)
       (format-timestring nil (now)))
    (getf ban :|banned|)))

(publish-page banned
  (unless (banned-p)
    (redirect "/"))

  (standard-page
      (:title "Hey idiot, you're banned.")
    (:body (:br)
           (execute-query-loop bans
               "SELECT banend,
                        banreason,
                        postcontent
                 FROM bans
                 JOIN posts ON bans.postid = posts.postid
                 WHERE (banneeid = ? OR banneeip = ?)
                   AND banend > ?
                 ORDER BY banend DESC"
               ((get-session-var 'userid)
                (real-remote-addr)
                (format-timestring nil (now)))
             (:p "Your ban will expire on "
                 (:span :class "time" (local-time:to-rfc3339-timestring
                                       (local-time:universal-to-timestamp
                                        (getf bans :|banend|)))))
             (:p (format nil "Reason for ban: ~a" (getf bans :|banreason|)))
             (:p (format nil "Post that got you banned: \"~a\""
                         (getf bans :|postcontent|)))
             (:hr)))))

(publish-page a/sticky-thread
  (unless (user-authority-check-p "Moderator")
    (redirect "/"))
  (execute-query-modify
   "UPDATE threads
    SET stickied = NOT stickied
    WHERE threadid = ?"
   ((get-parameter "thread")))
  (redirect "/"))

(publish-page a/lock-thread
  (unless (user-authority-check-p "Moderator")
    (redirect "/"))
  (execute-query-modify
   "UPDATE threads
    SET locked = NOT locked
    WHERE threadid = ?"
   ((get-parameter "thread")))
  (redirect "/"))

(publish-page edit-post
  (when (banned-p)
    (redirect "/banned"))
  (unless (user-authority-check-p "Moderator")
    (redirect "/"))
  (standard-page
      (:title "Edit Thread")
    (:body
     (:form :action "/a/submit-post-edit"
            :method "post"
            (:input :type "hidden"
                    :value (get-parameter "post")
                    :name "post")
            (:div :class "row"
                  (:textarea :id "postfield"
                             :name "body"
                             :rows 7
                             :cols 50
                             :class "col-xs-12"
                             :required t))
            (:div :class "row"
                  (:div :class "rightbuttons bottom"
                        (:input :id "submitbutton"
                                :class "btn btn-default btn-sm"
                                :name "Submit"
                                :type "submit"
                                :value "Submit")
                        (:input :onclick "window.location='/'"
                                :class "btn btn-default btn-sm"
                                :type "button"
                                :value "Back"))))
     (:div :class "row"
           (image-upload-form)))))

(publish-page a/submit-post-edit
  (unless (user-authority-check-p "Moderator")
    (redirect "/"))
  (execute-query-modify
   "UPDATE posts
    SET
        posteditcontent = ?,
        postlasteditby = ?
    WHERE postid = ?"
   ((post-parameter "postfield")
    (get-session-var 'userid)
    (post-parameter "post")))
  (redirect (format nil
                    "/view-thread?post=~d"
                    (post-parameter "post"))))

(publish-page b/upload-file
  (destructuring-bind
      (path file-name content-type)
      (post-parameter "upload")
    (upload-file path file-name content-type (post-parameter "filename") *file-upload-path*)))

(defun upload-file (path file-name content-type upload-file-name upload-path)
  (let ((allowed-extensions '("gif" "jpeg" "jpg" "png" "webm"))
        (allowed-types '("image/gif" "image/jpeg" "image/jpg" "image/pjpeg"
                         "image/x-png" "image/png" "video/webm"))
        (file-ext (second (split-sequence:split-sequence #\. file-name)))
        (new-path (make-pathname :defaults path
                                 :directory (pathname-directory upload-path)
                                 :name upload-file-name)))
    (log-message* "NOTE" "Uploading file ~a of content type ~a to server. (old path: ~a~tnew path: ~a)"
                  file-name content-type path new-path)
    (if (and (position content-type allowed-types :test 'equal)
             (position file-ext allowed-extensions :test 'equal))
        (progn (rename-file path new-path)
               (log-message* "NOTE" "Successfully uploaded ~a" file-name)
               ;; set error to none so uploadfile.js knows file has been uploaded
               (with-html-string (:raw "<script>var error = 'none';</script>")))
        (log-message* "WARN" "Did not upload file ~a because its content-type ~a or extension ~a are not allowed."
                      file-name content-type file-ext))))

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
         (let* ((search (get-parameter "search"))
                (search-for-query (substitute-if #\%
                                                 #'whitespacep
                                                 (dbi.driver:escape-sql
                                                  *conn*
                                                  (empty-string-if-nil search)))))
           (values (format nil "Search for \"~a\""
                           (empty-string-if-nil search))
                   (format nil "(SELECT 1
                                 FROM posts
                                 WHERE posts.ThreadID = IndexThreads.ThreadID
                                   AND PostContent LIKE '%~a%'
                                 LIMIT 1) = 1
                                 OR ThreadSubject LIKE '%~a%'"
                           search-for-query search-for-query)
                   nil)))
        ((equal type "following")
         (values "Following"
                 (if (logged-in-p)
                     (format nil "threadid IN (SELECT threadid
                                               FROM following
                                               WHERE userid = ~d)"
                             (dbi.driver:escape-sql
                              *conn* (stringify (get-session-var 'userid))))
                     (format nil "threadid IN (SELECT threadid
                                               FROM following
                                               WHERE userip = '~a')"
                             (dbi.driver:escape-sql
                              *conn* (real-remote-addr))))
                 "ORDER BY latestposttime DESC"))
        (t "")))

(defun whitespacep (c)
  (member c '(#\  #\Tab #\Return #\Newline)))

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
