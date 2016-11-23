(in-package :net.ignorama.web)

(defparameter *ignorama-purple* "#330066")
(defparameter *background-color* "#E8E8E8")
(defparameter *header-text-color* "white")

;;; site setup
(defun threads-query (condition)
  (format nil "SELECT *
               FROM IndexThreads
               WHERE (~a)
                 AND UserStatusID >= ~d
               LIMIT ~d"
          (if condition
              condition
              :true)
          (user-status-id)
          *index-row-limit*))

(defun tags-query ()
  (let ((user-status-id (user-status-id)))
    (format nil "SELECT TagID, TagName
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
                      :href "/signup" "Sign up")
                  (:span :class "header-link" :style "color: white;" "/")
                  (:a :class "header-link"
                      :href "/login" "Log in")))))

;;; page skeleton
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *header*
    '((row :class "header" :style (format nil "background-color: ~a" *ignorama-purple*)
           (col 12

             ;; logo and slogans
             (:div :class "container"
                   (row
                     (desktop-only
                       (col 6 :style "padding-top: 4px;"
                            (:a :href "/"
                                (:img :src *logo-path*))
                            (if *slogans*
                                (:b :style (format nil "position: absolute;
              top: 15px; color: ~a" *header-text-color*)
                                    (:raw (random-elt *slogans*))))))
                     (mobile-only
                       (col 6 :style "padding-top: 15px;"
                            (:a :href "/"
                                (:img :src *logo-path*))))

                     ;; right links
                     (col 6
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
       (:div :style "overflow-x: hidden;"
             ,@*header*
             (:div :class "container"
                   (unless (equal ,title "")
                     (:h2 ,title))
                   ,@body))))))

;;; this macro creates and publishes page <name> at https://your-site.com/<name>
(defmacro publish-page (name &body body)
  `(hunchentoot:define-easy-handler (,name
                                     :uri ,(string-downcase
                                            (if (equal 'index name)
                                                "/"
                                                (concatenate 'string "/" (symbol-name name))))) ()
     (setf (hunchentoot:content-type*) "text/html")
     (let ((*conn* *conn*))
       (with-db *conn*
         ,@body))))

(publish-page index
  (multiple-value-bind (title condition)
      (index-params-by-type (get-parameter "f"))
    (standard-page
        (:title title)
      (:body (index-buttons)
             (threads-table (threads-query condition))
             (:div :style "padding-top: 15px;"
                   (:raw *fake-copyright*))))))

(defhtml index-buttons ()
  ;; dropdown only displays correctly when I wrap all the buttons in this div
  (row :class "dropdown"
       (col 12 :style "margin-top: 5px; margin-bottom: 5px;"
            (:button :class "btn btn-default btn-sm threads"
                     :style "margin: 3px;"
                     :onclick "window.location='new-thread'"
                     "New Thread")

            (:form :style "float: right;"
                   :action "b/apply-tags"
                   :method "post"

                   (:input :type "button"
                           :style "margin: 3px;"
                           :class "btn btn-default btn-sm"
                           :onclick "window.location='b/reset-tags'"
                           :value "Reset Boards")
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
    (col 12 :class "thread"
         (print-link-to-thread id subject :locked locked :stickied stickied)
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
                                                         latest-post-time))))
                  " | "))))))

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
                 (getf post :|postcontent|)))))

(defhtml post-row (id time content)
  (row
    (col 12 :class "thread"
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
                             time)))
         (:br)
         (:div (format-post content)))))

(defhtml thread-buttons ()
  (let ((thread-id (get-parameter "thread")))
    (when thread-id
      (if (thread-locked-p thread-id)
          (:button :class "btn btn-default btn-sm"
                   :disabled t
                   "Locked")
          (:button :class "btn btn-default btn-sm"
                   :onclick (format nil "window.location='new-reply?thread=~d'"
                                    (get-parameter "thread"))
                   "Reply"))))
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

    (unless (and (or user-id user-ip)
                 (user-authority-check-p "Moderator"))
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
                     (:script (view-thread-js)))))
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

(defhtml image-upload-form ()
  (:form :class "col-xs-12"
         :id "uploadForm"
         :action "b/upload-file"
         :method "post"
         :enctype "mutlipart/form-data"
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
                 :value "none")))

(defhtml reply-buttons ()
  (:span :class "reply button-row checkboxes"
         (if (is-op-p (get-parameter "thread"))
             (:input :name "reveal-op"
                     :type "checkbox"
                     "Reveal OP Status? ")
             (:input :name "bump"
                     :type "checkbox"
                     "Bump! "))
         (if (logged-in-p)
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
  (if (thread-locked-p (get-parameter "thread"))
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
                (reply-buttons)
                (image-upload-form))))))

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
                   (:option :value (getf tag :|tagid|)
                            (getf tag :|tagname|))))))

(publish-page new-thread
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
                                :value "Back")))
            (:div :class "row"
                  (image-upload-form))))))

;;; pages that just do backend stuff then redirect
(publish-page b/login
  (let ((username (post-parameter "username"))
        (password (post-parameter "password")))
    (set-password-if-unset username password)
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
          (redirect "/login-failed")))))

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
           (if (not (is-null (getf user :|username|)))
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
          (t (if (and (or (and (not *force-anonymity*)
                               (not (getf user :|anonymous|)))
                          (not *allow-anonymity*))
                      (not (is-null (getf user :|username|))))
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
                 (not (is-null username)))
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
      :style "margin-left: -1px;"
      :data-toggle "dropdown"
      "Boards " (:b :class "caret"))
  (:ul :class "dropdown-menu dropdown-menu-form pull-right"
       :role "menu"
       (execute-query-loop tag (tags-query) ()
         (:li (:label
               (:input :type "checkbox"
                       :name (getf tag :|tagid|))
               (getf tag :|tagname|))))))

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

(defconstant +hash-size+ 32)
(defconstant +encoded-hash-size+ (* 5/4 +hash-size+))

(defvar *signing-key*)

(defun randomize-signing-key ()
  (setf *signing-key*
        (map-into (make-array +hash-size+ :element-type '(unsigned-byte 8))
                  (lambda () (random 256)))))

(defun signature (string &key (start 0))
  (unless (boundp '*signing-key*)
    (log-message* :warn "Signing key is unbound.  Using Lisp's RANDOM function to initialize it.")
    (randomize-signing-key))
  (let ((state (sha3:sha3-init :output-bit-length (* 8 +hash-size+))))
    (sha3:sha3-update state (babel:string-to-octets string :start start))
    (sha3:sha3-update state *signing-key*)
    (binascii:encode-base85 (sha3:sha3-final state))))
