;; -*- coding:utf-8 -*-
(in-package :net.ignorama.web)

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

(defmacro row (&body body)
  `(with-html
     (:div :class "row"
           ,@body)))

(defmacro col (size &body body)
  `(with-html
     (:div :class (format nil "col-xs-~d" ,size)
           ,@body)))

(defmacro col-xs (size &body body)
  `(with-html
     (:div :class (format nil "col-xs-~d" ,size)
           ,@body)))

(defmacro col-sm (size &body body)
  `(with-html
     (:div :class (format nil "col-sm-~d" ,size)
           ,@body)))

(defmacro col-md (size &body body)
  `(with-html
     (:div :class (format nil "col-md-~d" ,size)
           ,@body)))

(defmacro col-lg (size &body body)
  `(with-html
     (:div :class (format nil "col-lg-~d" ,size)
           ,@body)))

(defmacro desktop-only (&body body)
  `(with-html
     (:span :class "hidden-xs"
            ,@body)))

(defmacro mobile-only (&body body)
  `(with-html
     (:span :class "visible-xs-inline"
            ,@body)))

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
             :href "/static/style.css")

      (:script :src "//code.jquery.com/jquery-1.11.0.min.js")
      (:script :src "//maxcdn.bootstrapcdn.com/bootstrap/3.2.0/js/bootstrap.min.js")
      (:script :src "/static/script.js")
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
  (:div :class "header rightlinks"
        (rightlinks-desktop rightlinks sociallinks)
        (rightlinks-mobile rightlinks sociallinks)))

(defhtml rightlinks-desktop (rightlinks sociallinks)
  (desktop-only
    (:div
     (:div (generate-sociallinks sociallinks)
           (generate-rightlinks rightlinks))

     (login-links-desktop))))

(defhtml rightlinks-mobile (rightlinks sociallinks)
  (mobile-only
    (:div
     (:div :class "btn-group mobile header rightlinks"
           (:a :class "btn btn-default btn-sm dropdown-toggle"
               :data-toggle "dropdown"
               "Menu " (:span :class "caret"))
           (:ul :class "dropdown-menu pull-right"
                (generate-dropdown-links rightlinks)
                (generate-dropdown-links-social sociallinks)))
     (:br)
     (:br)
     (login-links-mobile))))

(defhtml login-links-desktop ()
  (if (logged-in-p)
      (:div :class "header loginlinks logout-area"
            (format nil "Logged in as ~a " (get-session-var 'username))
            (:a :href "/b/logout"
                "(logout)"))
      (:div :class "header loginlinks"
            (:a :class "header rightlink"
                :href "/signup" "Sign up")
            ("/")
            (:a :class "header rightlink"
                :href "/login" "Log in"))))

(defhtml login-links-mobile ()
  (if (logged-in-p)
      (:div :class "mobile-login-links"
            (:span (format nil "Logged in as ~a" (get-session-var 'username))
                   (:a :href "/b/logout"
                       (:span :class "mobile-login-link"
                              "(logout)"))))
      (:div :class "mobile-login-links"
            (:a :class "header rightlink mobile-login-link"
                :href "/signup" "Sign up")
            ("/")
            (:a :class "header rightlink mobile-login-link"
                :href "/login" "Log in"))))

;;; page skeleton
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *header*
    '((:div :class "header banner"
           (:div :class "header text"

                 ;; logo and slogans
                 (:span :class "hidden-xs"
                        (:a :href "/"
                            (:img :class "header logo" :src *logo-path*))
                        (if *slogans*
                            (:b :class "hidden-sm header slogan"
                                (:raw (random-elt *slogans*)))))
                 (:span :class "visible-xs-inline"
                        (:a :href "/"
                            (:img :class "header logo small" :src *small-logo-path*)))

                 ;; right links
                 (rightlinks *rightlinks* *sociallinks*))))))

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
       (:div :class "container"
             ,@*header*
             (unless (equal ,title "")
               (:h2 ,title))
             ,@body)))))

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
             (:div :class "fake-copyright"
                   (:raw *fake-copyright*))))))

(defhtml index-buttons ()
  ;; dropdown only displays correctly when I wrap all the buttons in this div
  (row :class "dropdown"
       (col 12
            (:button :class "btn btn-default btn-sm threads"
                     :onclick "window.location='new-thread'"
                     "New Thread")

            (:form :class "rightbuttons"
                   :action "b/apply-tags"
                   :method "post"

                   (desktop-only (index-buttons-desktop))
                   (mobile-only (index-buttons-mobile))

                   (tags-filter-dropdown))

            (desktop-only (search-box)))))

(defhtml index-buttons-desktop ()
  (:input :type "button"
          :class "btn btn-default btn-sm threads reset-tags"
          :onclick "window.location='b/reset-tags'"
          :value "Reset Boards")
  (:input :type "submit"
          :class "btn btn-default btn-sm threads"
          :value "Apply Boards"))

(defhtml index-buttons-mobile ()
  (:input :type "button"
          :class "btn btn-default btn-sm threads reset"
          :onclick "window.location='b/reset-tags'"
          :value "Reset")
  (:input :type "submit"
          :class "btn btn-default btn-sm threads"
          :value "Apply"))

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
         (:button :style "margin-top: -3px; margin-right: 4px;"
                  :class "btn btn-default btn-sm"
                  :type "submit"
                  (:span :class "glyphicon glyphicon-search"))))

(defhtml threads-table (query)
  (execute-query-loop thread query ()
    (:div :style ""
          (thread-row (getf thread :|threadid|)
                      (getf thread :|threadsubject|)
                      (getf thread :|tag|)
                      (getf thread :|postcount|)
                      (getf thread :|latestposttime|)
                      (getf thread :|locked|)
                      (getf thread :|stickied|)))))

(defhtml thread-row (id subject tag post-count latest-post-time locked stickied)
  (row
    (col 12 :class "thread"
         (link subject (thread-url id))
         (:br)
         (:span :style "font-size: 12px; color: gray;"
                (:raw
                 (join-string-list
                  (list
                   (format nil "Board: ~a" tag)
                   (format nil "Replies: ~a" post-count)
                   (format nil "Last reply: ~a" (with-html-string
                                                  (:span :class "time"
                                                         latest-post-time))))
                  " | "))))))

(defhtml link (text url)
  (:a :href url text))

(defun thread-url (id)
  (format nil "view-thread?thread=~d" id))

;; (defhtml threads-table (query)
;;   (desktop-only (threads-table-desktop query))
;;   (mobile-only (threads-table-mobile query)))

(defhtml threads-table-mobile (query)
  (:table :class "table table-bordered fixed main-table"
          (threads-table-header-mobile)
          (threads-table-rows-mobile query)))

(defhtml threads-table-rows-mobile (query)
  (execute-query-loop thread query ()
    (:tr (thread-name-cell-mobile thread))))

(defhtml thread-name-cell-mobile (thread)
  (:td :class "thread-name centered" (thread-link thread)
       (mobile-post-count thread)
       (:div (mobile-board thread))
       (:div (:raw (mobile-last-post-time thread)))
       (:div (print-user-name-and-ip (getf thread
                                           :|postid|)))))

(defhtml mobile-post-count (thread)
  (format nil " (~d)"
          (getf thread :|postcount|)))

(defhtml mobile-board (thread)
  (format nil "Board: ~a"
          (getf thread :|tag|)))

(defhtml mobile-last-post-time (thread)
  (format nil "Latest Post: ~a"
          (with-html-string
            (:span :class "time"
                   (getf thread
                         :|latestposttime|)))))

(defhtml threads-table-desktop (query)
  (:table :class "table table-bordered fixed main-table"
          (threads-table-header-desktop)
          (threads-table-rows-desktop query)))

(defhtml threads-table-rows-desktop (query)
  (execute-query-loop thread query ()
    (:tr (thread-name-cell thread)
         (thread-user-name-cell thread)
         (thread-easy-cell thread :|postcount|)
         (thread-easy-cell thread :|tag|)
         (thread-last-post-cell thread :|latestposttime|))))

(defhtml thread-name-cell (thread)
  (:td :class "thread-name centered" (thread-link thread)))

(defun thread-link (thread)
  (print-link-to-thread (getf thread :|threadid|)
                        (getf thread :|threadsubject|)
                        :locked (getf thread :|locked|)
                        :stickied (getf thread :|stickied|)))

(defhtml thread-last-post-cell (thread selector)
  (:td :class "thread-row centered time"
       (getf thread selector)))

(defhtml thread-easy-cell (thread selector)
  (:td :class "thread-row centered"
       (getf thread selector)))

(defhtml thread-user-name-cell (thread)
  (:td :class "thread-row centered"
       (print-user-name-and-ip (getf thread :|postid|))))

(defhtml print-user-name-and-ip (post-id)
  (multiple-value-bind (name ip)
      (print-username post-id)
    (:div name)
    (:div ip)))

(defhtml threads-table-header-desktop ()
  (:tr :class "thread-row"
       (:th :class "thread-row"
            "Thread")
       (:th :class "thread-row centered col-sm-2"
            "User")
       (:th :class "thread-row centered col-md-1 col-sm-2"
            "Replies")
       (:th :class "thread-row centered col-sm-3 col-md-2"
            "Board")
       (:th :class "thread-row centered col-sm-2"
            "Latest Post")))

(defhtml threads-table-header-mobile ()
  (:th :class "thread-row"
       "Threads"
       (:div :class "mobile-search"
             (search-box))))

(publish-page following
  (redirect "/?f=following"))

(publish-page hidden
  (redirect "/?f=hidden"))

(defmacro posts-table (query &rest params)
  `(with-html (:table :class "table table-bordered fixed main-table"
                      (:tbody
                       (execute-query-loop post ,query (,@params)
                         (let ((post-id (getf post :|postid|))
                               (post-time (getf post :|posttime|)))
                           (:tr :id (concatenate 'string
                                                 "post"
                                                 (write-to-string
                                                  post-id))
                                (:td :class "col-sm-3 hidden-xs"
                                     (:div :class "post-info"
                                           (multiple-value-bind (name ip)
                                               (print-username
                                                (getf post :|postid|))
                                             (:b (:div name))
                                             (:div ip))
                                           (let ((options (print-post-options post-id)))
                                             (if options
                                                 (:raw options)))
                                           (:br)
                                           (:br)
                                           (:div :class "time" post-time)))
                                (:td :class "col-sm-9 post-content centered"
                                     (:div :class "visible-xs mobile-post-info"
                                           (:span :class "time mobile-date"
                                                  post-time)
                                           (:span (multiple-value-bind (name ip)
                                                      (print-username
                                                       (getf post :|postid|))
                                                    (:div (:b name))
                                                    (:div ip))
                                                  (:raw (print-post-options post-id)))
                                           )
                                     (:div (format-post (getf post :|postcontent|)))))))))))

(defhtml thread-buttons ()
  (:button :class "btn btn-default btn-sm"
           :onclick (format nil "window.location='new-reply?thread=~d'"
                            (get-parameter "thread"))
           "Reply")
  (:button :class "btn btn-default btn-sm"
           :onclick "window.location='/'"
           "Main Page")

  (pagination))

(defhtml thread-dropdown ()
  (:span :class "btn-group rightbuttons"
         (:a :class "btn btn-default btn-sm dropdown-toggle"
             :data-toggle "dropdown"
             :href "#"
             (:span :class "caret"))
         (:ul :class "dropdown-menu pull-right"
              "TODO - add stuff here")))

(publish-page view-thread
  ;; if passed "post" parameter, redirect to appropriate thread and highlight post
  (if (get-parameter "post")
      (execute-query-one thread
          "SELECT ThreadID FROM posts WHERE PostID = ?"
          ((get-parameter "post"))
        (redirect (format nil
                          "/view-thread?thread=~d&highlight=~d#post~d"
                          (getf thread :|threadid|)
                          (get-parameter "post")
                          (get-parameter "post")))))

  (if (not (equal (empty-string-if-nil (get-parameter "thread"))
                  ""))
      (standard-page
          (:title (get-thread-title (get-parameter "thread")))
        (:body (thread-buttons)
               (thread-dropdown)
               (posts-table "SELECT *
                             FROM posts
                             WHERE ThreadID = ?
                             ORDER BY PostTime
                             LIMIT ?
                             OFFSET ?"
                            (get-parameter "thread")
                            *posts-per-page*
                            (if (get-parameter "page")
                                (* (- (parse-integer
                                       (get-parameter "page")) 1)
                                   *posts-per-page*)
                                0))
               (thread-buttons)
               (:div :class "fake-copyright"
                     (:raw *fake-copyright*))
               (:script (view-thread-js))))
      (redirect "/")))

(publish-page login
  (standard-page
      (:title "Login")
    (:body
     (:form :method "POST" :action "/b/login"
            (:div (:input :name "username" :type "text"))
            (:div (:input :name "password" :type "password"))
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
  (:a :class "header rightlink"
      :target "_blank"
      :href url
      :title (site-symbol-to-name site custom-name)
      (:span :class (site-symbol-to-fontawesome-class site))))

(defhtml rightlink (label)
  (:a :class "header rightlink"
      :href (concatenate 'string
                         "/"
                         (string-downcase label))
      label))

(defun print-username (post-id)
  (execute-query-one user "SELECT UserName,
                                  Anonymous,
                                  PostIP
                           FROM posts
                           LEFT JOIN users ON posts.UserID = users.UserID
                           WHERE PostID = ?" (post-id)
    (cond ((user-authority-check-p "Moderator")
           (if (not (is-null (getf user :|username|)))
               (values (getf user :|username|)
                       (getf user :|postip|))
               (getf user :|postip|)))
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
  (execute-query-one thread "SELECT count(1) AS PostCount
                                FROM posts
                                WHERE ThreadID = ?"
      ((get-parameter "thread"))
    ;; mobile
    (let ((num-of-pages (ceiling (/ (getf thread :|postcount|)
                                    *posts-per-page*)))
          (page (parse-integer (if (get-parameter "page")
                                   (get-parameter "page")
                                   "1"))))
      (if (> num-of-pages 1)
          (progn
            (:div :class "visible-xs-inline rightbuttons"
                  (:a :class "btn btn-sm btn-default"
                      :href (format nil
                                    "view-thread?thread=~d&page=~d"
                                    (get-parameter "thread")
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
                                    "view-thread?thread=~d&page=~d"
                                    (get-parameter "thread")
                                    (+ page 1))
                      (">")))

            ;; non-mobile
            (let ((start-page (- page 1)))
              (:ul :class "pagination pagination-sm hidden-xs rightbuttons"
                   ;; if on page higher than 3, it'll look like
                   ;; < 1 ... (- 1 page) page (+ 1 page) ... num-of-pages >
                   (if (>= page 3)
                       (:li :class (if (= page start-page) "active")
                            (:a :href (format nil "view-thread?thread=~d&page=1"
                                              (get-parameter "thread"))
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
                              (:a :href (format nil "view-thread?thread=~d&page=~d"
                                                (get-parameter "thread")
                                                j)
                                  j))))
                   (if (< page (- num-of-pages 2))
                       (:li :class "disabled"
                            (:a :href "#" "...")))
                   (if (< page (- num-of-pages 1))
                       (:li :class (if (= page num-of-pages) "active")
                            (:a :href (format nil "view-thread?thread=~d&page=~d"
                                              (get-parameter "thread")
                                              num-of-pages)
                                num-of-pages))))))))))
