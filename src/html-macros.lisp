;; -*- coding:utf-8 -*-
(in-package :net.ignorama.web)

(defun site-symbol-to-fontawesome-class (site-symbol)
  (concatenate 'string
               "fa fa-"
               (string-downcase (symbol-name site-symbol))))

(defun site-symbol-to-name (site-symbol custom)
  (if custom
      custom
      (string-capitalize (symbol-name site-symbol))))

(defmacro sociallink (site url &optional custom-name)
  `(with-html (:a :class "header rightlink"
                  :target "_blank"
                  :href ,url
                  :title ,(site-symbol-to-name site custom-name)
                  (:span :class ,(site-symbol-to-fontawesome-class site)))))

(defmacro rightlink (label)
  `(with-html (:a :class "header rightlink"
                  :href ,(concatenate 'string
                                      "/"
                                      (string-downcase label))
                  ,label)))

(defun generate-rightlinks (links)
  (let ((result '(progn)))
    (dolist (link links)
      (setf result (append result (list `(rightlink ,link)))))
    result))

(defun generate-sociallinks (sites)
  (let ((result '(progn)))
    (dolist (site sites)
      (setf result (append result (list `(sociallink ,(first site)
                                                     ,(second site)
                                                     ,(third site))))))
    result))

(defun generate-dropdown-links (links)
  (let ((result '(progn)))
    (dolist (link links)
      (setf result (append result (list `(:div (:a :class "dropdown-item"
                                                   :href ,(concatenate 'string
                                                                       "/"
                                                                       (string-downcase link))
                                                   ,link))))))
    result))

(defun generate-dropdown-links-social (sites)
  (let ((result '(progn)))
    (dolist (site sites)
      (destructuring-bind (type url &optional name) site
        (setf result (append result (list `(:div (:a :class "dropdown-item"
                                                     :target "_blank"
                                                     :href ,url
                                                     ,(site-symbol-to-name type
                                                                           name))))))))
    result))

(defmacro rightlinks ()
  (let ((rightlinks '("Following" "Hidden" "Rules" "Bans" "Settings")))
    `(with-html
       (:div :class "header rightlinks"
             ;; non-mobile
             (:div :class "hidden-xs"
                   (:div
                    ,(generate-sociallinks *sociallinks*)
                    ,(generate-rightlinks rightlinks))

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

             ;; mobile
             (:div :class "visible-xs-inline"
                   (:div :class "btn-group mobile header rightlinks"
                         (:div :class "visible-xs-inline"
                               (:a :class "btn btn-default btn-sm dropdown-toggle"
                                   :data-toggle "dropdown"
                                   "Menu " (:span :class "caret"))
                               (:ul :class "dropdown-menu pull-right"
                                    ,(generate-dropdown-links rightlinks)
                                    ,(generate-dropdown-links-social *sociallinks*))))
                   (:br)
                   (:br)
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
                                 :href "/login" "Log in"))))))))

(defmacro print-username (post-id)
  `(execute-query-one user "SELECT UserName,
                                   Anonymous,
                                   PostIP
                            FROM posts
                            LEFT JOIN users ON posts.UserID = users.UserID
                            WHERE PostID = ?" (,post-id)
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

(defmacro print-link-to-thread (thread-id thread-title &key locked stickied)
  `(with-html
     (execute-query-one op
         "SELECT CONCAT(LEFT(PostContent, 200),
                 CASE
                      WHEN LENGTH(PostContent) > 200 THEN '...'
                      ELSE ''
                 END) AS PostContent
          FROM posts WHERE ThreadID = ?
          ORDER BY PostTime ASC" (,thread-id)
       (if ,stickied
           (progn (:span :class "thread-icon glyphicon glyphicon-bookmark")
                  (" ")))
       (if ,locked
           (progn (:span :class "thread-icon glyphicon glyphicon-lock")
                  (" ")))
       (:a :title (getf op :|postcontent|)
           :href
           (concatenate 'string
                        "view-thread?thread="
                        (write-to-string ,thread-id))
           ,thread-title))))

(defmacro threads-table (query)
  `(with-html (:table :class "table table-bordered fixed main-table"
                      (:tr :class "thread-row"
                           ;; non-mobile header
                           (:th :class "thread-row hidden-xs"
                                "Thread")
                           (:th :class "thread-row centered col-sm-2 hidden-xs"
                                "User")
                           (:th :class "thread-row centered col-md-1 col-sm-2 hidden-xs"
                                "Replies")
                           (:th :class "thread-row centered col-sm-3 col-md-2 hidden-xs"
                                "Board")
                           (:th :class "thread-row centered col-sm-2 hidden-xs"
                                "Latest Post")

                           ;; mobile header
                           (:th :class "thread-row visible-xs"
                                "Threads"
                                (:form :action "/"
                                       :method "get"
                                       :class "visible-xs searchform"
                                       (:div :class "mobile-search"
                                             (:input :class "searchbox mobile"
                                                     :name "search"
                                                     :type "textbox")
                                             (:input :type "hidden"
                                                     :name "f"
                                                     :value "search")
                                             (:button :style "margin-top: -3px; margin-right: 4px;"
                                                      :class "btn btn-default btn-sm"
                                                      :type "submit"
                                                      (:span :class "glyphicon glyphicon-search"))))))

                      (execute-query-loop thread ,query ()
                        (:tr
                         (:td :class "thread-name centered"
                              (print-link-to-thread (getf thread :|threadid|)
                                                    (getf thread :|threadsubject|)
                                                    :locked (getf thread :|locked|)
                                                    :stickied (getf thread :|stickied|))

                              ;; stuff for mobile
                              (:span :class "visible-xs-inline"

                                     (format nil " (~d)"
                                             (getf thread :|postcount|))
                                     (:div (format nil "Board: ~a"
                                                   (getf thread :|tag|)))
                                     (:div
                                      (:raw
                                       (format nil "Latest Post: ~a"
                                               (with-html-string
                                                 (:span :class "time"
                                                        (getf thread
                                                              :|latestposttime|))))))
                                     (:div
                                      (multiple-value-bind (name ip)
                                          (print-username
                                           (getf thread :|postid|))
                                        (:div name)
                                        (:div ip)))))

                         (:td :class "hidden-xs thread-row centered"
                              (multiple-value-bind (name ip)
                                  (print-username
                                   (getf thread :|postid|))
                                (:div name)
                                (:div ip)))
                         (:td :class "hidden-xs thread-row centered"
                              (getf thread :|postcount|))
                         (:td :class "hidden-xs thread-row centered"
                              (getf thread :|tag|))
                         (:td :class "hidden-xs time thread-row centered"
                              (getf thread
                                    :|latestposttime|)))))))

(defmacro tags-filter-dropdown ()
  `(with-html (:a :class "dropdown-toggle btn btn-default btn-sm"
                  :data-toggle "dropdown"
                  "Boards " (:b :class "caret"))
              (:ul :class "dropdown-menu dropdown-menu-form pull-right"
                   :role "menu"
                   (execute-query-loop tag (tags-query) ()
                     (:li (:label
                           (:input :type "checkbox"
                                   :name (getf tag :|tagid|))
                           (getf tag :|tagname|)))))))

(defmacro tags-dropdown ()
  `(with-html (:div :class "tagsdropdown" ("Tag: ")
                    (:select :id "tagdropdown"
                             :name "tag"
                             :required t
                             (:option :value ""
                                      "- Select a tag - ")
                             (execute-query-loop tag (tags-query) ()
                               (:option :value (getf tag :|tagid|)
                                        (getf tag :|tagname|)))))))

(defmacro index-buttons ()
  ;; dropdown only displays correctly when I wrap all the buttons in this div
  `(with-html (:div :class "dropdown"
                    (:button :class "btn btn-default btn-sm threads"
                             :onclick "window.location='new-thread'"
                             "New Thread")

                    (:form :class "rightbuttons"
                           :action "b/apply-tags"
                           :method "post"

                           ;; non-mobile buttons
                           (:input :type "button"
                                   :class "btn btn-default btn-sm hidden-xs threads reset-tags"
                                   :onclick "window.location='b/reset-tags'"
                                   :value "Reset Boards")
                           (:input :type "submit"
                                   :class "btn btn-default btn-sm hidden-xs threads"
                                   :value "Apply Boards")

                           ;; mobile buttons
                           (:input :type "button"
                                   :class "btn btn-default btn-sm visible-xs-inline threads reset"
                                   :onclick "window.location='b/reset-tags'"
                                   :value "Reset")
                           (:input :type "submit"
                                   :class "btn btn-default btn-sm visible-xs-inline threads"
                                   :value "Apply")

                           (tags-filter-dropdown))

                    (:form :action "/"
                           :method "get"
                           :class "hidden-xs searchform"
                           (:input :class "searchbox"
                                   :name "search"
                                   :type "textbox")
                           (:input :type "hidden"
                                   :name "f"
                                   :value "search")
                           (:button :style "margin-top: -3px; margin-right: 4px;"
                                    :class "btn btn-default btn-sm"
                                    :type "submit"
                                    (:span :class "glyphicon glyphicon-search"))))))

(defmacro posts-table (query &rest params)
  `(with-html (:table :class "table table-bordered fixed main-table"
                      (execute-query-loop post ,query (,@params)
                        (let ((post-id (getf post :|postid|))
                              (post-time (getf post :|posttime|)))
                          (:tr :id (concatenate 'string
                                                "post"
                                                (write-to-string
                                                 post-id))
                               (:td :class "col-sm-3 hidden-xs thread-row centered"
                                    (:div (multiple-value-bind (name ip)
                                              (print-username
                                               (getf post :|postid|))
                                            (:b (:div name))
                                            (:div ip))
                                          ;; (print-post-options post-id)
                                          )
                                    (:div :class "time" post-time))
                               (:td :class "col-sm-9 post-content centered"
                                    (:div :class "visible-xs mobile-post-info"
                                          (:span :class "time mobile-date"
                                                 post-time)
                                          (:span (multiple-value-bind (name ip)
                                                     (print-username
                                                      (getf post :|postid|))
                                                   (:div (:b name))
                                                   (:div ip))
                                                 ;; (print-post-options post-id)
                                                 )
                                          )
                                    (:div (format-post (getf post :|postcontent|))))))))))

(defmacro thread-buttons ()
  `(with-html
     (:button :class "btn btn-default btn-sm"
              :onclick (format nil "window.location='new-reply?thread=~d'"
                               (get-parameter "thread"))
              "Reply")
     (:button :class "btn btn-default btn-sm"
              :onclick "window.location='/'"
              "Main Page")

     ;; pagination
     (execute-query-one thread "SELECT count(1) AS PostCount
                                FROM posts
                                WHERE ThreadID = ?"
         ((get-parameter "thread"))
       (let* ((num-of-pages (ceiling (/ (getf thread :|postcount|)
                                        *posts-per-page*)))
              (page (parse-integer (if (get-parameter "page")
                                       (get-parameter "page")
                                       "1")))
              (start-page (- page 1)))
         (:div :class "visible-xs-inline rightbuttons"
               (:a :class "btn btn-sm btn-default"
                   :href (format nil
                                 "view-thread?thread=~d&page=~d"
                                 (get-parameter "thread")
                                 (- page 1))
                   ("<"))
               (:select :name "Page"
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
                   (">")))))))

(defmacro thread-dropdown ()
  `(with-html
     (:span :class "btn-group rightbuttons"
            (:a :class "btn btn-default btn-sm dropdown-toggle"
                :data-toggle "dropdown"
                :href "#"
                (:span :class "caret"))
            (:ul :class "dropdown-menu pull-right"
                 "TODO - add stuff here"))))

(defmacro image-upload-form ()
  `(with-html
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
                    :value "none"))))

(defmacro reply-buttons ()
  `(with-html
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
                                     (get-parameter "thread"))))))
