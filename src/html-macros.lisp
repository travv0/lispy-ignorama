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
      (setf result (append result (list `(progn (:a :class "dropdown-item"
                                                    :href ,(concatenate 'string
                                                                        "/"
                                                                        (string-downcase link))
                                                    ,link)
                                                (:br))))))
    result))

(defun generate-dropdown-links-social (sites)
  (let ((result '(progn)))
    (dolist (site sites)
      (destructuring-bind (type url &optional name) site
        (setf result (append result (list `(progn (:a :class "dropdown-item"
                                                      :href ,url
                                                      ,(site-symbol-to-name type
                                                                            name))
                                                  (:br)))))))
    result))

(defmacro rightlinks ()
  (let ((rightlinks '("Following" "Hidden" "Rules" "Bans" "Settings")))
    `(with-html
       ;; non-mobile
       (:div :class "hidden-xs"
             ,(generate-sociallinks *sociallinks*)
             ,(generate-rightlinks rightlinks)

             (:br)

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
       (:div :class "btn-group mobile"
             (:div :class "visible-xs-inline"
                   (:a :class "btn btn-default btn-sm dropdown-toggle"
                       :data-toggle "dropdown"
                       "Menu" (:span :class "caret"))
                   (:ul :class "dropdown-menu pull-right"
                        ,(generate-dropdown-links rightlinks)
                        ,(generate-dropdown-links-social *sociallinks*)))))))

(defmacro print-username (post-id)
  `(execute-query-one user "SELECT UserName,
                                   Anonymous,
                                   PostIP
                            FROM posts
                            LEFT JOIN users ON posts.UserID = users.UserID
                            WHERE PostID = ?" (,post-id)
     (cond ((user-authority-check-p "Moderator") (if (is-null (getf user :|username|))
                                                     (getf user :|postip|)
                                                     (getf user :|username|)))
           (t (if (and (or (and (not *force-anonymity*)
                                (not (getf user :|anonymous|)))
                           (not *allow-anonymity*))
                       (not (is-null (getf user :|username|))))
                  (getf user :|username|)
                  *nameless-name*)))))

(defun is-null (x)
  (equal x :null))

(defmacro print-link-to-thread (thread-id thread-title &key locked stickied)
  `(with-html
     (execute-query-one op
         "SELECT CONCAT(LEFT(PostContent, 200),
                 CASE
                      WHEN LENGTH(PostContent) > 200 THEN '...'
                      ELSE ''
                 END) AS PostContent
          FROM posts WHERE ThreadID = ?" (,thread-id)
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
                                "Tag(s)")
                           (:th :class "thread-row centered col-sm-2 hidden-xs"
                                "Latest Post")

                           ;; mobile header
                           (:th :class "thread-row visible-xs"
                                "Threads")

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
                                          (:br)
                                          (format nil "Tags: ~a"
                                                  (getf thread :|tag|))
                                          (:br)
                                          (:raw
                                           (format nil "Latest Post: ~a"
                                                   (with-html-string
                                                     (:span :class "time"
                                                            (getf thread
                                                                  :|latestposttime|)))))
                                          (:br)
                                          (print-username
                                           (getf thread :|postid|))))

                              (:td :class "hidden-xs thread-row centered"
                                   (print-username (getf thread :|postid|)))
                              (:td :class "hidden-xs thread-row centered"
                                   (getf thread :|postcount|))
                              (:td :class "hidden-xs thread-row centered"
                                   (getf thread :|tag|))
                              (:td :class "hidden-xs time thread-row centered"
                                   (getf thread
                                         :|latestposttime|))))))))

(defmacro tags-dropdown ()
  `(with-html (:a :class "dropdown-toggle btn btn-default btn-sm"
                  :data-toggle "dropdown"
                  "Tags " (:b :class "caret"))
              (:ul :class "dropdown-menu dropdown-menu-form pull-right"
                   :role "menu"
                   (execute-query-loop tag "SELECT TagID, TagName FROM tags" ()
                     (:li (:label
                           (:input :type "checkbox"
                                   :name (getf tag :|tagid|))
                           (getf tag :|tagname|)))))))

(defmacro index-buttons ()
  ;; dropdown only displays correctly when I wrap all the buttons in this div
  `(with-html (:div :class "dropdown"
                    (:button :class "btn btn-default btn-sm threads"
                             :onclick "window.location='new-thread'"
                             "New Thread")

                    (:form :class "rightbuttons"
                           :action "b/submitfilter"
                           :method "post"

                           ;; non-mobile buttons
                           (:input :type "button"
                                   :class "btn btn-default btn-sm hidden-xs threads reset-tags"
                                   :onclick "window.location='b/resettags'"
                                   :value "Reset Tags")
                           (:input :type "submit"
                                   :class "btn btn-default btn-sm hidden-xs threads"
                                   :value "Apply Tags")

                           ;; mobile buttons
                           (:input :type "button"
                                   :class "btn btn-default btn-sm visible-xs-inline threads reset"
                                   :onclick "window.location='b/resettags'"
                                   :value "Reset")
                           (:input :type "submit"
                                   :class "btn btn-default btn-sm visible-xs-inline threads"
                                   :value "Apply")

                           (tags-dropdown))

                    (:form :action "/"
                           :method "get"
                           :class "hidden-xs searchform"
                           (:input :class "searchbox"
                                   :name "search"
                                   :type "textbox")
                           (:button :style "margin-top: -3px; margin-right: 4px;"
                                    :class "btn btn-default btn-sm"
                                    :type "submit"
                                    (:span :class "glyphicon glyphicon-search"))))))

(defmacro posts-table (query &rest params)
  `(with-html (:table :class "table table-bordered fixed main-table"
                      (execute-query-loop post ,query (,@params)
                        (:tr :id (concatenate 'string
                                              "post"
                                              (write-to-string
                                               (getf post :|postid|)))
                             (:td :class "col-sm-3 hidden-xs thread-row centered"
                                  (print-username (getf post :|postid|))
                                  (:br)
                                  (:br)
                                  (:span :class "time" (getf post :|posttime|)))
                             (:td :class "col-sm-9 post-content centered"
                                  (getf post :|postcontent|)))))))

(defmacro thread-buttons ()
  `(with-html
     (:button :class "btn btn-default btn-sm"
              :onclick (format nil "window.location='new-reply?thread=~d'"
                               (get-parameter "thread"))
              "Reply")
     (:button :class "btn btn-default btn-sm"
              :onclick "window.location='/'"
              "Main Page")))

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
