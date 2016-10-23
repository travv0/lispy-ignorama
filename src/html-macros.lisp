;; -*- coding:utf-8 -*-
(in-package :net.ignorama.web)

(defmacro sociallink (site url)
  `(with-html (:a :class "header rightlink"
                  :target "_blank"
                  :href ,url
                  (:span :class ,(concatenate 'string
                                              "fa fa-"
                                              (string-downcase (symbol-name site)))))))

(defmacro rightlink (label)
  `(with-html (:a :class "header rightlink"
                  :href ,(string-downcase label)
                  ,label)))

(defmacro rightlinks ()
  `(with-html ( sociallinks)
              (rightlink "Following")
              (rightlink "Rules")
              (rightlink "Bans")
              (rightlink "Settings")

              (:br)

              (:div :class "header loginlinks"
                    (:a :class "header rightlink"
                        :href "/signup" "Sign up")
                    ("/")
                    (:a :class "header rightlink"
                        :href "/login" "Log in"))))

(defmacro print-username (name)
  ` (if (and ,name
             (not *force-anonymity*))
        ,name
        *nameless-name*))

(defmacro print-link-to-thread (thread-id thread-title &key locked stickied)
  `(with-html (let* ((q (prepare *db*
                                 "SELECT PostContent FROM `posts` WHERE ThreadID = ?"))
                     (result (execute q ,thread-id))
                     (op (fetch result)))
                (if (= ,stickied 1)
                    (progn (:span :class "thread-icon glyphicon glyphicon-bookmark")
                           ("&nbsp;")))
                (if (= ,locked 1)
                    (progn (:span :class "thread-icon glyphicon glyphicon-lock")
                           ("&nbsp;")))
                (:a :title (getf op :|PostContent|)
                    :href
                    (concatenate 'string
                                 "viewthread?thread="
                                 (write-to-string ,thread-id))
                    ,thread-title))))

(defmacro threads-table (query)
  `(with-html (:table :class "table table-bordered fixed main-table"
                      (:tr :class "thread-row"
                           (:th :class "thread-row"
                                "Thread")
                           (:th :class "thread-row centered col-sm-2 hidden-xs"
                                "User")
                           (:th :class "thread-row centered col-md-1 col-sm-2 hidden-xs"
                                "Replies")
                           (:th :class "thread-row centered col-sm-3 col-md-2 hidden-xs"
                                "Tag(s)")
                           (:th :class "thread-row centered col-sm-2 hidden-xs"
                                "Latest Post")
                           (let* ((q (prepare *db* ,query))
                                  (result (execute q)))
                             (loop for thread = (fetch result)
                                   while thread do
                                     (:tr
                                      (:td :class "thread-name centered"
                                           (print-link-to-thread (getf thread :|ThreadID|)
                                                                 (getf thread :|ThreadSubject|)
                                                                 :locked (getf thread :|Locked|)
                                                                 :stickied (getf thread :|Stickied|)))
                                      (:td :class "thread-row centered"
                                           (print-username (getf thread :|ModName|)))
                                      (:td :class "thread-row centered"
                                           (getf thread :|PostCount|))
                                      (:td :class "thread-row centered"
                                           (getf thread :|Tag|))
                                      (:td :class "time thread-row centered"
                                           (universal-to-unix
                                            (getf thread
                                                  :|LatestPostTime|))))))))))

(defmacro tags-dropdown ()
  `(with-html (:a :class "dropdown-toggle btn btn-default btn-sm"
                  :data-toggle "dropdown"
                  "Tags" (:b :class "caret")
                  (:ul :class "dropdown-menu dropdown-menu-form pull-right"
                       :role "menu"
                       (let* ((q (prepare *db*
                                          "SELECT TagID, TagName FROM `tags`"))
                              (result (execute q)))
                         (loop for tag = (fetch result)
                               while tag do
                                 (:li (:label
                                       (:input :type "checkbox"
                                               :name (getf tag :|TagID|))
                                       (getf tag :|TagName|)))))))))

(defmacro index-buttons ()
  ;; dropdown only displays correctly when I wrap all the buttons in this div
  `(with-html (:div :class "dropdown"
                    (:button :class "btn btn-default btn-sm threads"
                             :onclick "window.location='newthread'"
                             "New Thread")

                    (:form :class "rightbuttons"
                           :action "b/submitfilter"
                           :method "post"
                           (:input :type "button"
                                   :class "btn btn-default btn-sm threads"
                                   :onclick "window.location='hiddenthreads'"
                                   :value "Hidden Threads")
                           (:input :type "button"
                                   :class "btn btn-default btn-sm threads"
                                   :onclick "window.location='b/resettags'"
                                   :value "Reset Tags")
                           (:input :type "submit"
                                   :class "btn btn-default btn-sm threads"
                                   :value "Apply Tags")

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
                      (let* ((q (prepare *db* ,query))
                             (result (execute q ,@params)))
                        (loop for post = (fetch result)
                              while post do
                                (:tr :id (concatenate 'string
                                                      "post"
                                                      (write-to-string
                                                       (getf post :|PostID|)))
                                     (:td :class "col-sm-3 hidden-xs thread-row centered"
                                          (print-username (getf post :|ModName|))
                                          (:br)
                                          (:br)
                                          (:span :class "time" (getf post :|PostTime|)))
                                     (:td :class "col-sm-9 post-content centered"
                                          (getf post :|PostContent|))))))))

(defmacro thread-buttons ()
  ;; dropdown only displays correctly when I wrap all the buttons in this div
  `(with-html
     (:button :class "btn btn-default btn-sm"
              :onclick (format nil "window.location='newpost?thread=~d'"
                               (query-param "thread"))
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
