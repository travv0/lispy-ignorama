;; -*- coding:utf-8 -*-
(in-package :net.ignorama.web)

(define-html-macro :sociallink (site url)
  `(:a :class "header rightlink"
       :target "_blank"
       :href ,url
       (:span :class ,(concatenate 'string
                                   "fa fa-"
                                   (string-downcase (symbol-name site))))))

(define-html-macro :rightlink (label)
  `(:a :class "header rightlink"
       :href ,(string-downcase label)
       ,label))

(define-html-macro :rightlinks ()
  `(html (:sociallinks)

         (:rightlink "Following")
         (:rightlink "Rules")
         (:rightlink "Bans")
         (:rightlink "Settings")

         (:br)

         (:div :class "header loginlinks"
               (:a :class "header rightlink"
                   :href "/signup" "Sign up")
               (echo "&nbsp;/&nbsp;")
               (:a :class "header rightlink"
                   :href "/login" "Log in"))))

(define-html-macro :print-username (name)
  `(:print (if (and ,name
                    (not *force-anonymity*))
               ,name
               *nameless-name*)))

(define-html-macro :print-link-to-thread (thread-id thread-title &key locked stickied)
  `(let* ((q (prepare *db*
                      "SELECT PostContent FROM `posts` WHERE ThreadID = ?"))
          (result (execute q ,thread-id))
          (op (fetch result)))
     (if (= ,stickied 1)
         (html (:span :class "thread-icon glyphicon glyphicon-bookmark")
               (echo "&nbsp;")))
     (if (= ,locked 1)
         (html (:span :class "thread-icon glyphicon glyphicon-lock")
               (echo "&nbsp;")))
     (html (:a :title (:print (getf op :|PostContent|))
               :href (:print
                      (concatenate 'string
                                   "viewthread?thread="
                                   (write-to-string ,thread-id)))
               (:print ,thread-title)))))

(define-html-macro :indextable (query)
  `(:table :class "table table-bordered fixed"
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
                       (html
                         (:tr
                          (:td :class "thread-name centered"
                               (:print-link-to-thread (getf thread :|ThreadID|)
                                                      (getf thread :|ThreadSubject|)
                                                      :locked (getf thread :|Locked|)
                                                      :stickied (getf thread :|Stickied|)))
                          (:td :class "thread-row centered"
                               (:print-username (getf thread :|ModName|)))
                          (:td :class "thread-row centered"
                               (:print (getf thread :|PostCount|)))
                          (:td :class "thread-row centered"
                               (:print (getf thread :|Tag|)))
                          (:td :class "time thread-row centered"
                               (:print (universal-to-unix
                                        (getf thread
                                              :|LatestPostTime|)))))))))))

(define-html-macro :tagsdropdown ()
  `(html (:a :class "dropdown-toggle btn btn-default btn-sm"
             :data-toggle "dropdown"
             "Tags" (:b :class "caret"))
         (:ul :class "dropdown-menu dropdown-menu-form pull-right"
              :role "menu"
              (let* ((q (prepare *db*
                                 "SELECT TagID, TagName FROM `tags`"))
                     (result (execute q)))
                (loop for tag = (fetch result)
                   while tag do
                     (html
                       (:li (:label
                             (:input :type "checkbox"
                                     :name (:print (getf tag :|TagID|))
                                     (:print (getf tag :|TagName|)))))))))))

(define-html-macro :indexbuttons ()
  ;; dropdown only displays correctly when I wrap all the buttons in this div
  `(:div :class "dropdown"
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

                (:tagsdropdown))

         (:form :action "/"
                :method "get"
                :class "hidden-xs searchform"
                (:input :class "searchbox"
                        :name "search"
                        :type "textbox")
                (:button :style "margin-top: -3px; margin-right: 4px;"
                         :class "btn btn-default btn-sm"
                         :type "submit"
                         (:span :class "glyphicon glyphicon-search")))))
