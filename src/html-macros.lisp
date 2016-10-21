;; -*- coding:utf-8 -*-
(in-package :net.ignorama.web)

(defmacro sociallink (site url)
  `(htm (:a :class "header rightlink"
	    :target "_blank"
	    :href ,url
	    (:span :class ,(concatenate 'string
					"fa fa-"
					(string-downcase (symbol-name site)))))))

(defmacro rightlink (label)
  `(htm (:a :class "header rightlink"
	    :href ,(string-downcase label)
	    ,label)))

(defmacro rightlinks ()
  `(htm (sociallinks)

         (rightlink "Following")
         (rightlink "Rules")
         (rightlink "Bans")
         (rightlink "Settings")

         (:br)

         (:div :class "header loginlinks"
               (:a :class "header rightlink"
                   :href "/signup" "Sign up")
               (str "&nbsp;/&nbsp;")
               (:a :class "header rightlink"
                   :href "/login" "Log in"))))

(defmacro print-username (name)
  ` (if (and ,name
                    (not *force-anonymity*))
               ,name
               *nameless-name*))

(defmacro print-link-to-thread (thread-id thread-title &key locked stickied)
  `(let* ((q (prepare *db*
                      "SELECT PostContent FROM `posts` WHERE ThreadID = ?"))
          (result (execute q ,thread-id))
          (op (fetch result)))
     (if (= ,stickied 1)
         (htm (:span :class "thread-icon glyphicon glyphicon-bookmark")
               (str "&nbsp;")))
     (if (= ,locked 1)
         (htm (:span :class "thread-icon glyphicon glyphicon-lock")
               (str "&nbsp;")))
     (htm (:a :title  (getf op :|PostContent|))
               :href
                      (concatenate 'string
                                   "viewthread?thread="
                                   (write-to-string ,thread-id)))
                ,thread-title))

(defmacro indextable (query)
  `(htm (:table :class "table table-bordered fixed"
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
			    (htm
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
						  :|LatestPostTime|)))))))))))

(defmacro tagsdropdown ()
  `(htm (:a :class "dropdown-toggle btn btn-default btn-sm"
             :data-toggle "dropdown"
             "Tags" (:b :class "caret"))
         (:ul :class "dropdown-menu dropdown-menu-form pull-right"
              :role "menu"
              (let* ((q (prepare *db*
                                 "SELECT TagID, TagName FROM `tags`"))
                     (result (execute q)))
                (loop for tag = (fetch result)
                   while tag do
                     (htm
                       (:li (:label
                             (:input :type "checkbox"
                                     :name  (getf tag :|TagID|))
                                      (getf tag :|TagName|)))))))))

(defmacro indexbuttons ()
  ;; dropdown only displays correctly when I wrap all the buttons in this div
  `(htm (:div :class "dropdown"
	      (:button :class "btn btn-default btn-sm threads"
		       :onclick (escape-string "window.location='newthread'")
		       "New Thread")

	      (:form :class "rightbuttons"
		     :action "b/submitfilter"
		     :method "post"
		     (:input :type "button"
			     :class "btn btn-default btn-sm threads"
			     :onclick (escape-string "window.location='hiddenthreads'")
			     :value "Hidden Threads")
		     (:input :type "button"
			     :class "btn btn-default btn-sm threads"
			     :onclick (escape-string "window.location='b/resettags'")
			     :value "Reset Tags")
		     (:input :type "submit"
			     :class (escape-string "btn btn-default btn-sm threads")
			     :value "Apply Tags")

		     (tagsdropdown))

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
