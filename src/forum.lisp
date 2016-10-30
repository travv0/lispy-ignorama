;; -*- coding:utf-8 -*-
(in-package :net.ignorama.web)

;;; site setup
(defvar *sessions* (make-hash-table :test 'equal))
(defvar *conn* nil)

(defparameter *session-id-cookie-name* "sessionid")

(defparameter *threads-query* "SELECT * FROM IndexThreads")

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
      (:script :src "/static/script.js"))))

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
        (rightlinks))))))

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
             (:h2 ,title)
             ,@body)))))

;;; this macro creates and publishes page <name> at https://your-site.com/<name>
(defmacro publish-page (name &body body)
  `(hunchentoot:define-easy-handler (,name
                                     :uri ,(string-downcase
                                            (if (equal 'index name)
                                                "/"
                                                (concatenate 'string "/" (symbol-name name))))) (params)
     (setf (hunchentoot:content-type*) "text/html")
     (let ((*conn* *conn*))
       (with-db *conn*
         ,@body))))

;;; web pages beyond here
(publish-page index
  (multiple-value-bind (title condition)
      (index-params-by-type (get-parameter "f"))
    (standard-page
        (:title title)
      (:body (index-buttons)
             (threads-table (concatenate 'string
                                         *threads-query*
                                         condition
                                         (format nil " LIMIT ~d" *index-row-limit*)))
             (:div :class "fake-copyright"
                   (:raw *fake-copyright*))))))

(publish-page view-thread
  ;; if passed "post" parameter, redirect to appropriate thread and highlight post
  (if (get-parameter "post")
      (execute-query-one thread
          "SELECT ThreadID FROM posts WHERE PostID = ?"
          ((get-parameter "post"))
        (redirect (format nil
                          "/view-thread?thread=~d&highlight=~d"
                          (getf thread :|threadid|)
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
                             ORDER BY PostTime"
                            (get-parameter "thread"))
               (thread-buttons)
               (:div :class "fake-copyright"
                     (:raw *fake-copyright*))))
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

(publish-page new-thread
  (standard-page
      (:title "New Thread")))
