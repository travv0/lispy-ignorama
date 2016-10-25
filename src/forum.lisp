;; -*- coding:utf-8 -*-
(in-package :net.ignorama.web)

(defvar *app* (make-instance '<app>))

(load "js/script.lisp")

;;; site setup
(let* ((q (prepare *db* "SET NAMES utf8")))
  (execute q))

(defvar *sessions* (make-hash-table))

(defparameter *threads-query* "SELECT * FROM IndexThreads LIMIT 0,200")

;;; stuff to go in the <head> tags (minus <title>)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *head*
    `((:meta :charset "UTF-8")
      (:meta :name "viewport"
             :content "width=device-width, initial-scale=1, maximum-scale=1")

      (:link :rel "shortcut icon"
             :type "image/png"
             :href "/static/favicon.png")

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
      (:script :src "/js/script.js"))
    "Content that goes in the header of every page."))

;;; page skeleton
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *header*
    '((:div :class "header banner"
       (:div :class "header text"

        ;; logo and slogans
        (:span :class "hidden-xs"
               (:a :href "/"
                   (:img :class "header logo" :src "/static/ignorama.png"))
               (if *slogans*
                   (:b :class "hidden-sm header slogan"
                       (:raw (random-elt *slogans*)))))
        (:span :class "visible-xs-inline"
               (:a :href "/index"
                   (:img :class "header logo small" :src "/static/ignoramasmall.png")))

        ;; right links
        (:div :class "hidden-xs header rightlinks"
              (rightlinks)))))))

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
             ,@body
             )))))

;;; this macro creates and publishes page <name> at https://your-site.com/<name>
(defmacro publish-page (name (&body body) &key (method :GET))
  `(progn (defun ,name (&optional params)
            (macrolet ((query-param (param)
                         `(cdr (assoc ,param params :test #'string=))))
              ,@body))
          (setf (ningle:route *app*
                              ,(string-downcase
                                (concatenate 'string "/" (symbol-name name)))
                              :method ,method)
                #',name)))

;;; web pages beyond here
(publish-page index
  ((standard-page
       (:title "")
     (:body (index-buttons)
            (threads-table *threads-query*)
            (:div :class "fake-copyright"
                  (:raw *fake-copyright*))))))

(setf (ningle:route *app* "/") #'index)

(publish-page view-thread
  ((standard-page
       (:title (get-thread-title (query-param "thread")))
     (:body (thread-buttons)
            (thread-dropdown)
            (posts-table "SELECT *
                                      FROM posts
                                      WHERE ThreadID = ?"
                         (query-param "thread"))
            (thread-buttons)
            (:div :class "fake-copyright"
                  (:raw *fake-copyright*))))))

(publish-page login
  ((standard-page
       (:title "Login")
     (:body
      (:form :method "POST" :action "/b/login"
             (:input :name "username" :type "text")
             (:br)
             (:input :name "password" :type "password")
             (:br)
             (:input :name "Submit1" :type "submit" :value "Submit")
             (:input :type "button"
                     :value "Main Page"
                     :onclick "window.location='../'"))))))

(publish-page new-reply
  ((cond ((thread-locked-p (query-param "thread"))
          '(302 (:location "/locked")))
         (t (standard-page
                (:title "New Reply")
              (:body
               (:form :action (format nil "b/submitpost?thread=~d"
                                      (query-param "thread"))
                      :method "post"
                      (:textarea :id "postfield"
                                 :name "Body"
                                 :rows "7"
                                 :class "col-xs-12"
                                 :required t)
                      ;; (reply-buttons)
                      (image-upload-form))))))))

(publish-page locked
  ((standard-page
       (:title "Thread Locked")
     (:body (:p "And it's surely better that way.")
            (:p (:button :class "btn btn-default btn-sm"
                         :onclick "window.location='./'"
                         "Main Page"))))))

(publish-page new-thread
  ((standard-page
       (:title "New Thread"))))

(publish-page b/login
  ((let ((user-status nil))
     ;; if no status, user doesn't exist
     (if (setf user-status (get-user-status (query-param "username")))
         (let ((session-id nil))
           ;; find an id not in use and set it to session-id
           (loop while (gethash
                        (setf session-id (intern (write-to-string (make-v4-uuid))))
                        *sessions*))

           ;; TODO: make it easier to query session variables
           (setf (gethash session-id *sessions*) (make-hash-table))
           (setf (gethash 'username (gethash session-id *sessions*)) (query-param "username"))
           (setf (gethash 'userstatus (gethash session-id *sessions*)) user-status)
           (setf (gethash 'userlastactive (gethash session-id *sessions*)) (get-universal-time))

           `(302 (:location "/"
                            :set-cookie ,(set-cookie "sessionid" session-id))))
         '(302 (:location "/loginfailed")))))
  :method :POST)
