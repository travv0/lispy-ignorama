;; -*- coding:utf-8 -*-
(in-package :net.ignorama.web)

(defvar *app* (make-instance '<app>))
(setf *prologue* "<!DOCTYPE html>")

;;; site setup
(let* ((q (prepare *db* "SET NAMES utf8")))
  (execute q))

(defvar *sessions* (make-hash-table))

(defparameter *threads-query* "SELECT * FROM IndexThreads LIMIT 0,200")

;; (defparameter *resource-dirs* '("js/" "css/" "img/"))
;; ;;; publish css, js, img, etc.
;; (loop for dir in *resource-dirs*
;;       do (publish-directory :prefix (concatenate 'string "/" dir)
;;                             :destination dir))

;;; stuff to go in the <head> tags (minus <title>)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *head*
    `((:meta :charset="UTF-8")
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
      (:script :src "/static/js.js"))
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
		   (htm (:b :class "hidden-sm header slogan"
			    (str (random-elt *slogans*))))))
        (:span :class "visible-xs-inline"
	       (:a :href "/index"
		   (:img :class "header logo small" :src "/static/ignoramasmall.png")))

        ;; right links
        (:div :class "hidden-xs header rightlinks"
	      (rightlinks)))))))

;;; The basic format that every viewable page will follow.
(defmacro standard-page ((&key title) &body body)
  `(htm
    (:html
      (:head (:title ,(concatenate 'string
				   title (if (equal title "")
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
  `(setf (ningle:route *app* (concatenate 'string "/" ,name))
	 (with-html-output-to-string (*standard-output* nil :prologue t)
	   (htm ,@body))))

;;; this is for pages that don't show anything, only run logic then redirect
(defmacro publish-script (name &body body)
  `(publish :path ,(string-downcase
                    (concatenate 'string "/" (symbol-name name)))
            :content-type "text/html"
            :function
            #'(lambda (request entity)
                (macrolet ((query-param (param)
                             `(cdr (assoc ,param (request-query request) :test #'equal))))
                  (with-http-response (request entity :response *response-found*)
                    ,@body
                    (with-http-body (request entity)))))))

;;; web pages beyond here
(publish-page ""
  (standard-page
      (:title "")
    (:body
     (indexbuttons)
     (indextable *threads-query*)
     ;; (echo *fake-copyright*)
     )))

;; (publish-page "viewthread"
;;   (:standard-page
;;    (:title (concatenate 'string (get-thread-title (query-param "thread"))))
;;    (:body
;;     (:print (get-thread-title (query-param "thread"))))))

(publish-page "login"
  (standard-page
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
		    :onclick "window.location='../'")))))

(publish-page "unicode-test"
  (standard-page
      (:title "Unicode Test")
    (:body
     (:p "これは機械翻訳です。"))))

;; (publish-script b/login
;;   (let ((user-status nil))
;;     ;; if no status, user doesn't exist
;;     (if (setf user-status (get-user-status (query-param "username")))
;;         (let ((sessionid nil))
;;           ;; find an id not in use and set it to sessionid
;;           (loop while (gethash (setf sessionid (intern (write-to-string (make-v4-uuid)))) *sessions*))

;;           ;; TODO: make it easier to query session variables
;;           (setf (gethash sessionid *sessions*) (make-hash-table))
;;           (setf (gethash 'username (gethash sessionid *sessions*)) (query-param "username"))
;;           (setf (gethash 'userstatus (gethash sessionid *sessions*)) user-status)
;;           (setf (gethash 'userlastactive (gethash sessionid *sessions*)) (get-universal-time))

;;           (set-cookie-header request :name "sessionid" :value (symbol-name sessionid))
;;           (setf (reply-header-slot-value request :location) "/"))
;;         (setf (reply-header-slot-value request :location) "/loginfailed"))))

;;(clackup *app*)
