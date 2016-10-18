;; -*- coding:utf-8 -*-
(in-package :net.ignorama.web)

;;; site setup
(setf *default-aserve-external-format* :utf-8)

(let* ((q (prepare *db* "SET NAMES utf8")))
  (execute q))

(defparameter *threads-query* "SELECT * FROM IndexThreads LIMIT 0,200")

(defparameter *resource-dirs* '("js/" "css/" "img/"))
;;; publish css, js, img, etc.
(loop for dir in *resource-dirs*
   do (publish-directory :prefix (concatenate 'string "/" dir)
			 :destination dir))

;;; stuff to go in the <head> tags (minus <title>)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *head*
    `((:meta :charset="UTF-8")
      (:meta :name "viewport"
	     :content "width=device-width, initial-scale=1, maximum-scale=1")

      (:link :rel "shortcut icon"
	     :type "image/png"
	     :href "/img/favicon.png")

      (:link :rel "stylesheet"
	     :href "//maxcdn.bootstrapcdn.com/bootstrap/3.2.0/css/bootstrap.min.css")
      (:link :rel "stylesheet"
	     :href "//maxcdn.bootstrapcdn.com/bootstrap/3.2.0/css/bootstrap-theme.min.css")
      (:link :rel "stylesheet"
	     :href "//maxcdn.bootstrapcdn.com/font-awesome/4.2.0/css/font-awesome.min.css")
      (:link :rel "stylesheet"
	     :href "/css/style.css")

      (:script :src "//code.jquery.com/jquery-1.11.0.min.js")
      (:script :src "//maxcdn.bootstrapcdn.com/bootstrap/3.2.0/js/bootstrap.min.js")
      (:script :src "/js/js.js"))
    "Content that goes in the header of every page."))

;;; page skeleton
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *header*
    '((:div :class "header banner"
       (:div :class "header text"

	;; logo and slogans
	(:span :class "hidden-xs"
	       (:a :href "/"
		   (:img :class "header logo" :src "/img/ignorama.png"))
	       (if *slogans*
		   (html (:b :class "hidden-sm header slogan"
			     (echo (random-elt *slogans*))))))
	(:span :class "visible-xs-inline"
	       (:a :href "/index"
		   (:img :class "header logo small" :src "/img/ignoramasmall.png")))

	;; right links
	(:div :class "hidden-xs header rightlinks"
	      (:rightlinks)))))))

;;; The basic format that every viewable page will follow.
(define-html-macro :standard-page ((&key title) &body body)
  `(progn
     (html
       (:doctype "html" "PUBLIC" "-//W3C//DTD XHTML 1.0 Strict//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd"))
     (html
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
		,@body))))))

;;; this function creates and publishes page <name> at https://your-site.com/<name>
(defmacro publish-page (name &body body)
  `(progn
     (defun ,name (request entity)
       (with-http-response (request entity :content-type "text/html")
	 (with-http-body (request entity :external-format :utf8-base)
	   (with-html-output ((request-reply-stream request))
	     (html
	       ,@body)))))
     (publish :path ,(string-downcase
		      (concatenate 'string "/" (symbol-name name)))
	      :function ',name)))

;; publish index to /
(publish :path "/" :function 'index)

;;; web pages beyond here
(publish-page index
  (:standard-page
   (:title "")
   (:body
    (:indexbuttons)
    (:indextable *threads-query*)
    (echo *fake-copyright*))))

(publish-page login
  (:standard-page
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

(publish-page unicode-test
  (:standard-page
   (:title "Unicode Test")
   (:body
    (:p "これは機械翻訳です。"))))

(publish :path "/b/login"
	 :content-type "text/html"
	 :function
	 #'(lambda (request entity)
	     (with-http-response (request entity :response *response-found*)
	       (set-cookie-header request :name "TestCookie" :value "TestValue")
	       (setf (reply-header-slot-value request :location) "http://localhost:2001/")
	       (with-http-body (request entity)))))
