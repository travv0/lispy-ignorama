;; -*- coding:utf-8 -*-

(in-package :net.ignorama.web)

(setf *default-aserve-external-format* :utf-8)

;; connect to database (FIXME: don't use root lol)
(defvar *db* (connect :mysql
		      :username "root"
		      :password "password"
		      :database-name "tssund93_forums"))
(let* ((q (prepare *db* "SET NAMES utf8")))
  (execute q))

;; utility functions and macros
(defun random-elt (choices)
  "Choose an element from a list at random."
  (elt choices (random (length choices))))

(defmacro echo (html)
  `(format (request-reply-stream request) "~a" ,html))

;; site setup
(defparameter *site-name* "Ignorama")
(defparameter *slogans*
  '("I should've made it blue"
    "<a target='_blank' href='http://ignorama.net/settings.php'>Hide You Some Random for Great Good!</a>"
    "<a target='_blank' href='http://firefox.com/'>I don't test this site on IE or Safari</a>"
    "Ignorance for the masses"
    "Made with MS Paint"
    "The six million dollar forum"
    "We live so you don't have to"
    "No boys allowed"
    "The hippest MRA hangout on the 'net"
    "A safe place to have fun"
    "It's called an irony mark, look it up"
    "Check your privilege"
    "Leave your identity at the door"
    "Partnered with the NSA"
    "Fully Web 2.0 Compliant"
    "Fun for all ages"
    "Endorsed by NEETs everywhere"
    "Quality posts served fresh daily"
    "LGBTQIA allies"
    "<a target='_blank' href='http://ignorama.net/newthread.php'>Easier than logging into Facebook</a>"
    "Don't forget to Like and Subscribe!"
    "It is now safe to turn on your brain"
    "It is now safe to turn off your brain"
    "It's not offensive if you're being ironic"
    "Cishets not welcome"
    "Where best friends are made"
    "Where gay lovers are made"
    "<a target='_blank' href='http://ignorama.net/banlog.php'>All moderators approved by Hitler</a>"
    "Home of the Troglodyte"
    "Trigger Warning: Opinions"
    "Now gluten free"
    "<a target='_blank' href='http://ignorama.net/rules.php'>Read the goddamn rules</a>"
    "Your hobbies are stupid"))

(defparameter *fake-copyright* "
<div style='text-align:center;font-size:9pt;'>
  <p>
    All trademarks and copyrights are owned by their respective parties.
    Comments and uploaded images are the responsibility of the poster.
  </p>
  <p>Copyright © 2014 Ignorama. All rights reserved.</p>
</div>
<br/>")

(defparameter *resource-dirs* '("js/" "css/" "img/"))
;; publish css, js, img, etc.
(loop for dir in *resource-dirs*
   do (publish-directory :prefix (concatenate 'string "/" dir)
			 :destination dir))

(defparameter *threads-query* "CALL QueryThreads;")

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
  "Content that goes in the header of every page.")

;;; html macros
(define-html-macro :rightlink (label)
  `(:a :class "header rightlink"
       :href ,(string-downcase label)
       ,label))

(define-html-macro :sociallink (site url)
  `(:a :class "header rightlink"
       :target "_blank"
       :href ,url
       (:span :class ,(concatenate 'string
				   "fa fa-"
				   (string-downcase (symbol-name site))))))

(define-html-macro :indextable (query)
  `(:table :class "table table-bordered fixed"
	   (:tr :class "thread-row"
		(:th :class "thread-row" "Thread")
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
		  (loop for row = (fetch result)
		     while row do
		       (html
			 (:tr
			  (:td (:print (getf row :|ThreadSubject|)))
			  (:td (:print (getf row :|ModName|)))
			  (:td (:print (getf row :|PostCount|)))
			  (:td (:print (getf row :|Tag|)))
			  (:td (:print (getf row :|LatestPostTime|))))))))))

(define-html-macro :indexbuttons ()
        ;; dropdown only display correctly when I wrap all the buttons in this div
    `(:div :class "dropdown"
	  (:button :class "btn btn-default btn-sm threads"
		   :onclick "window.location='newthread'"
		   "New Thread")
	  (:form :class "rightbuttons"
		 :action "php/submitfilter"
		 :method "post"
		 (:input :type "button"
			 :class "btn btn-default btn-sm threads"
			 :onclick "window.location='hiddenthreads'"
			 :value "Hidden Threads")
		 (:input :type "button"
			 :class "btn btn-default btn-sm threads"
			 :onclick "window.location='php/resettags'"
			 :value "Reset Tags")
		 (:input :type "submit"
			 :class "btn btn-default btn-sm threads"
			 :value "Apply Tags")

	   ;; code for tags dropdown
	   (:a :class "dropdown-toggle btn btn-default btn-sm"
	       :data-toggle "dropdown"
	       "Tags" (:b :class "caret"))
	   (:ul :class "dropdown-menu dropdown-menu-form pull-right"
		:role "menu"
		(:label :type "checkbox"
			(:li "test"))))))

;;; page skeleton
(defparameter *header*
  '((:div :class "header banner"
     (:div :class "header text"

      ;; logo and slogans
      (:span :class "hidden-xs"
	     (:a :href "/index"
		 (:img :class "header logo" :src "/img/ignorama.png"))
	     (:b :class "hidden-sm header slogan"
		 (echo (random-elt *slogans*))))
      (:span :class "visible-xs-inline"
	     (:a :href "/index"
		 (:img :class "header logo small" :src "/img/ignoramasmall.png")))

      ;; right links
      (:div :class "hidden-xs header rightlinks"
	    (:sociallink twitter "https://twitter.com/IgnoramaDotNet")
	    (:sociallink github "https://github.com/tssund93/lispy-ignorama")

	    (:rightlink "Following")
	    (:rightlink "Rules")
	    (:rightlink "Bans")
	    (:rightlink "Settings"))))))

(define-html-macro :standard-page ((&key title) &body body)
  "The basic format that every page will follow."
  `(:html
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
	   ,@body))))

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

;;; web pages beyond here
(publish-page login
  (:standard-page
   (:title "Login")
   (:body
    (:form :method "POST" :action "/admin/verify"
	   (:input :name "username" :type "text")
	   (:br)
	   (:input :name "password" :type "password")
	   (:br)
	   (:input :name "Submit1" :type "submit" :value "Submit")
	   (:input :type "button"
		   :value "Main Page"
		   :onclick "window.location='../'")))))

(publish-page index
  (:standard-page
   (:title "")
   (:body
    (:indexbuttons)
    (:indextable *threads-query*)
    (echo *fake-copyright*))))

(publish-page unicode-test
  (:standard-page
   (:title "Unicode Test")
   (:body
    (:p "これは機械翻訳です。"))))
