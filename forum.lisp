(in-package :com.ignorama.web)

;; utility functions
(defun random-elt (choices)
  "Choose an element from a list at random."
  (elt choices (random (length choices))))

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
    "Register or login"
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
    "Your hobbies are stupid"
    "We don't censor you"))

(defparameter *resource-dirs* '("js/" "css/" "img/"))
;; publish css, js, img, etc.
(loop for dir in *resource-dirs*
   do (publish-directory :prefix (concatenate 'string "/" dir)
			 :destination dir))

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

(defparameter *header*
  '((:div :class "header banner"
     (:div :class "header text"

      ;; logo and slogans
      (:span :class "hidden-xs"
	     (:a :href "/index"
		 (:img :class "header logo" :src "/img/ignorama.png"))
	     (:b :class "hidden-sm header slogan"
		 (format (request-reply-stream request) "~a" (random-elt *slogans*))))
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
    (:head (:title ,(concatenate 'string title " - " *site-name*))
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
	 (with-http-body (request entity)
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
    (:p "hello"))))
