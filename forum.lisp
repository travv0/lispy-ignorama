(in-package :com.ignorama.web)

(defparameter *site-name* "Ignorama")

(defparameter *head*
  `((:meta :charset="UTF-8")
    (:meta :name "viewport"
	   :content "width=device-width, initial-scale=1, maximum-scale=1")

    (:link :rel "shortcut icon"
	  :type "image/png"
	  :href "/img/favicon.png")

    (:link :rel "stylesheet" :href "//maxcdn.bootstrapcdn.com/bootstrap/3.2.0/css/bootstrap.min.css")
    (:link :rel "stylesheet" :href "//maxcdn.bootstrapcdn.com/bootstrap/3.2.0/css/bootstrap-theme.min.css")
    (:link :rel "stylesheet" :href "/css/style.css")

    (:script :src "//code.jquery.com/jquery-1.11.0.min.js")
    (:script :src "//maxcdn.bootstrapcdn.com/bootstrap/3.2.0/js/bootstrap.min.js")
    (:script :src "/js/js.js"))
  "Content that goes in the header of every page.")

(defparameter *header*
  '((:div :class "header banner"
     (:div :class "header text"
      (:span :class "hidden-xs"
	     (:a :href "/index"
		 (:img :src "/img/ignorama.png")))))))


(define-html-macro :standard-page ((&key title) &body body)
  "The basic format that every page will follow."
  `(:html
    (:head (:title ,(concatenate 'string title " - " *site-name*))
	   ,@*head*
	   ,@*header*)
    (:body
     (:div :class "container"
	   (:h2 ,title)
	   ,@body))))

(defparameter *resource-dirs* '("js/" "css/" "img/"))

;; publish css, js, img, etc.
(loop for dir in *resource-dirs*
   do (publish-directory :prefix (concatenate 'string "/" dir)
			 :destination dir))

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
	   (:input :type "button" :value "Main Page" :onclick "window.location='../'")))))

(publish-page index
  (:standard-page
   (:title "")
   (:body
    (:p "hello"))))
