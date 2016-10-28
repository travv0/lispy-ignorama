;; -*- coding:utf-8 -*-
(in-package :net.ignorama.web)

(defun heroku-getenv (target)
  #+ccl (getenv target)
  #+sbcl (sb-posix:getenv target))

;;; connect to database
(defmacro with-db (conn &body body)
  `(let ((db-url (quri:uri (heroku-getenv "DATABASE_URL"))))
     (with-connection (,conn :postgres
                             :host (format nil "~a" (quri:uri-host db-url))
                             :username (first (split-sequence:split-sequence #\: (quri:uri-userinfo db-url)))
                             :password (second (split-sequence:split-sequence #\: (quri:uri-userinfo db-url)))
                             :database-name (format nil "~a" (string-left-trim '(#\/) (quri:uri-path db-url))))
       ,@body)))

(defparameter *site-name* "Ignorama")

;;; will load random slogan on each page load
;;; for a single slogan, just include one string in the list
;;; for no slogan, leave it empty or nil
(defparameter *slogans*
  '("I should've made it blue"
    "Ignorance for the masses"
    "Made with MS Paint"
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

(defparameter *nameless-name* "Anonymous")

;;; allow-anonymity takes precedence over force-anonymity
;;; e.g. if allow is set to nil and force is set to t, you'll still see people's names
(defparameter *allow-anonymity* t)
(defparameter *force-anonymity* nil)

;;; links to external social sites:
;;;             (:name-of-social-site "https://site-url.com/your-site.html" "Optional Dropdown Text")
;;;     go to http://fontawesome.io/icons/ to see choices for :name-of-social-site
(defparameter *sociallinks* '((:twitter "https://twitter.com/IgnoramaDotNet")
                              (:github "https://github.com/tssund93/lispy-ignorama" "GitHub")))
