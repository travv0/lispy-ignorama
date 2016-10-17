;; -*- coding:utf-8 -*-
(in-package :net.ignorama.web)

;;; connect to database (FIXME: don't use root lol)
(defvar *db* (connect :mysql
		      :username "root"
		      :password "password"
		      :database-name "tssund93_forums"))

(defparameter *site-name* "Ignorama")

;;; will load random slogan on each page load
;;; for a single slogan, just include on string in the list
;;; for no slogan, leave it empty or nil
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

(defparameter *nameless-name* "Anonymous")

;;; the links that show up on the right side of the header
;;; links to external social sites:
;;;		(:sociallink :name-of-social-site "https://site-url.com/your-site.html")
;;;	go to http://fontawesome.io/icons/ to see choices for :name-of-social-site
;;; links to other pages on this site:
;;;		(:rightlink "Pagename") will take you to https://your-site/pagename
(define-html-macro :rightlinks ()
  `(html (:sociallink :twitter "https://twitter.com/IgnoramaDotNet")
	 (:sociallink :github "https://github.com/tssund93/lispy-ignorama")

	 (:rightlink "Following")
	 (:rightlink "Rules")
	 (:rightlink "Bans")
	 (:rightlink "Settings")))
