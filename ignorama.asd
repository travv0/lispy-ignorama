(defsystem "ignorama"
  :description "ignorama: a dumb forum template thing"
  :version "0.0.1"
  :author "Travis"
  :licence "AGPL-3.0"
  :depends-on ("aserve" "monkeylib-html" "cl-dbi" "local-time")
  :components ((:file "packages")
	       (:file "src/util")
               (:file "src/html-macros" :depends-on ("packages"))
               (:file "config" :depends-on ("src/html-macros"))
	       (:file "src/forum" :depends-on ("config" "src/util" "src/html-macros"))))
