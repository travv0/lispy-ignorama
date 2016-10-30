(asdf:defsystem "ignorama"
  :description "ignorama: a dumb forum template thing"
  :version "0.0.1"
  :author "Travis"
  :licence "AGPL-3.0"
  :depends-on ("hunchentoot"
               "spinneret"
               "uuid"
               "cl-ppcre"
               "parenscript"
               "clack-errors"
               "cl-dbi"
               "local-time")
  :components ((:file "packages")
               (:file "src/util" :depends-on ("config"))
               (:file "src/html-macros" :depends-on ("packages"))
               (:file "config" :depends-on ("src/html-macros"))
               (:file "js/script" :depends-on ("packages"))
               (:file "src/forum" :depends-on ("config" "src/util" "src/html-macros" "js/script"))
               (:file "src/redirects" :depends-on ("src/forum"))))
