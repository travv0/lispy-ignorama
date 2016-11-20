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
               "cl-dbi"
               "local-time"
               "sha3"
               "binascii"
               "web-util")
  :components ((:file "packages")
               (:file "src/forum-util" :depends-on ("packages"))
               (:file "config" :depends-on ("packages"))
               (:file "js/script" :depends-on ("packages"))
               (:file "src/forum" :depends-on ("config"
                                               "src/forum-util"
                                               "js/script"))))
