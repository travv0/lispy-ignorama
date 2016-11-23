(asdf:defsystem "ignorama"
  :description "ignorama: a dumb forum template thing"
  :version "0.0.1"
  :author "Travis"
  :licence "AGPL-3.0"
  :serial t
  :depends-on ("hunchentoot"
               "spinneret"
               "uuid"
               "cl-ppcre"
               "parenscript"
               "cl-dbi"
               "dbd-postgres"
               "local-time"
               "sha3"
               "binascii"
               "web-util")
  :pathname "./"
  :components ((:file "packages")
               (:file "src/forum-util")
               (:file "config")
               (:file "js/script")
               (:file "src/forum")))
