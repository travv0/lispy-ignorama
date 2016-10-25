(defsystem "ignorama"
           :description "ignorama: a dumb forum template thing"
           :version "0.0.1"
           :author "Travis"
           :licence "AGPL-3.0"
           :depends-on ("clack" "ningle" "spinneret" "uuid" "clack-errors" "cl-dbi" "local-time" "parenscript")
           :components ((:file "packages")
                        (:file "src/util")
                        (:file "src/html-macros" :depends-on ("packages"))
                        (:file "config" :depends-on ("src/html-macros"))
                        (:file "js/script" :depends-on ("src/forum"))
                        (:file "src/forum" :depends-on ("config" "src/util" "src/html-macros"))))
