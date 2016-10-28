(in-package :net.ignorama.web)

(defpsmacro $ (selector &body chains)
  `(chain (j-query ,selector)
          ,@chains))

(defparameter *js*
  (ps
    ($ (lambda ()
         ($ ".time"
            (each (lambda ()
                    (let ((date (new (*date ($ this (html))))))
                      ($ this (text (chain date (to-locale-string))))))))))))

(hunchentoot:define-easy-handler (js :uri "/js/script.js") ()
  (setf (hunchentoot:content-type*) "application/javascript")
  *js*)
