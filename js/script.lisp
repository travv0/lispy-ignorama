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
                      ($ this (text (chain date (to-locale-string))))))))

         ;; ($ "#uploadForm" (submit
         ;;                   (lambda ()
         ;;                     ($ this (append
         ;;                              (with-html
         ;;                                (:input :type "hidden"
         ;;                                        :name "javascript"
         ;;                                        :value "yes"))))
         ;;                     (let ((iframe-name "iframeUpload")
         ;;                           (iframe-temp ($ (with-html
         ;;                                             (:iframe :id "uploadframe"
         ;;                                                      :name iframe-name
         ;;                                                      :src "about:blank"))))
         ;;                           (count 0)
         ;;                           (timeout 60)
         ;;                           (postcontents ($ "#postfield" value))
         ;;                           (uploadingtext (+ postcontents " Uploading file")))
         ;;                       ($ this (attr
         ;;                                (create :action "TODO"
         ;;                                        :method "post"
         ;;                                        :enctype "multipart/form-data"
         ;;                                        :encoding "multipart/form-data"
         ;;                                        :target iframe-name)))))))
         ))))

(hunchentoot:define-easy-handler (js :uri "/js/script.js") ()
  (setf (hunchentoot:content-type*) "application/javascript")
  *js*)
