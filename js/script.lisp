(in-package :net.ignorama.web)

(defpsmacro $ (selector &body chains)
  `(chain (j-query ,selector)
          ,@chains))

(defparameter *js*
  (ps))

(hunchentoot:define-easy-handler (js :uri "/js/script.js") ()
  (setf (hunchentoot:content-type*) "application/javascript")
  *js*)

(defun view-thread-js ()
  (execute-query-one min-max-posts
      "SELECT max(PostID) AS MaxPost,
       min(PostID) AS MinPost
       FROM posts
       WHERE ThreadID = ?
       LIMIT ?
       OFFSET ?"
      ((get-parameter "thread")
       *posts-per-page*
       (if (get-parameter "page")
           (* (- (parse-integer
                  (get-parameter "page")) 1)
              *posts-per-page*)
           0))
    (with-html
      (:raw
       (ps
         ($ (lambda () (highlight-post (get-parameter-by-name "highlight"))))

         (defun highlight-post (post)
           ;; unhighlight all posts
           ($ "tr"
              (each (lambda ()
                      (if (not (equal ($ this (attr "id"))
                                      (+ "post" post)))
                          ($ this (css "background-color" "white"))))))
           ;; highlight the selected post
           ($ (+ "#post" post) (css "background-color" "#D6BBF2")))

         (defun view-post (post)
           (if (or (> post (lisp (getf min-max-posts :|maxpost|)))
                   (< post (lisp (getf min-max-posts :|minpost|))))
               (chain window (open (+ "/view-thread?post=" post)))
               (progn (highlight-post post)
                      (chain window (open (+ "#post" post) "_self")))))

         (defun go-to-page (sel)
           (setf window.location (+ "/view-thread?thread="
                                    (get-parameter-by-name "thread")
                                    "&page="
                                    (@ sel value)))))))))
