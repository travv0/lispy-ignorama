env $(cat .env | xargs) sbcl --load "run.lisp"
