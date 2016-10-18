sudo apt install sbcl curl libmysqlclient-dev -y

curl -O https://beta.quicklisp.org/quicklisp.lisp
sbcl --non-interactive --load quicklisp.lisp --eval "(quicklisp-quickstart:install)"

if [ ! -f /usr/lib/x86_64-linux-gnu/libmysqlclient_r.so ]; then
    sudo ln -s /usr/lib/x86_64-linux-gnu/libmysqlclient.so /usr/lib/x86_64-linux-gnu/libmysqlclient_r.so
fi
