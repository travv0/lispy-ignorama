#!/usr/bin/env bash

apt install vim gcc git sbcl curl libmysqlclient-dev mysql-server -y

/etc/init.d/mysql restart

if [ ! -f quicklisp.lisp ]; then
    curl -O https://beta.quicklisp.org/quicklisp.lisp
    sbcl --non-interactive --load quicklisp.lisp --eval "(quicklisp-quickstart:install)"
fi

if [ ! -f /usr/lib/x86_64-linux-gnu/libmysqlclient_r.so ]; then
    ln -s /usr/lib/x86_64-linux-gnu/libmysqlclient.so /usr/lib/x86_64-linux-gnu/libmysqlclient_r.so
fi

git clone https://github.com/tssund93/lispy-ignorama

cd lispy-ignorama
./runscripts.sh
sbcl --load run.lisp
