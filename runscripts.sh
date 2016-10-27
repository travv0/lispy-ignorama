#/usr/bin/env bash
find ./sql/tables -type f -name '*.sql' | awk '{ print "source",$0 }' | tee /dev/tty | mysql --database=heroku_cb03cff5bc034d1 --host=us-cdbr-iron-east-04.cleardb.net --user=ba744961246c99 -p --batch
find ./sql/views -type f -name '*.sql' | awk '{ print "source",$0 }' | tee /dev/tty | mysql mysql --database=heroku_cb03cff5bc034d1 --host=us-cdbr-iron-east-04.cleardb.net --user=ba744961246c99 -p --batch
