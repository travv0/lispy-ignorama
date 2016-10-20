#/usr/bin/env bash
find ./sql -type f -name '*.sql' | awk '{ print "source",$0 }' | tee /dev/tty | mysql tssund93_forums --user=root --password=password --batch
