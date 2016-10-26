#/usr/bin/env bash
echo "CREATE DATABASE tssund93_forums" | mysql --user=root --password=password 
find ./sql/tables -type f -name '*.sql' | awk '{ print "source",$0 }' | tee /dev/tty | mysql tssund93_forums --user=root --password=password --batch
find ./sql/views -type f -name '*.sql' | awk '{ print "source",$0 }' | tee /dev/tty | mysql tssund93_forums --user=root --password=password --batch
