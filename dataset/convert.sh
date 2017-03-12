#! /bin/sh
for i in $(ls O90); do
    ly musicxml O90/$i/$i.ly -o $i.xml
done
