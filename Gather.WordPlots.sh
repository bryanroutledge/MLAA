#!/bin/bash

RUN=$1
echo "PLOT Theta Words"


for f in $(find ./$RUN -name "Theta.*" -maxdepth 1 |sort )
do
    echo $f | perl -pe 's/\.pdf/\}/g;s/\.*\/*RUN[^\/]+/\\pgfimage\[width=0.6\\paperwidth\]\<\+\>\{\\modelRUN/g'
done 