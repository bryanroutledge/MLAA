#!/bin/bash

RUN=$1
echo "FIX HEADER in $RUN"


for f in $(find ./$RUN -name "*.tex" -maxdepth 1 | grep -v NEW)
do
    cat $f | ./Fix.xtabletex.pl >$f.NEW.tex
done 