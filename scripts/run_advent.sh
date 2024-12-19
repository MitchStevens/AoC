#!/bin/bash

language=$1
year=$2
day=$3
# watch=$3

./scripts/download_input.sh "$year" "$day"
case "$language" in
    "haskell")
        ./scripts/generate-package-yaml.py
        stack run "advent-$year-$day"
        ;;
    "racket")
        echo "racket time"
        ;;
    *)
        echo "exit"
        ;;
esac



##file="./src/$year/day$day.hs"
##
##chmod +x $file
##if [ "$watch" == "-w" ]; then
##    find src test -name "*.hs" | entr -s $file
##else
##    $file
##fi


