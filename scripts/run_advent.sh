#!/bin/bash

language=$1
year=$2
day=$3
watch=$4

./scripts/download_input.sh "$year" "$day"
case "$language" in
    "haskell")
        ./scripts/generate-package-yaml.py
        command="stack run advent-$year-$day"
        ;;
    "racket")
        echo "racket time"
        ;;
    *)
        echo "exit"
        ;;
esac




if [ "$watch" == "-w" ]; then
    find src app test -name "*.hs" | entr -s "$command"
else 
    echo $watch
    $command
fi


