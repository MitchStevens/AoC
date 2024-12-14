#!/bin/bash

watch=0
command="stack test"

while getopts "t:w" opt; do
    case $opt in
        #w) find src test -name "*.hs" | entr -r "$command" ;;
        w) watch=1 ;;
        t) command="stack build :$OPTARG-test" ;;
        *) ;;
    esac
done

if [ "$watch" -eq 0 ]; then
    eval "$command"
else
    find src test -name "*.hs" | entr -s "$command"
fi
