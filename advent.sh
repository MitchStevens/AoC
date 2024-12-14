#!/bin/bash

year=$1
day=$2
watch=$3

#file="./app/Main.hs"
#
#echo "module Main where " > "$file"
#
#for module in $(ls "./src/$year" | cut -d'.' -f1); do
#    echo "import $module" >> "$file"
#done
#
#echo -e "
#main :: IO ()
#main = day$day
#" >> "$file"

file="./src/$year/day$day.hs"

chmod +x $file
if [ "$watch" == "-w" ]; then
    find src test -name "*.hs" | entr -s $file
else
    $file
fi


