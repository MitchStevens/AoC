year=$1
day=$2

if [ -z $2 ]; then
  year=$(date -d "$1" "+%Y")
  day=$(date -d "$1" "+%d")
fi


dir="./app/$year/Day$day"
file="$dir/Main.hs"
contents="import Advent

main :: IO ()
main = do
  input <- readInput $year $day
  pure ()
"

mkdir -p "$dir"
echo "$contents" > $file

./scripts/generate-package-yaml.py

./scripts/download_input.sh "$year" "$day"
