year=$1
day=$2

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

./generate-package-yaml.py

./download_input.sh "$year" "$day"
