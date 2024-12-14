year=$1
day=$2

dir="./src/$year/Day$day"
file="$dir/Main.hs"
contents="import Advent

main :: IO ()
main = do
  input <- readInput $year $day
  pure ()
"

mkdir -p "$dir"
echo "$contents" > $file

./generate-package-yaml

./download_input.sh "$year" "$day"
