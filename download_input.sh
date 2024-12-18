year=$1
day=$2

dir="./inputs/$year"
file="$dir/day$day.txt"

mkdir -p "$dir"
touch "$file"
curl "https://adventofcode.com/$year/day/$day/input" --cookie "session=$AOC_SESSION_COOKIE" > "$file"