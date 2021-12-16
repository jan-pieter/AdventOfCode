#!bin/bash

YEAR=2021
DAY=16
SESSION=53616c7465645f5f456caa90bea02d91f62c22f3e41038bc2da4ae45f0dbc5dc1b796c28d2e26b61c3c61c0013e121c9

formatted_day=$(printf "%02d" $DAY)

echo "Downloading to ./$YEAR/src/main/resources/$formatted_day-input.txt"

curl -H "Cookie: session=$SESSION;" -o "./$YEAR/src/main/resources/$formatted_day-input.txt" "https://adventofcode.com/$YEAR/day/$DAY/input"
