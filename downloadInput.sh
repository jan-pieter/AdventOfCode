#!bin/bash

YEAR=2021
DAY=2
SESSION=53616c7465645f5f52a5683ceeaedcfd84fdd75d6cc71dfc14843c5b8a5e729acac393108d834bb3ee0ebc9c7a4633b6

formatted_day=$(printf "%02d" $DAY)

echo "Downloading to ./$YEAR/src/main/resources/$formatted_day-input.txt"

curl -H "Cookie: session=$SESSION;" -o "./$YEAR/src/main/resources/$formatted_day-input.txt" "https://adventofcode.com/$YEAR/day/$DAY/input"
