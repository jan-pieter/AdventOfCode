#!/bin/bash

YEAR=2022
DAY=3
SESSION=53616c7465645f5fbe656246a8867f24f56ec389a48a4b892c949dee2a170da2d2cd50e222e1540133317f7d745a9fff9a73392c979edc18f25ed811e792b764

formatted_day=$(printf "%02d" $DAY)

echo "Downloading to ./$YEAR/src/main/resources/$formatted_day-input.txt"

curl -H "Cookie: session=$SESSION;" -o "./$YEAR/src/main/resources/$formatted_day-input.txt" "https://adventofcode.com/$YEAR/day/$DAY/input"
