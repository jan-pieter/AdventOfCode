#!/bin/bash

YEAR=2023
DAY=1

formatted_day=$(printf "%02d" $DAY)

echo "Downloading to ./$YEAR/src/main/resources/$formatted_day-input.txt"

curl -H "Cookie: session=$SESSION;"  -A "jan-pieter via curl" -o "./$YEAR/src/main/resources/$formatted_day-input.txt" "https://adventofcode.com/$YEAR/day/$DAY/input"
