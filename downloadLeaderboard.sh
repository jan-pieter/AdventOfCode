#!/bin/bash

BOARD=400015
YEAR=2023

filename="./leaderboard/$BOARD-$YEAR.js"

echo "Downloading to $filename"

echo 'callBack(' > $filename

curl -H "Cookie: session=$SESSION;" -A "jan-pieter via curl" -o - "https://adventofcode.com/$YEAR/leaderboard/private/view/$BOARD.json" >> $filename

echo ')' >> $filename
