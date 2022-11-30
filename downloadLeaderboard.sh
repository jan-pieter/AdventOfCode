#!bin/bash

BOARD=400015
YEAR=2022
SESSION=53616c7465645f5fbe656246a8867f24f56ec389a48a4b892c949dee2a170da2d2cd50e222e1540133317f7d745a9fff9a73392c979edc18f25ed811e792b764

filename="./leaderboard/$BOARD-$YEAR.js"

echo "Downloading to $filename"

echo 'callBack(' > $filename

curl -H "Cookie: session=$SESSION;" -o - "https://adventofcode.com/$YEAR/leaderboard/private/view/$BOARD.json" >> $filename

echo ')' >> $filename
