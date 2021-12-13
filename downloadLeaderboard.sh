#!bin/bash

BOARD=400015
YEAR=2021
SESSION=53616c7465645f5f456caa90bea02d91f62c22f3e41038bc2da4ae45f0dbc5dc1b796c28d2e26b61c3c61c0013e121c9

filename="./leaderboard/$BOARD-$YEAR.js"

echo "Downloading to $filename"

echo 'callBack(' > $filename

curl -H "Cookie: session=$SESSION;" -o - "https://adventofcode.com/$YEAR/leaderboard/private/view/$BOARD.json" >> $filename

echo ')' >> $filename
