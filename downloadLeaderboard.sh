#!bin/bash

BOARD=400015
YEAR=2021
SESSION=53616c7465645f5f52a5683ceeaedcfd84fdd75d6cc71dfc14843c5b8a5e729acac393108d834bb3ee0ebc9c7a4633b6

filename="./leaderboard/$BOARD-$YEAR.js"

echo "Downloading to $filename"

echo 'callBack(' > $filename

curl -H "Cookie: session=$SESSION;" -o - "https://adventofcode.com/$YEAR/leaderboard/private/view/$BOARD.json" >> $filename

echo ')' >> $filename
