#!/usr/bin/env bash

# +------------------------------------------------------+
# | Script to download input from Advent Of Code Website |
# | using `Curl`.                                        |
# | Arguments are day and year                           |
# +------------------------------------------------------+

# default value for YEAR is current year, otherwise first argument to script
if [ -z "${1}" ]
then 
    YEAR=$(date +%Y)
else
    YEAR="${1}"
fi

# default value for DAY is current day, otherwise second argument to script
if [ -z "${2}" ]
then
    DAY=$(date +%d)
else
    DAY="${2}"
fi

DAY_NO_ZEROS="$(echo $DAY | sed 's/^0*//')"
PUZZLE_URL="https://adventofcode.com/${YEAR}/day/${DAY_NO_ZEROS}/input"
PUZZLE_FILE="input/day_${DAY}.txt"

# prevent overwriting a file accidentally
if [ -f "$PUZZLE_FILE" ]
then
    echo "File already exists, exiting." && exit
else
    curl "${PUZZLE_URL}" -H "cookie: session=${AOC_SESSION_COOKIE}" -o "${PUZZLE_FILE}" 2>/dev/null
fi
