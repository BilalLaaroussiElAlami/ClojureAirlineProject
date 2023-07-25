#!/usr/bin/env bash

# set -e makes the script exit if any command fails
# set -u makes the script exit if any unset variable is used
# set -o pipefail makes the script exit if any command in a pipeline fails
set -euo pipefail

for i in {1..30}
do
    echo "---"
    echo "> simple, iteration $i"
    ./clj flight_reservation.clj simple > "benchmarks/result-simple-$i.txt"
    echo "> random, iteration $i"
    ./clj flight_reservation.clj random > "benchmarks/result-random-$i.txt"
done
