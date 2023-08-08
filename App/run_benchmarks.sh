#!/usr/bin/env bash

# set -e makes the script exit if any command fails
# set -u makes the script exit if any unset variable is used
# set -o pipefail makes the script exit if any command in a pipeline fails
set -euo pipefail

for i in {1..8}
do
    echo "---"
    echo ">threads: $i"
    ./clj flight_reservation_parallel.clj experiment-speedup-threads $i > "benchmarks/result-speedupthread -$i.txt"
done
