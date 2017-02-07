#!/usr/bin/env bash

if [[ "$#" -ne 3 ]]; then
  echo "Expected arguments are missing: EXTENSIONS DELAY COMMAND"
  exit 1
fi

IFS=',' read -r -a exts <<< "$1"
delay=$2
cmd=$3

RED='\033[1;31m'
GREEN='\033[1;32m'
BLUE='\033[1;34m'
NC='\033[0m'

function run () {
  eval $cmd
}

run

inotifywait -q -r -m -e moved_to,moved_from ./ | while read path events file; do
  for ext in "${exts[@]}"; do
    if [[ $file =~ .$ext$ ]]; then
      echo -e "${GREEN}$file${NC} has changed in ${BLUE}$path${NC}, triggering command ..."
      sleep $delay
      run
    fi
  done
done
