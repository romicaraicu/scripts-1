#!/usr/bin/env bash

command -v inotifywait >/dev/null 2>&1 || { echo >&2 "[inotifywait] is required, but not installed.  Aborting."; exit 1; }
command -v date >/dev/null 2>&1 || { echo >&2 "[date] is required, but not installed.  Aborting."; exit 1; }

if [[ "$#" -ne 3 ]]; then
  echo "Expected arguments are missing: EXTENSIONS DELAY COMMAND"
  exit 1
fi

IFS=',' read -r -a exts <<< "$1"
delay=$2
cmd=$3

function run () {
  clear
  echo ":: Triggered on -> $(date) ::"
  echo ""
  bash -c "eval $cmd"
}

run
dt=$(date +%s)

inotifywait -q -r -m -e moved_to,moved_from ./ | while read path events file; do
  for ext in "${exts[@]}"; do
    if [[ $file =~ .$ext$ ]]; then
      if [[ $(date +%s) > $(($dt + $delay)) ]]; then
        dt=$(date +%s)
        run
      fi
    fi
  done
done
