#!/usr/bin/env bash

if [[ "$#" -ne 3 ]]; then
  echo "Expected arguments are missing: EXTENSIONS DELAY COMMAND"
  exit 1
fi

IFS=',' read -r -a exts <<< "$1"
delay=$2
cmd=$3

eval $cmd

while true; do
  inotifywait -q -r -e moved_to,moved_from ./ | while read path events file; do

    for ext in "${exts[@]}"; do
      if [[ $file =~ .$ext$ ]]; then
        echo "$file has changed in $path, triggering command ..."
        sleep $delay
        eval $cmd
      fi
    done

  done
done
