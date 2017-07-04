#!/usr/bin/env bash

GNAV="$HOME/.gnav"

function zz () {
  name=$1
  if [ -z "$name" ]; then
    # echo "Missing arguments"
    exit 1
  fi
  name=$(echo "$name" | awk '{print tolower($0)}')
  lines=($(cat "$GNAV" 2>/dev/null))
  found=0
  for line in "${lines[@]}"; do
    IFS=':' read -r -a xs <<< "$line"
    n=$(echo "${xs[0]}" | awk '{print tolower($0)}')
    v="${xs[@]:1}"
    if [ "$n" == "$name" ]; then
      found=1
      if [ ! -d "$v" ]; then
        # echo "Target directory does not exist: $v"
        exit 1
      else
        echo "$v"
      fi
    fi
  done
  if [ "$found" -eq "0" ]; then
    # echo "Supplied key is not present"
    exit 1
  fi
}

zz $@
