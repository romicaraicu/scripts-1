#!/usr/bin/env bash

GNAV="$HOME/.gnav"

function zr () {
  name=$1
  if [ -z "$name" ]; then
    echo -n "Name: "
    read name
  fi
  name=$(echo "$name" | awk '{print tolower($0)}')
  lines=($(cat "$GNAV" 2>/dev/null))
  rm -f "$GNAV" 2>/dev/null
  touch "$GNAV" 2>/dev/null
  found=0
  for line in "${lines[@]}"; do
    IFS=':' read -r -a xs <<< "$line"
    n=$(echo "${xs[0]}" | awk '{print tolower($0)}')
    v="${xs[@]:1}"
    if [ "$n" != "$name" ]; then
      echo "$n:$v" >> "$GNAV"
    else
      found=1
    fi
  done
  if [ "$found" -eq "0" ]; then
    echo "Supplied key is not present"
  fi
}

zr $@
