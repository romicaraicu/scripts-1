#!/usr/bin/env bash

command -v printf >/dev/null 2>&1 || { echo >&2 "[printf] is required, but not installed.  Aborting."; exit 1; }

GNAV="$HOME/.gnav"

RED='\033[1;31m'
YELLOW='\033[1;33m'
CYAN='\033[0;36m'
GRAY='\033[1;30m'
NC='\033[0m'

function format () {
  line='--------------------------------------------------------------------------------------------'
  name=$1
  root=$2
  length=$((${#name} + ${#root}))
  printf "${RED}#${NC} ${CYAN}%s${NC} ${GRAY}%s${NC} [${YELLOW} %s ${NC}]\n" "$name" "${line:$length}" "$root"
}

function zl () {
  lines=($(cat "$GNAV" 2>/dev/null))
  for line in "${lines[@]}"; do
    IFS=':' read -r -a xs <<< "$line"
    n=$(echo "${xs[0]}" | awk '{print tolower($0)}')
    v="${xs[@]:1}"
    format "$n" "$v"
  done
}

zl
