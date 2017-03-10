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

YELLOW='\033[1;33m'
CYAN='\033[0;36m'
NC='\033[0m'

function run () {
  clear
  x=$(date -ud "$(date -u)" +'%s')
  echo -e "${YELLOW}>> Triggered on -> $(date) <<${NC}"
  echo ""
  bash -c "eval $cmd"
  echo -e "${YELLOW}>> Completed on -> $(date) <<${NC}"
  y=$(date -ud "$(date -u)" +'%s')
  s=$(($y-$x))
  m=$(($s/60))
  h=$(($s/60/60))
  printf "${CYAN}>>     Duration -> %02d:%02d:%02d                     <<${NC}\n" $h $m $s
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
