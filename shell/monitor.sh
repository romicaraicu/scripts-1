#!/usr/bin/env bash

command -v date >/dev/null 2>&1 || { echo >&2 "[date] is required, but not installed.  Aborting."; exit 1; }
command -v find >/dev/null 2>&1 || { echo >&2 "[find] is required, but not installed.  Aborting."; exit 1; }
command -v stat >/dev/null 2>&1 || { echo >&2 "[stat] is required, but not installed.  Aborting."; exit 1; }
command -v sort >/dev/null 2>&1 || { echo >&2 "[sort] is required, but not installed.  Aborting."; exit 1; }
command -v head >/dev/null 2>&1 || { echo >&2 "[head] is required, but not installed.  Aborting."; exit 1; }

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
  echo ""
  echo -e "${YELLOW}>> Completed on -> $(date) <<${NC}"
  y=$(date -ud "$(date -u)" +'%s')
  s=$(($y-$x))
  m=$(($s/60))
  h=$(($s/60/60))
  printf "${CYAN}>>     Duration -> %02d:%02d:%02d                     <<${NC}\n" $h $m $s
}

last_changed="find ./ -type f \( "
for ext in "${exts[@]}"; do
  last_changed="$last_changed -iname \"*.$ext\" -o"
done
last_changed=${last_changed::-2}
last_changed="$last_changed \) -exec stat -c \"%Y\" {} ';' | sort -r | head -n 1"

t=0
while true; do
  x=$(eval $last_changed)
  if [ -z "$x" ]; then x=0; fi
  if [[ $x > $t ]]; then
    t=$x
    run
  fi
  sleep $delay
done
