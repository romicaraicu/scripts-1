#!/usr/bin/env bash

command -v find >/dev/null 2>&1 || { echo >&2 "[find] is required, but not installed.  Aborting."; exit 1; }
command -v sed >/dev/null 2>&1 || { echo >&2 "[sed] is required, but not installed.  Aborting."; exit 1; }
command -v printf >/dev/null 2>&1 || { echo >&2 "[printf] is required, but not installed.  Aborting."; exit 1; }

if [[ "$#" -ne 2 ]]; then
  echo "Expected arguments are missing: TARGET_HOST RABBITMQ_CREDENTIALS"
  exit 1
fi

RED='\033[1;31m'
GREEN='\033[1;32m'
CYAN='\033[0;36m'
GRAY='\033[1;30m'
NC='\033[0m'

TARGET_HOST=$1
RABBITMQ_CREDENTIALS=$2
ROOT="$(dirname "$(readlink -f "$0")")"

function format () {
  line='--------------------------------------------------------------------------------'
  PROC_NAME=$1
  printf "${RED}#${NC} ${CYAN}%s${NC} ${GRAY}%s${NC} [${GREEN}DONE${NC}]\n" $PROC_NAME "${line:${#PROC_NAME}}"
}

inputs=($(find "$ROOT/checks" -type f -iregex ".*\.input$"))
offset=$((${#ROOT} + 8))
echo ""
for input in "${inputs[@]}"; do
  sed -i "s/127.0.0.1/$TARGET_HOST/" $input
  sed -i "s/guest:guest/$RABBITMQ_CREDENTIALS/" $input
  format "${input:$offset}"
done
