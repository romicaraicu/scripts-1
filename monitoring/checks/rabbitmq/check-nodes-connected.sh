#!/usr/bin/env bash

command -v curl >/dev/null 2>&1 || { echo >&2 "[curl] is required, but not installed.  Aborting."; exit 1; }
command -v jq >/dev/null 2>&1 || { echo >&2 "[jq] is required, but not installed.  Aborting."; exit 1; }
command -v sed >/dev/null 2>&1 || { echo >&2 "[sed] is required, but not installed.  Aborting."; exit 1; }
command -v sort >/dev/null 2>&1 || { echo >&2 "[sort] is required, but not installed.  Aborting."; exit 1; }

ROOT="$(dirname "$(readlink -f "$0")")"
NAME=$(basename "$0")
INPUT_FILE="$ROOT/${NAME%.*}.input"
INPUT=($(cat $INPUT_FILE 2> /dev/null))
EXPECTED_FILE="$ROOT/${NAME%.*}.expected"
EXPECTED=$(cat $EXPECTED_FILE 2> /dev/null)

if [ -z "$INPUT" ]; then
  echo "Configuration for check is not set"
  exit 1
fi

ADDRESS="${INPUT[0]}"
CREDS="${INPUT[1]}"
CURL_MAX_TIME=5

result=$(curl --max-time $CURL_MAX_TIME --fail --fail-early -sb -i -u $CREDS "$ADDRESS/api/nodes")
rc=$?
if [ ! "$rc" -eq "0" ]; then
  echo "Server seams to be offline"
  exit 1
fi

ACTUAL=$( \
  echo $result | \
  jq '.[] | .name, .running' | \
  sed ':a;N;$!ba;s/\n/ /g' | \
  sed 's/true /true\n/g' | \
  sed 's/false /false\n/g' | \
  sort )

if [ "$EXPECTED" != "$ACTUAL" ]; then
  echo "Expected:\n$EXPECTED"
  echo "Actual:\n$ACTUAL"
  exit 1
fi
