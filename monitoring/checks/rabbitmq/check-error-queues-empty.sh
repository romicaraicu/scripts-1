#!/usr/bin/env bash

command -v curl >/dev/null 2>&1 || { echo >&2 "[curl] is required, but not installed.  Aborting."; exit 1; }
command -v jq >/dev/null 2>&1 || { echo >&2 "[jq] is required, but not installed.  Aborting."; exit 1; }
command -v sed >/dev/null 2>&1 || { echo >&2 "[sed] is required, but not installed.  Aborting."; exit 1; }
command -v sort >/dev/null 2>&1 || { echo >&2 "[sort] is required, but not installed.  Aborting."; exit 1; }

ROOT="$(dirname "$(readlink -f "$0")")"
NAME=$(basename "$0")
INPUT_FILE="$ROOT/${NAME%.*}.input"
INPUT=($(cat $INPUT_FILE 2> /dev/null))

if [ -z "$INPUT" ]; then
  echo "Configuration for check is not set"
  exit 1
fi

ADDRESS="${INPUT[0]}"
CREDS="${INPUT[1]}"
ERROR_QUEUES="${INPUT[2]}"

QUEUES=($( \
  curl --fail-early -sb -i -u $CREDS "$ADDRESS/api/queues" | \
  jq '.[] | .name, .messages' | \
  sed ':a;N;$!ba;s/\n/ /g' | \
  sed -r 's/" ([0-9]+)/"=\1\n/g' | \
  sort ))

errors=0
for queue in "${QUEUES[@]}"; do
  if [[ $queue =~ $ERROR_QUEUES ]]; then
    IFS='=' read -r -a xs <<< "$queue"
    queue="${xs[0]}"
    n="${xs[1]}"
    if [ "$n" -gt "0" ]; then
      echo "Error queue [$queue] has some messages: $n"
      errors=$(($errors + 1))
    fi
  fi
done

if [ "$errors" -gt "0" ]; then
  exit 2
fi
