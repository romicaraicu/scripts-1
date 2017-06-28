#!/usr/bin/env bash

command -v curl >/dev/null 2>&1 || { echo >&2 "[curl] is required, but not installed.  Aborting."; exit 1; }
command -v jq >/dev/null 2>&1 || { echo >&2 "[jq] is required, but not installed.  Aborting."; exit 1; }
command -v sed >/dev/null 2>&1 || { echo >&2 "[sed] is required, but not installed.  Aborting."; exit 1; }
command -v sort >/dev/null 2>&1 || { echo >&2 "[sort] is required, but not installed.  Aborting."; exit 1; }

ROOT="$(dirname "$(readlink -f "$0")")"
NAME=$(basename "$0")
RABBITMQ_INPUT=($(cat "$ROOT/../../.rabbitmq" 2> /dev/null))
INPUT_FILE="$ROOT/${NAME%.*}.input"
INPUT=($(cat $INPUT_FILE 2> /dev/null))

if [ -z "$RABBITMQ_INPUT" ]; then
  echo "RabbitMQ configuration for check is not set (.rabbitmq)"
  exit 1
fi

if [ -z "$INPUT" ]; then
  echo "Configuration for check is not set"
  exit 1
fi

ADDRESS="${RABBITMQ_INPUT[0]}"
CREDS="${RABBITMQ_INPUT[1]}"
ERROR_QUEUES="${INPUT[0]}"
CURL_MAX_TIME=5

result=$(curl --max-time $CURL_MAX_TIME --fail --fail-early -sb -i -u $CREDS "$ADDRESS/api/queues")
rc=$?
if [ ! "$rc" -eq "0" ]; then
  echo "Server seams to be offline"
  exit 1
fi

QUEUES=($( \
  echo $result | \
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
    if [ "${n:=0}" -gt "0" ]; then
      echo "Error queue [$queue] has some messages: $n"
      errors=$(($errors + 1))
    fi
  fi
done

if [ "$errors" -gt "0" ]; then
  exit 2
fi
