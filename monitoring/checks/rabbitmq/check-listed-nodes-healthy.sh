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
NODES=("${INPUT[@]}")
CURL_MAX_TIME=5

failing_nodes=0
for node in "${NODES[@]}"; do
  result=$(curl --max-time $CURL_MAX_TIME --fail --fail-early -sb -i -u $CREDS "$ADDRESS/api/healthchecks/node/$node")
  rc=$?
  if [ ! "$rc" -eq "0" ]; then
    echo "Server seams to be offline"
    exit 1
  fi
  xs=($(echo $result | jq '.status, .reason'))
  status="${xs[0]}"
  reason="${xs[@]:1}"
  if [ "$status" != "\"ok\"" ]; then
    echo "Node [$node] failing: $reason"
    failing_nodes=$((failing_nodes + 1))
  fi
done

if [ "$failing_nodes" -gt "0" ]; then
  exit 1
fi
