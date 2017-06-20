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

function trim_quotes () {
  line=$1
  if [ "$line" != "null" ]; then
    line=${line:1:$((${#line} - 2))}
  fi
  echo $line
}

NODES=($(curl --fail-early -sb -i -u $CREDS "$ADDRESS/api/nodes" | jq '.[] | .name'))

failing_nodes=0
for node in "${NODES[@]}"; do
  node=$(trim_quotes $node)
  xs=($(curl --fail-early -sb -i -u $CREDS "$ADDRESS/api/healthchecks/node/$node" | jq '.status, .reason'))
  status=$(trim_quotes "${xs[0]}")
  reason="${xs[@]:1}"
  if [ "$status" != "ok" ]; then
    echo "Node [$node] failing: $reason"
    failing_nodes=$((failing_nodes + 1))
  fi
done

if [ "$failing_nodes" -gt "0" ]; then
  exit 1
fi
