#!/usr/bin/env bash

command -v find >/dev/null 2>&1 || { echo >&2 "[find] is required, but not installed.  Aborting."; exit 1; }
command -v sed >/dev/null 2>&1 || { echo >&2 "[sed] is required, but not installed.  Aborting."; exit 1; }

if [[ "$#" -ne 2 ]]; then
  echo "Expected arguments are missing: TARGET_HOST RABBITMQ_CREDENTIALS"
  exit 1
fi

TARGET_HOST=$1
RABBITMQ_CREDENTIALS=$2
ROOT="$(dirname "$(readlink -f "$0")")"

inputs=($(find "$ROOT/checks" -type f -iregex ".*\.input$"))
offset=$((${#ROOT} + 8))
for input in "${inputs[@]}"; do
  echo -n "# ${input:$offset} ... "
  sed -i "s/127.0.0.1/$TARGET_HOST/" $input
  sed -i "s/guest:guest/$RABBITMQ_CREDENTIALS/" $input
  echo "done."
done
