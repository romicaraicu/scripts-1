#!/usr/bin/env bash

command -v docker >/dev/null 2>&1 || { echo >&2 "[docker] is required, but not installed.  Aborting."; exit 1; }
command -v awk >/dev/null 2>&1 || { echo >&2 "[awk] is required, but not installed.  Aborting."; exit 1; }

ROOT="$(dirname "$(readlink -f "$0")")"
NAME=$(basename "$0")
INPUT_FILE="$ROOT/${NAME%.*}.input"
INPUT=($(cat $INPUT_FILE 2> /dev/null))

if [ -z "$INPUT" ]; then
  echo "Configuration for check is not set"
  exit 1
fi

export DOCKER_HOST="${INPUT[0]}"

failing_services=0
services=($(docker service ls | grep -v '^ID' | awk '{print $2"/"$4}'))

if [ "${#services}" -eq "0" ]; then
  echo "No services found"
  exit 1
fi

for line in "${services[@]}"; do
  IFS='/' read -r -a xs <<< "$line"
  name="${xs[0]}"
  m="${xs[1]}"
  n="${xs[2]}"
  if [ "$n" -ne "$m" ]; then
    echo "$name ($m / $n)"
    $failing_services=$(($failing_services + 1))
  fi
done

if [ "$failing_services" -gt "0" ]; then
  exit 1
fi
