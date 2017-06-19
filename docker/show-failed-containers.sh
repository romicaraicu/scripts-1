#!/usr/bin/env bash

command -v docker >/dev/null 2>&1 || { echo >&2 "[docker] is required, but not installed.  Aborting."; exit 1; }
command -v wc >/dev/null 2>&1 || { echo >&2 "[wc] is required, but not installed.  Aborting."; exit 1; }

echo "Monitoring: $DOCKER_HOST"

services=$(docker service ls | grep -v "^ID" | awk '{print $2}' | sed -e :a -e '$!N; s/\n/ /; ta')
IFS=' ' read -r -a services <<< "$services"
count=0
for service in "${services[@]}"; do
  n=$(docker service ps $service | grep "\_" | wc -l)
  if [[ $n -gt 0 ]]; then
    count=$(( $count + $n ))
    docker service ps $service | grep "\_" | awk '{ print $1 " @ " $5 "\t" $3 "\t\t" $4 }'
  fi
done

echo ""
echo "Total # of dead containers: $count"
