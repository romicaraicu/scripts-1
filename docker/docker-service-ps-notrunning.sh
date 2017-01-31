#!/usr/bin/env bash

if [ "$(docker service ls | wc -l)" -eq "2" ]; then
  exit 0
fi

docker service ls -q | \
  xargs -L1 docker service ps | \
  awk '($1 !~ /ID/) && ($2 ~ /^\_/) { print $0 }'
