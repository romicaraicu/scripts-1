#!/usr/bin/env bash

if [[ "$#" -ne 1 ]]; then
  echo "Expected arguments are missing: SERVICE_NAME_PREFIX"
  exit 1
fi

prefix=$1

if [ "$(docker service ls | wc -l)" -eq "2" ]; then
  exit 0
fi

docker service ls -q | \
  xargs -L1 docker service ps | \
  awk "(\$1 !~ /ID/) && (\$2 ~ /^$prefix/) { print \$0 }"
