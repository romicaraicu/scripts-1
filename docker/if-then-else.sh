#!/usr/bin/env bash

if [[ "$#" -ne 3 ]]; then
  echo Expected arguments are missing: IF_CMD THEN_CMD ELSE_CMD
  exit 1
fi

if_cmd=$1
then_cmd=$2
else_cmd=$3

if eval $if_cmd; then
  eval $then_cmd
  exit_code=$?
  if [ "$exit_code" -ne 0 ]; then
    echo "ERROR: cannot perform then script"
    exit $exit_code
  fi
else
  eval $else_cmd
  exit_code=$?
  if [ "$exit_code" -ne 0 ]; then
    echo "ERROR: cannot perform else script"
    exit $exit_code
  fi
fi
