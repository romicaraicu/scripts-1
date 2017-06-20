#!/usr/bin/env bash

command -v find >/dev/null 2>&1 || { echo >&2 "[find] is required, but not installed.  Aborting."; exit 1; }
command -v printf >/dev/null 2>&1 || { echo >&2 "[printf] is required, but not installed.  Aborting."; exit 1; }
command -v notify-send >/dev/null 2>&1 || { echo >&2 "[notify-send] is required, but not installed.  Aborting."; exit 1; }

FLAG=$1
ROOT="$(dirname "$(readlink -f "$0")")"
LOCK="$ROOT/lock"
CHECKS_OUTPUT="$ROOT/checks-output"
PASSED_OUTPUT="$ROOT/passed-output"
FAILED_OUTPUT="$ROOT/failed-output"
NOTIFY_TIME=$((5 * 1000))
IMAGE_SIZE=24

function xnotify_healthy () {
  notify-send -t $NOTIFY_TIME -i "$ROOT/icons/healthy.png" "$1" "$2"
}

function xnotify_sick () {
  notify-send -t $NOTIFY_TIME -i "$ROOT/icons/sick.png" "$1" "$2"
}

function run_checks () {
  echo "" > $CHECKS_OUTPUT
  checks=($(find "$ROOT/checks" -type f -iregex ".*\.sh$"))
  offset=$((${#ROOT} + 8))
  failed_checks=0
  passed_checks=0
  for check in "${checks[@]}"; do
    echo "# ${check:$offset} ..." >> $CHECKS_OUTPUT
    $check >> $CHECKS_OUTPUT
    rc=$?
    if [ "$rc" -eq "0" ]; then
      passed_checks=$(($passed_checks + 1))
      echo " >> PASSED" >> $CHECKS_OUTPUT
    else
      failed_checks=$(($failed_checks + 1))
      echo " >> FAILED" >> $CHECKS_OUTPUT
    fi
    echo "" >> $CHECKS_OUTPUT
  done
  echo "$failed_checks" > $FAILED_OUTPUT
  echo "$passed_checks" > $PASSED_OUTPUT
}

output=$(cat $CHECKS_OUTPUT 2> /dev/null)
passed=$(cat $PASSED_OUTPUT 2> /dev/null)
failed=$(cat $FAILED_OUTPUT 2> /dev/null)
icon=$(if [ "$failed" -gt "0" ]; then echo "sick"; else echo "healthy"; fi)

( flock -x 200
if [ "$FLAG" == "-n" ]; then
  printf "Failed: $failed\nPassed: $passed\n\n$output" | vim -g -
  if [ "$failed" -gt "0" ]; then
    xnotify_sick "Sick" "System has some failing checks"
  else
    xnotify_healthy "Healthy" "System seams to be fully operational"
  fi
else
  $(run_checks)
  echo "<click>$ROOT/monitor.sh -n</click>"
  echo "<tool>Failed: $failed; Passed: $passed</tool>"
  echo "<img>$ROOT/icons/$icon$IMAGE_SIZE.png</img>"
fi
) 200> "$LOCK"
