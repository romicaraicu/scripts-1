#!/usr/bin/env bash

command -v docker >/dev/null 2>&1 || { echo >&2 "[docker] is required, but not installed.  Aborting."; exit 1; }
command -v awk >/dev/null 2>&1 || { echo >&2 "[awk] is required, but not installed.  Aborting."; exit 1; }
command -v notify-send >/dev/null 2>&1 || { echo >&2 "[notify-send] is required, but not installed.  Aborting."; exit 1; }
command -v printf >/dev/null 2>&1 || { echo >&2 "[printf] is required, but not installed.  Aborting."; exit 1; }
command -v curl >/dev/null 2>&1 || { echo >&2 "[curl] is required, but not installed.  Aborting."; exit 1; }
command -v readlink >/dev/null 2>&1 || { echo >&2 "[readlink] is required, but not installed.  Aborting."; exit 1; }
command -v jq >/dev/null 2>&1 || { echo >&2 "[jq] is required, but not installed.  Aborting."; exit 1; }

if [[ "$#" -lt 4 ]]; then
  echo "Expected arguments are missing: DOCKER_HOST SERVICES RABBITMQ_MGMT_PORT RABBITMQ_CREDENTIALS [FLAG]"
  echo ""
  echo "NOTE: SERVICES is a comma-delimited partial names of services,"
  echo "that expect to have one or more alive connections to RabbitMQ."
  echo ""
  echo "NOTE: [FLAG] is only used by Generic Monitor panel plugin in Xfce4."
  echo ""
  echo "Example:"
  echo "  ./monitor-with-rabbit.sh 127.0.0.1:2375 service,api 15672 guest:guest"
  exit 1
fi

export DOCKER_HOST=$1
IFS=',' read -r -a SERVICES <<< "$2"
RABBITMQ_MGMT_PORT=$3
RABBITMQ_CREDENTIALS=$4
FLAG=$5
IMAGE_SIZE=24
NOTIFY_TIME=$((5 * 1000))
DIR="$(dirname "$(readlink -f "$0")")"

HOST=$(echo "$DOCKER_HOST" | sed 's/:.*//')

FAILING_SERVICES_STATE="$DIR/$HOST-failing-services"
RABBITMQ_CONNECTIONS_STATE="$DIR/$HOST-rabbitmq-connections"
LOCK="$DIR/$HOST-lock"

function xnotify_healthy () {
  notify-send -t $NOTIFY_TIME -i "$DIR/icons/healthy.png" "$1" "$2"
}

function xnotify_sick () {
  notify-send -t $NOTIFY_TIME -i "$DIR/icons/sick.png" "$1" "$2"
}

function print-not-running-services () {
  echo "" > $FAILING_SERVICES_STATE
  for svc in "${SERVICES[@]}"; do
    lines=($(docker service ls | grep -v '^ID' | grep "$svc" | awk '{print $2"/"$4}'))
    for line in "${lines[@]}"; do
      IFS='/' read -r -a xs <<< "$line"
      name="${xs[0]}"
      m="${xs[1]}"
      n="${xs[2]}"
      if [ $n -ne $m ]; then
        echo "$name ($m / $n)" >> $FAILING_SERVICES_STATE
      fi
    done
  done
}

function test-rabbitmq-connections () {
  #curl -sb -i -u $RABBITMQ_CREDENTIALS "http://$HOST:$RABBITMQ_MGMT_PORT/api/channels" | jq '.[] | .name'
  echo 1
}

( flock -x 200
if [ "$FLAG" == "clicked" ]; then
  failing_services=$(cat $FAILING_SERVICES_STATE 2> /dev/null)
  if [ ! -z "$failing_services" ]; then
    printf "Some services are not running:\n$failing_services" | vim -g -
    xnotify_sick "Some services are not running" "$failing_services"
  else
    xnotify_healthy "Healthy" "System seams to be fully operational!"
  fi
else
  echo "<click>$DIR/monitor-with-rabbit.sh $DOCKER_HOST $RABBITMQ_MGMT_PORT $3 clicked</click>"
  echo "<tool>Monitoring $HOST</tool>"
  print-not-running-services
  failing_services=$(cat $FAILING_SERVICES_STATE 2> /dev/null)
  if [ ! -z "$failing_services" ]; then
    echo "<img>$DIR/icons/sick$IMAGE_SIZE.png</img>"
  else
    echo "<img>$DIR/icons/healthy$IMAGE_SIZE.png</img>"
  fi
fi
) 200> "$LOCK"
