#!/usr/bin/env bash

if [[ "$#" -ne 5 ]]; then
  echo Expected arguments are missing: SERVICE_NAME TIMEOUT_PER_REPLICA_S STABILITY_DELAY_S ERROR_SLIP UPDATE_CMD
  exit 1
fi

name=$1
timeout_per_replica=$(echo $2 | sed 's/[^0-9]//g')
stability_delay=$(echo $3 | sed 's/[^0-9]//g')
error_slip=$(echo $4 | sed 's/[^0-9]//g')
update_cmd=$5
delay=3

# find all currently visible containers (running and shut down) to ignore while inspecting status changes
ignore_ids=$(docker service ps $name | awk '(NR > 1) {print "|"$1}' | xargs echo | awk '{gsub(/ /, ""); print}')
ignore_ids=${ignore_ids#|*}
function get_new_running_ids() {
  # all new ids (failed + running)
  i=$(docker service ps $name | awk -v ids="$ignore_ids" '(NR > 1) && ($1 !~ ids) {print $1}' | wc -l)
  # all new ids (failed only)
  j=$(docker service ps $name | grep "_ $name" | awk -v ids="$ignore_ids" '(NR > 1) && ($1 !~ ids) {print $1}' | wc -l)
  echo $(($i - $j))
}
function get_new_ids() {
  # all new ids (failed + running)
  echo $(docker service ps $name | awk -v ids="$ignore_ids" '(NR > 1) && ($1 !~ ids) {print $1}' | wc -l)
}

replicas=$(docker service inspect $name | grep -i replicas | sed 's/[^0-9]//g')
timeout=$(($timeout_per_replica * $replicas))

echo ""
echo "     Update command:  $update_cmd"
echo "      # of replicas:  $replicas"
echo "    Timeout/replica:  $timeout_per_replica sec"
echo "      Total timeout:  $timeout sec"
echo "    Stability delay:  $stability_delay sec"
echo "         Error slip:  $error_slip"
echo -n "   Updating service:  "
eval $update_cmd
exit_code=$?
if [ "$exit_code" -ne 0 ]; then
  echo "ERROR: cannot update service"
  exit $exit_code
fi

# test that there are at least $(($replicas+$error_slip)) more containers after update
m=0
x=0
echo -n " Remaining replicas:  "
while [ "$m" -lt "$replicas" ]; do
  i=$(get_new_running_ids)
  if [ "$i" -gt "$m" ]; then
    echo -n "$(($replicas-$i+1)) "
  fi
  m=$(get_new_running_ids)
  if [ "$(($x * $delay))" -gt "$stability_delay" ]; then
    if [ "$m" -eq "0" ]; then
      echo "SKIPPED: it seems that there is nothing to update"
      exit 0
    fi
  fi
  if [ "$(($x * $delay))" -gt "$timeout" ]; then
    echo "ERROR: update is taking too long time (timeout reached)"
    exit 2
  fi
  x=$(($x+1))
  sleep $delay
done
echo "done"

echo -n "     Stability test:  "
max_n=$(($replicas + $replicas * $error_slip))
k=$(get_new_ids)
if [ "$k" -gt "$max_n" ]; then
  echo "failed"
  exit 3
fi
sleep $stability_delay
k=$(get_new_ids)
if [ "$k" -gt "$max_n" ]; then
  echo "failed"
  exit 3
fi
echo "done"
echo ""

echo "SUCCESS"
