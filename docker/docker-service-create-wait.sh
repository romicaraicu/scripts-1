#!/usr/bin/env bash

if [[ "$#" -ne 4 ]]; then
  echo "Expected arguments are missing: SERVICE_NAME STABILITY_DELAY_S ERROR_SLIP CREATE_CMD"
  exit 1
fi

name=$1
stability_delay=$(echo $2 | sed 's/[^0-9]//g')
error_slip=$(echo $3 | sed 's/[^0-9]//g')
create_cmd=$4

function get_new_ids() {
  echo $(docker service ps $name | awk '(NR > 1) {print $1}' | wc -l)
}

echo ""
echo "     Create command:  $create_cmd"
echo "    Stability delay:  $stability_delay sec"
echo "         Error slip:  $error_slip"
echo -n "   Creating service:  "
eval $create_cmd
exit_code=$?
if [ "$exit_code" -ne 0 ]; then
  echo "ERROR: cannot create service"
  exit $exit_code
fi
echo "done"

echo -n "     Stability test:  "
replicas=$(docker service inspect $name | grep -i replicas | sed 's/[^0-9]//g')
max_n=$(($replicas + $replicas * $error_slip))
n=$(get_new_ids)
if [ $n -gt $max_n ]; then
  echo "failed"
  exit 3
fi
sleep $stability_delay
n=$(get_new_ids)
if [ $n -gt $max_n ]; then
  echo "failed"
  exit 3
fi
echo "done"
echo ""

echo "SUCCESS"
