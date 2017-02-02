# docker-service-create-wait

Following is a very simple wrapping bash script that is doing a little magic to
help wait for the feedback from an update process of a service in docker
cluster:

* Script accepts 4 parameters:
    * `SERVICE_NAME` - docker service name to be created
    * `STABILITY_DELAY_S` - after all replicas are created script will wait some
      seconds and then test status of an update again to ensure that no more
      replicas are getting created (normally the sign that new version is simply
      unstable and crashes, letting docker swarm to create new instances all the
      time)
    * `ERROR_SLIP` - number of failing containers per replica that is allowed to
      still consider service created
    * `CREATE_CMD` - update command to be evaluated by this script, i.e. exactly
      what this script is wrapping (e.g. `docker service create <service_name>
      ...`)

After creating service, the script fetches service status by running `docker
service ps <service_name>` which gives information about currently running
containers. There should be no old instances of a service registered, otherwise
`docker service create` will fail saying that service has already been created.
But the number of old instances of a service can suddenly go up in cases when
installed service is not stable and crashes making swarm to re-create it again
and again.

```bash
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
```

The script is also quite talkative and will produce a lot of information which
is typically what you want when running it as part of your CI/CD setup to ensure
that troubleshooting process is not a nightmare.

## Usage example

```
./docker-service-create-wait.sh redis-slave 60 1 \
  "docker service create --name redis-slave redis:3.2.6-alpine"
```

It will output something like this:

```
     Create command:  docker service create --name redis-slave redis:3.2.6-alpine
    Stability delay:  120 sec
         Error slip:  1
   Creating service:  8oz8ldtxoju18v4ds1g6ith6i
done
     Stability test:  done

SUCCESS
```

In case of a failure script is producing non-zero result code which is what is
typically getting detected by CI/CD setup.
