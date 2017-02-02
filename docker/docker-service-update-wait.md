# docker-service-update-wait

## The issue

Docker Swarm mode is an excellent tool that recently became available to us
developers as well as operations (DevOps).  `docker service update` is an
invaluable asset to everyone trying to integrate Docker with their CI/CD setup.
But rather soon you would encounter small annoying issue related to it - it is
completely asynchronous, i.e. after verifying validity of input parameters it
will simply send off the request to do the job to the docker host (swarm
manager) and returns to the user saying that everything went well. In reality
service update could have failed miserably. As you suspect this is not how it
should work in CI/CD space.

Here I will try to suggest one way that will help to overcome this issue.

## Assumptions

Solution that will be suggested later is based on a few assumptions that is
important to understand before applying it in your environment.

* `docker service ps <service_name>` produces output where old containers are
  distinguished from new ones with `\_ <service_name>` title. As you can imagine
  that if/when Docker team will decide to change this format, this approach will
  simply fall apart and will require some love.
* in case when within `stability_delay` period of time there is nothing
  happening to the service, script will assume that update will not happen as it
  is not necessary (all parameters are exactly the same)

On the other hand, solution does not assume following:

* Number of replicas - instead script fetches current number of replicas from
  swarm manager and acts accordingly.
* Update command - it is simply supplied as an input, meaning that users can
  still run any variations of `docker service update` with any parameters they
  like.

## Solution

Following is a very simple wrapping bash script that is doing a little magic to
help wait for the feedback from an update process of a service in docker
cluster:

* Script accepts 5 parameters:
    * `SERVICE_NAME` - docker service name to be updated
    * `TIMEOUT_PER_REPLICA_S` - timeout per replica in seconds
    * `STABILITY_DELAY_S` - after all replicas are updated script will wait some
      seconds and then test status of an update again to ensure that no more
      replicas are getting updated (normally the sign that new version is simply
      unstable and crashes, letting docker swarm to create new instances all the
      time)
    * `ERROR_SLIP` - number of failing containers per replica that is allowed to
      still consider service created
    * `UPDATE_CMD` - update command to be evaluated by this script, i.e. exactly
      what this script is wrapping (e.g. `docker service update <service_name>
      ...`)

The script fetches service status by running `docker service ps <service_name>`
which gives information about currently running containers as well as old
containers that are not active any longer. Number of old instances of a
container corresponds with the number of times user ran `docker service update`
command on that particular service. But docker will show only last 4 entries.

Before starting to update a service, this script will detect old incarnations of
currently running instances and ignore them, such that it would be possible to
track status by simply counting the number of new incarnations of service
instances. It might sound a little too abstract, so if that is the case please
have a look at the code - it is always better :)

```bash
#!/usr/bin/env bash

if [[ "$#" -ne 5 ]]; then
  echo Expected arguments are missing: \
    SERVICE_NAME \
    TIMEOUT_PER_REPLICA_S \
    STABILITY_DELAY_S \
    ERROR_SLIP \
    UPDATE_CMD
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
```

> The use of `sed` and `awk` is probably not conventional here and one might
> argue that it is possible to use only one. Author ended up using both.

The script is pretty talkative and will produce a lot of information which is
typically what you want when running it as part of your CI/CD setup to ensure
that troubleshooting process is not a nightmare.

## Usage example

```
./docker-service-update-wait.sh redis-slave 60 2 \
  "docker service update --image redis:3.2.6-alpine --update-delay 5s redis-slave"
```

It will output something like this:

```
     Update command:  docker service update --image redis:3.2.6-alpine --update-delay 5s redis-slave
      # of replicas:  3
    Timeout/replica:  60 sec
      Total timeout:  180 sec
    Stability delay:  2 sec
   Updating service:  redis-slave
 Remaining replicas:  3 2 1 done
     Stability test:  done

SUCCESS
```

In case of a failure script is producing non-zero result code which is what is
typically getting detected by CI/CD setup.

> There is a follow up post regarding `docker service create`
> [here](/posts/ci/docker-service-create-wait.html).
