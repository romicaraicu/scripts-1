#!/usr/bin/env bash

if [[ "$#" -ne 1 ]]; then
  echo Expected argument is missing: SWARN_MANAGER_HOSTNAME
  exit 1
fi

export DOCKER_HOST=$1
line=$(docker node ls -q | xargs -L1 docker node inspect | grep -i addr | grep -o "[0-9]*\.[0-9]*\.[0-9]*\.[0-9]*" | sed -e :a -e '$!N; s/\n/ /; ta')

# declare -a hosts=("server1"
#                   "server2"
#                   "server3"
#                   )
IFS=' ' read -r -a hosts <<< "$line"
echo "Swarm cluster (through $1):"
for host in "${hosts[@]}"; do
  echo "  * $host"
done
echo ""

for host in "${hosts[@]}"; do
  echo ">> Cleaning up $host ..."
  export DOCKER_HOST=$host
  if [ "$(docker service ls | wc -l)" -ne "1" ]; then
    echo -n "     * Services:          "
    docker service rm $(docker service ls -q) > /dev/null
    echo "done"
  fi
  if [ "$(docker ps -a | wc -l)" -ne "1" ]; then
    echo -n "     * Containers:          "
    docker rm -f $(docker ps -a -q) > /dev/null
    echo "done"
  fi
  if [ "$(docker images | wc -l)" -ne "1" ]; then
    echo -n "     * Images:          "
    docker rmi -f $(docker images -q) > /dev/null
    echo "done"
  fi
done
