#!/usr/bin/env bash

advertise_ip=$1

echo -n "Initializing swarm cluster ... "
docker swarm init --advertise-addr="$advertise_ip:2377" 2>&1 > /dev/null
echo "done."
