#!/usr/bin/env bash

echo -n "Configuring docker service ... "
echo "OPTIONS=--dns 8.8.8.8 --dns 8.8.4.4 -H tcp://0.0.0.0:2375 -H unix:///var/run/docker.sock --insecure-registry docker-hub:5000 --insecure-registry docker-hub:5043 --log-opt max-size=10M --log-opt max-file=1" > /etc/sysconfig/docker
cat /lib/systemd/system/docker.service | \
  sed 's/^EnvironmentFile.*//' | \
  sed 's/^ExecStart=.*/EnvironmentFile=-\/etc\/sysconfig\/docker\nExecStart=\/usr\/bin\/dockerd $OPTIONS/' \
  > /lib/systemd/system/docker.service.new
mv /lib/systemd/system/docker.service /lib/systemd/system/docker.service.bak
mv -f /lib/systemd/system/docker.service.new /lib/systemd/system/docker.service
echo "done."

systemctl disable docker

echo -n "Stoping docker service ... "
systemctl daemon-reload
systemctl stop docker
echo "done."

echo -n "Cleaning docker files ... "
if [ -d "/var/lib/docker" ]; then
  rm -rf /var/lib/docker/*
fi
if [ ! -d "/var/run/netns" ]; then
  mkdir -p /var/run/netns
fi
if [ -d "/var/run/docker/netns" ]; then
  num_files=$(ls -l /var/run/docker/netns/ | wc -l)
  if [[ $num_files -gt 1 ]]; then
    for p in $(find /var/run/docker/netns/* -maxdepth 1 -print); do
      ln -s $p /var/run/netns
    done
    echo ""
    ip netns list
    for p in $(find /var/run/docker/netns/* -maxdepth 1 -print); do
      ip netns del $(basename $p)
    done
  fi
fi
if [ -d "/var/run/docker" ]; then
  rm -rf /var/run/docker/*
fi
echo "done."

systemctl enable docker

echo -n "Starting docker service ... "
systemctl start docker
echo "done."

x=$(ls -la /var/run/docker.sock | wc -l)
if [ "$x" == "1" ]; then
  echo -n "Opening up /var/run/docker.sock"
  chmod 0666 /var/run/docker.sock
  echo "done."
fi
