#!/usr/bin/env bash

command -v base64 >/dev/null 2>&1 || { echo >&2 "[base64] is required, but not installed.  Aborting."; exit 1; }

YELLOW='\033[1;33m'
CYAN='\033[0;36m'
NC='\033[0m'

if [[ "$#" -ne 4 ]]; then
  echo "Expected arguments are missing: USER_NAME DOMAIN_NAME MANAGERS WORKERS"
  echo ""
  echo "Example:"
  echo -e "  ${CYAN}./swarm-cluster-startup.sh${YELLOW} user domain host1,host2,host3 host4,hostt5${NC}"
  exit 1
fi

user_name=$1
domain_name=$2
IFS=',' read -r -a managers <<< "$3"
IFS=',' read -r -a workers <<< "$4"

root=$(dirname $0)

function init_cluster () {
  server=$1
  ip=$2
  echo ""
  echo -e "Init cluster @ ${YELLOW}$server${NC}"
  command=$(cat $root/startup.sh | sed "s/advertise_ip=\$1/advertise_ip=\"$ip\"/" | base64 -w0)
  ssh -t $server "echo $command | base64 -d | sudo su"
}

advertise_ip=$(ping ${managers[0]} -c 1 | grep '^PING' | awk '{print $3}' | sed 's/(//' | sed 's/)//')
init_cluster "$user_name@${managers[0]}.$domain_name" $advertise_ip

worker_join_cmd=$(docker -H "$advertise_ip:2375" swarm join-token worker | grep '^\s' | sed 's/^\s*//' | sed -e 's/ \\$//' | tr '\n' ' ')
manager_join_cmd=$(docker -H "$advertise_ip:2375" swarm join-token manager | grep '^\s' | sed 's/^\s*//' | sed -e 's/ \\$//' | tr '\n' ' ')

echo ""
echo -e "${CYAN}Managers to:${NC} $manager_join_cmd"
echo -e "${CYAN} Workers to:${NC} $worker_join_cmd"

function join_cluster_as_manager () {
  server=$1
  join_cmd=$2
  echo ""
  echo -e "Join cluster as ${CYAN}manager${NC} @ ${YELLOW}$server${NC}"
  command=$(echo $join_cmd | base64 -w0)
  ssh -t $server "echo $command | base64 -d | sudo su"
}

function join_cluster_as_worker () {
  server=$1
  join_cmd=$2
  echo ""
  echo -e "Join cluster as ${CYAN}worker${NC} @ ${YELLOW}$server${NC}"
  command=$(echo $join_cmd | base64 -w0)
  ssh -t $server "echo $command | base64 -d | sudo su"
}

for manager in "${managers[@]}"; do
  if [[ "$manager" != "${managers[0]}" ]]; then
    join_cluster_as_manager "$user_name@$manager.$domain_name" "$manager_join_cmd"
  fi
done

for worker in "${workers[@]}"; do
  join_cluster_as_worker "$user_name@$worker.$domain_name" "$worker_join_cmd"
done

echo ""
docker -H "$advertise_ip:2375" node ls

echo ""
echo -e "${YELLOW}done!${NC}"
