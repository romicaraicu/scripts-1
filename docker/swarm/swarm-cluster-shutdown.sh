#!/usr/bin/env bash

command -v base64 >/dev/null 2>&1 || { echo >&2 "[base64] is required, but not installed.  Aborting."; exit 1; }

YELLOW='\033[1;33m'
CYAN='\033[0;36m'
NC='\033[0m'

if [[ "$#" -ne 3 ]]; then
  echo "Expected arguments are missing: USER_NAME DOMAIN_NAME SERVERS"
  echo ""
  echo "Example:"
  echo -e "  ${CYAN}./swarm-cluster-shutdown.sh${YELLOW} user domain host1,host2,host3,host4,host5${NC}"
  exit 1
fi

user_name=$1
domain_name=$2
IFS=',' read -r -a servers <<< "$3"

function shutdown_node () {
  server=$1
  echo ""
  echo -e "${YELLOW}$server${NC}"
  command=$(base64 -w0 shutdown.sh)
  ssh -t $server "echo $command | base64 -d | sudo su"
}

for server in "${servers[@]}"; do
  shutdown_node "$user_name@$server.$domain_name"
done
