#!/usr/bin/env bash

command -v base64 >/dev/null 2>&1 || { echo >&2 "[base64] is required, but not installed.  Aborting."; exit 1; }

YELLOW='\033[1;33m'
CYAN='\033[0;36m'
NC='\033[0m'

if [[ "$#" -ne 2 ]]; then
  echo ""
  echo -e "Expected arguments are missing: ${YELLOW}DOMAIN_NAME MANAGER_NODE${NC}"
  echo -e "Command also expects ${YELLOW}LABEL_SPEC${NC} to be supplied through pipe"
  echo ""
  echo "Example:"
  echo -e "  echo 'host1!infra=on+rabbitmq-1=on,host2!app=on' | ./swarm-cluster-label.sh domain"
  exit 1
fi

domain_name=$1
manager_node=$2

function fetch_node_id () {
  domain=$1
  manager=$2
  node=$3
  echo $(docker -H "$manager.$domain:2375" node ls | grep -v "^ID" | grep -i "$node.$domain" | awk '{ print $1 }')
}

input=""
while read -t 1 line; do
  input="$input$line"
done

IFS=',' read -r -a lines <<< "$input"
for line in "${lines[@]}"; do
  echo ""
  IFS='!' read -r -a xs <<< "$line"
  node=${xs[0]}
  node_id=$(fetch_node_id $domain_name $manager_node $node)
  echo -e "Applying labels to ${YELLOW}$node.$domain_name${NC} [$node_id]:"
  IFS='+' read -r -a labels <<< "${xs[1]}"
  for label in "${labels[@]}"; do
    docker -H "$manager_node.$domain_name:2375" node update --label-add "$label" $node_id 2>&1 > /dev/null
    echo -e "  * ${CYAN}$label${NC}"
  done
done
