#!/usr/bin/env bash

node_id=$(docker node ls 2>&1 | grep '*' | awk '{print $1}')
in_cluster=$(if [ -z "$node_id" ]; then echo 0; else echo 1; fi)

if [[ $in_cluster -eq 0 ]]; then
  echo "Node is not part of any cluster."
fi

if [[ $in_cluster -eq 1 ]]; then
  other_nodes=$(docker node ls 2>&1 | grep -v $node_id | grep -v '^ID' | awk '{print $1}')
  other_nodes_exist=$(if [ -z "$other_nodes" ]; then echo 0; else echo 1; fi)
  if [[ $has_other_nodes -eq 0 ]]; then
    echo "No other nodes left in the cluster."
  else
    for other_node in "${other_nodes[@]}"; do
      docker node demote $other_node 2>&1
      docker node rm $other_node 2>&1
      echo "Demoted and removed: $other_node"
    done
  fi
  docker swarm leave --force 2>&1
  echo "Left cluster."
fi
