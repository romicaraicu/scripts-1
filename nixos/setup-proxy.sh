#!/usr/bin/env bash

if [ "$EUID" -ne 0 ]; then
  echo "Please run as root"
  exit
fi

ROOT=/etc/nixos

RED='\033[1;31m'
GREEN='\033[1;32m'
NC='\033[0m'

echo ""
echo -e "${RED}## ${GREEN}Configuring proxy${NC}"
echo ""
echo -n -e "${GREEN}Domain:${NC}   "
read domain
echo -n -e "${GREEN}Username:${NC} "
read user
echo -n -e "${GREEN}Password:${NC} "
read -s password
echo ""

sed -i "s/username = \".*\";/username = \"$user\";/" "$ROOT/networking.nix"
sed -i "s/password = \".*\";/password = \"$password\";/" "$ROOT/networking.nix"
sed -i "s/domain = \".*\";/domain = \"$domain\";/" "$ROOT/networking.nix"

echo ""
echo -e "${RED}## ${GREEN}Applying configuration${NC}"
echo ""

nixos-rebuild switch
exit_code=$?
if [ "$exit_code" -ne 0 ]; then
  echo "ERROR: cannot apply configuration"
  exit $exit_code
fi
nix-collect-garbage -d
