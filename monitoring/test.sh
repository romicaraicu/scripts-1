#!/usr/bin/env bash

command -v find >/dev/null 2>&1 || { echo >&2 "[find] is required, but not installed.  Aborting."; exit 1; }
command -v printf >/dev/null 2>&1 || { echo >&2 "[printf] is required, but not installed.  Aborting."; exit 1; }

ROOT="$(dirname "$(readlink -f "$0")")"

RED='\033[1;31m'
YELLOW='\033[1;33m'
GREEN='\033[1;32m'
CYAN='\033[0;36m'
NC='\033[0m'

failed_checks=0
warned_checks=0
passed_checks=0

echo ""
checks=($(find "$ROOT/checks" -type f -iregex ".*\.sh$"))
offset=$((${#ROOT} + 8))
for check in "${checks[@]}"; do
  echo -e "# ${CYAN}${check:$offset}${NC} ..."
  $check
  rc=$?
  if [ "$rc" -eq "0" ]; then
    passed_checks=$(($passed_checks + 1))
    echo -e " >> ${GREEN}PASSED${NC}"
  elif [ "$rc" -eq "1" ]; then
    failed_checks=$(($failed_checks + 1))
    echo -e " >> ${RED}FAILED${NC}"
  else
    warned_checks=$(($warned_checks + 1))
    echo -e " >> ${YELLOW}WARNED${NC}"
  fi
  echo ""
done

echo -e "${GRAY}Failed:${NC} ${RED}$failed_checks${NC}"
echo -e "${GRAY}Warned:${NC} ${YELLOW}$warned_checks${NC}"
echo -e "${GRAY}Passed:${NC} ${GREEN}$passed_checks${NC}"
