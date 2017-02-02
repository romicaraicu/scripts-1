#!/usr/bin/env bash

command -v wc >/dev/null 2>&1 || { echo >&2 "[wc] is required, but not installed.  Aborting."; exit 1; }
command -v ssh-agent >/dev/null 2>&1 || { echo >&2 "[ssh-agent] is required, but not installed.  Aborting."; exit 1; }
command -v ssh-add >/dev/null 2>&1 || { echo >&2 "[ssh-add] is required, but not installed.  Aborting."; exit 1; }

if [[ "$(ssh-add -L | wc -c)" -lt 100 ]]; then
  eval $(ssh-agent)
  ssh-add
fi
