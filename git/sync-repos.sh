#!/usr/bin/env bash

command -v wc >/dev/null 2>&1 || { echo >&2 "[wc] is required, but not installed.  Aborting."; exit 1; }
command -v ssh-agent >/dev/null 2>&1 || { echo >&2 "[ssh-agent] is required, but not installed.  Aborting."; exit 1; }
command -v ssh-add >/dev/null 2>&1 || { echo >&2 "[ssh-add] is required, but not installed.  Aborting."; exit 1; }
command -v git >/dev/null 2>&1 || { echo >&2 "[git] is required, but not installed.  Aborting."; exit 1; }

if [[ "$#" -ne 1 ]]; then
  echo "Expected arguments are missing: GIT_REPO_PREFIX"
  exit 1
fi

#SCRIPTPATH=$(cd $(dirname $0); pwd -P)
PREFIX=$1

if [[ "$(ssh-add -L | wc -c)" -lt 100 ]]; then
  eval $(ssh-agent)
  ssh-add
fi

function clone_repo() {
  git clone $1
}

function update_repo() {
  name=$1
  pushd ./ > /dev/null
  cd $name
  git pull
  popd > /dev/null
}

while read p; do
  url="$PREFIX/$p.git"
  if [ -d $p ]; then
    echo "... Updating $url ..."
    update_repo $p
  else
    echo "... Cloning $url ..."
    clone_repo $url
  fi
done
