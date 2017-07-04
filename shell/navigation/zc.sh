#!/usr/bin/env bash

GNAV="$HOME/.gnav"

function zc () {
  rm -f "$GNAV" 2>/dev/null
}

zc
