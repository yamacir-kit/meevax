#!/bin/sh -e

paths=$PATH

if command -v brew >/dev/null
then
  paths=$paths:$(brew --prefix)/bin
fi

name=$(printf '%s\n' "$1" | sed 's/[][\\.^$*+?(){}|]/\\&/g')

latest=$(
  printf '%s\n' "$paths" | tr ':' '\n' | while read -r path
  do # shellcheck disable=SC2012
    test -d "$path" && ls -1 "$path" | sed -En "s#^$name-([0-9]+)\$#\1 $path/&#p"
  done | sort -nr | sed -n '1s/^[0-9]* //p')

if test "$latest"
then
  printf '%s\n' "$latest"
else
  command -v "$1"
fi
