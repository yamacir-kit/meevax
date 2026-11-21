#!/bin/sh -eu

# ./script/update.sh -DCMAKE_BUILD_TYPE=Release -DCMAKE_CXX_COMPILER=g++

root="$(git rev-parse --show-toplevel)"

build()
{
  rm -rf "$1/build"
  cmake -B "$1/build" -S "$@"
  cmake --build "$1/build" --target continuous-integration
}

echo "0.6.$(($(git rev-list --first-parent --merges --count master) - 203))" > "$root/VERSION"

build "$root" "$@"

build "$root/example" "$@"
