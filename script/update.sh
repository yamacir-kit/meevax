#!/bin/sh -eu

# ./script/update.sh -DCMAKE_BUILD_TYPE=Release -DCMAKE_CXX_COMPILER=g++

root="$(git rev-parse --show-toplevel)"

build()
{
  rm -rf "$1/build"
  cmake -B "$1/build" -S "$@"
  cmake --build "$1/build" --target develop
}

echo "0.4.$(($(git rev-list --no-merges --count HEAD) - 2854))" > "$root/VERSION"

if dpkg --status meevax
then
  sudo dpkg --purge meevax
fi

build "$root" "$@"

build "$root/example" "$@"
