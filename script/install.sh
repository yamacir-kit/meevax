#!/bin/sh -e

root="$(git rev-parse --show-toplevel)"

git fetch origin --tags

git tag --list | sed -e 's/^/  /'

echo "\e[32m* v$("$root"/script/version.sh)\e[0m"

build_and_test()
{
  rm -rf "$1"
  cmake -B "$1" -S "$(dirname "$1")" -DCMAKE_BUILD_TYPE=Release -DCMAKE_CXX_COMPILER=g++
  cmake --build "$1" --target all+
}

# ---- Phase 1 -----------------------------------------------------------------

sudo dpkg -r meevax

build_and_test "$root/build"

sudo dpkg -i "$root/build/meevax_$(cat "$root"/VERSION)_amd64.deb"

# ---- Phase 2 -----------------------------------------------------------------

rm -rf "$root/example/build"

cmake -B "$root/example/build" -S "$root/example" -DCMAKE_BUILD_TYPE=Release -DCMAKE_CXX_COMPILER=g++
cmake --build "$root/example/build" --parallel "$(nproc)"
cmake --build "$root/example/build" --parallel "$(nproc)" --target test -- ARGS=-j"$(nproc)"
