#!/bin/sh -e

root="$(git rev-parse --show-toplevel)"

git fetch origin --tags

git tag --list | sed -e 's/^/  /'

echo "\e[32m* v$("$root"/script/version.sh)\e[0m"

# ---- Phase 1 -----------------------------------------------------------------

sudo apt remove meevax

rm -rf "$root/build"

cmake -B "$root/build" -S "$root" -DCMAKE_BUILD_TYPE=Release -DCMAKE_CXX_COMPILER=g++
cmake --build "$root/build" --parallel "$(nproc)"
cmake --build "$root/build" --parallel "$(nproc)" --target test -- ARGS=-j"$(nproc)"
cmake --build "$root/build" --parallel "$(nproc)" --target package

sudo apt install "$root/build/meevax_$(cat "$root"/VERSION)_amd64.deb"

# ---- Phase 2 -----------------------------------------------------------------

rm -rf "$root/example/build"

cmake -B "$root/example/build" -S "$root/example" -DCMAKE_BUILD_TYPE=Release -DCMAKE_CXX_COMPILER=g++
cmake --build "$root/example/build" --parallel "$(nproc)"
cmake --build "$root/example/build" --parallel "$(nproc)" --target test -- ARGS=-j"$(nproc)"
