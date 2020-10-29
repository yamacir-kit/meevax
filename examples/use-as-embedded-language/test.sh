#!/bin/sh -eu

root="$(git rev-parse --show-toplevel)"

here="$root/examples/use-as-embedded-language"

if test -e "$here/build"
then
  rm -rf "$here/build"
fi

mkdir -p "$here/build"

cd "$here/build"

cmake .. "$@"

make

valgrind --error-exitcode=1 \
         --leak-check=full \
         --quiet \
         --show-leak-kinds=all \
         "$here/build/unit-test" --report_level=detailed

# ctest --verbose
