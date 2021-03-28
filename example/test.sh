#!/bin/sh -eu

here="$(git rev-parse --show-toplevel)/example"

if test -e "$here/build"
then
  rm -rf "$here/build"
fi

mkdir -p "$here/build"

cd "$here/build"

cmake .. "$@"

make

# valgrind --error-exitcode=1 \
#          --leak-check=full \
#          --quiet \
#          --show-leak-kinds=all \
#          "$here/build/unit-test" --report_level=detailed

"$here/build/unit-test"

# ctest --verbose
