#!/bin/sh -eu

here="$(git rev-parse --show-toplevel)/example"

if test -e "$here/build"
then
  rm -rf "$here/build"
fi

cmake -B "$here/build" -S "$here" "$@"

cd "$here/build"

make -j"$(nproc)"

check()
{
  valgrind --error-exitcode=1 --leak-check=full --quiet --show-leak-kinds=all "$@"
}

check "$here/build/test-gc"   --report_level=detailed
check "$here/build/test-list" --report_level=detailed
check "$here/build/test-r7rs" --report_level=detailed

# ctest --verbose
