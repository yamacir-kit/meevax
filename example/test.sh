#!/bin/sh -eu

# THIS SCRIPT IS DEPRECATED

here="$(git rev-parse --show-toplevel)/example"

if test -e "$here/build"
then
  rm -rf "$here/build"
fi

cmake -B "$here/build" -S "$here" "$@"

cd "$here/build"

make -j"$(nproc)"

make test ARGS=-j"$(nproc)"

check()
{
  valgrind --error-exitcode=1 \
           --leak-check=full \
           --quiet \
           --show-leak-kinds=all \
  "$@" --build_info=yes \
       --catch_system_error=no \
       --color_output=yes \
       --report_level=short \
       --show_progress=yes
}

# check "$here/build/test-collector"
# check "$here/build/test-list"
# check "$here/build/test-r7rs"
