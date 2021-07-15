#!/bin/sh -e

stem="/tmp/callgrind"

valgrind --tool=callgrind \
         --callgrind-out-file=$stem.out \
         --quiet \
         meevax "$@"

kcachegrind $stem.out
