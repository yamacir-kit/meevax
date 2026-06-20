#!/bin/sh -e

stem="/tmp/callgrind"

valgrind --tool=callgrind \
         --branch-sim=yes \
         --cache-sim=yes \
         --callgrind-out-file=$stem.out \
         --dump-instr=yes \
         --quiet \
         "$@"

kcachegrind $stem.out
