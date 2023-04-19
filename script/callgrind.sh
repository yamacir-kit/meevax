#!/bin/sh -e

stem="/tmp/callgrind"

valgrind --tool=callgrind \
         --callgrind-out-file=$stem.out \
         --dump-instr=yes \
         --quiet \
         meevax "$@"

kcachegrind $stem.out
