#!/bin/sh -e

stem="/tmp/massif"

valgrind --tool=massif \
         --massif-out-file=$stem.out \
         --quiet \
         --stacks=yes \
         --time-unit=B \
         meevax "$@"

massif-visualizer $stem.out
