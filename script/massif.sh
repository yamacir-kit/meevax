#!/bin/sh -e

stem="/tmp/massif"

valgrind --tool=massif \
         --massif-out-file=$stem.out \
         --quiet \
         --stacks=yes \
         --time-unit=B \
         "$@"

massif-visualizer $stem.out
