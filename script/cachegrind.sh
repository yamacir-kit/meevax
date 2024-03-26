#!/bin/sh -e

stem="/tmp/cachegrind"

valgrind --tool=cachegrind \
         --cachegrind-out-file=$stem.out \
         "$@"

cg_annotate $stem.out
