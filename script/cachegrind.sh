#!/bin/sh -e

stem="/tmp/cachegrind"

valgrind --tool=cachegrind \
         --cachegrind-out-file=$stem.out \
         meevax "$@"

# cg_annotate /tmp/$stem.out
