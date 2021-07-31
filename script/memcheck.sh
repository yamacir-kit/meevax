#!/bin/sh -e

valgrind --tool=memcheck \
         --error-exitcode=1 \
         --leak-check=full \
         --quiet \
         --show-leak-kinds=all \
         meevax "$@"
