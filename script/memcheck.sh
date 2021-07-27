#!/bin/sh -e

valgrind --tool=memcheck \
         --error-exitcode=1 \
         --leak-check=full \
         --quiet \
         --show-leak-kinds=all \
         "$@" # --build_info=yes \
              # --catch_system_error=no \
              # --color_output=yes \
              # --report_level=short \
              # --show_progress=yes # for Boost.Test
