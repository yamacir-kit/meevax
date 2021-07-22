#!/bin/sh -e

valgrind --error-exitcode=1 \
         --leak-check=full \
         --quiet \
         --show-leak-kinds=all \
         meevax "$@" # --build_info=yes \
                     # --catch_system_error=no \
                     # --color_output=yes \
                     # --report_level=short \
                     # --show_progress=yes # for Boost.Test
