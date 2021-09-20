#!/bin/sh -e


export RLWRAP_EDITOR='vim -c "set filetype=scheme"'

rlwrap --break-chars "(){}[].,;#@|'\`\"" \
       --complete-filenames \
       --multi-line \
       --quote-characters "\"" \
       meevax -i "$@"
