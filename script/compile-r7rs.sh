#!/bin/sh -e

# SRFI 138 compile-r7rs for Meevax
# This is a minimal implementation that works adequately, but may fail when
# given arguments containing characters that require complex escaping.

[ -n "$COMPILE_R7RS" ] && [ "$0" != "$COMPILE_R7RS" ] && exec "$COMPILE_R7RS" "$@"

while [ 0 -lt $# ]
do
  case $1 in
    -o)
      shift
      output=$1
      ;;
    -o?*)
      output=${1#-o}
      ;;
    *)
      options="$options \"$1\""
      ;;
  esac
  shift
done

[ -z "$output" ] && output=a.out

{
  echo '#!/bin/sh -e'
  echo "exec meevax $options \"\$@\""
} > "$output"

chmod +x "$output"
