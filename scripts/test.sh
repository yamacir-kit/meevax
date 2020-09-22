#!/bin/sh

# THIS SCRIPT IS DEPRECATED

root="$(git rev-parse --show-toplevel)"

exit_code=0

size="$(ls "$root/test" | wc -l)"

for each in $root/test/*.ss
do
  # echo "$each"

  name="$(basename "$each" .ss)"

  # "$root/build/bin/ice" --quiet < "$each"
  # "$root/build/bin/ice" --file "$each"

  sh "$each" "$root/build/bin/ice"

  index=$(( index + 1))

  diff "$root/test/$name.a" \
       "$root/test/$name.b"

  if test "$?" -eq 0
  then
    echo "[$index/$size] \e[1;32mpassed\e[0m: $name"
  else
    echo "\e[1;31mfailed\e[0m: $name"
    exit_code=1
  fi
done

exit "$exit_code"

