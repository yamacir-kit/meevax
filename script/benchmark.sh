#!/bin/sh -e

root="$(git rev-parse --show-toplevel)"

scripts()
{
  echo ack
}

real()
{
  (time -p "$@") 2>&1 | grep -e 'real' | sed -e 's/real\s//'
}

ratio()
{
  time_meevax=$(real meevax "$@")
  time_gauche=$(real gosh   "$@")

  echo "scale = 1; $time_meevax / $time_gauche" | bc -s
}

for each in $(scripts)
do
  printf "%s\t%s\n" "$each" "$(ratio "$root/benchmark/$each.ss")"
done
