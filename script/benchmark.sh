#!/bin/sh -e

root="$(git rev-parse --show-toplevel)"

scripts()
{
  echo ack
  echo fib
  echo tarai
}

real()
{
  (time -p "$@") 2>&1 | grep -e 'real' | sed -e 's/real\s//'
}

quotient()
{
  echo "scale = 1; $1 / $2" | bc -s
}

tsv()
{
  printf "script\tMeevax\tChibi-Scheme\tGauche\n"

  for each in $(scripts)
  do
    t0=$(real meevax       "$root/benchmark/$each.ss")
    t1=$(real chibi-scheme "$root/benchmark/$each.ss")
    t2=$(real gosh         "$root/benchmark/$each.ss")

    printf "%s\t%s\t%s\t%s\n" \
      "$each" \
      "$t0" \
      "$t1 (x$(quotient "$t0" "$t1"))" \
      "$t2 (x$(quotient "$t0" "$t2"))"
  done
}

tsv | column -t -s"$(printf '\t')"
