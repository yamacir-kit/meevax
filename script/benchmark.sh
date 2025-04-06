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
  (command time -p "$@") 2>&1 | grep -e 'real' | sed -e 's/real[ ]*//'
}

quotient()
{
  echo "scale = 1; $1 / $2" | bc -s
}

tsv()
{
  printf "\tMeevax\tChibi-Scheme\tGauche\n"

  for script in $(scripts)
  do
    t0=$(real meevax       "$root/benchmark/$script.ss")
    t1=$(real chibi-scheme "$root/benchmark/$script.ss")
    t2=$(real gosh         "$root/benchmark/$script.ss")

    printf "%s\t%s\t%s\t%s\n" \
      "$script" \
      "$t0" \
      "$t1 (x$(quotient "$t0" "$t1"))" \
      "$t2 (x$(quotient "$t0" "$t2"))"
  done
}

tsv | column -t -s "$(printf '\t')"
