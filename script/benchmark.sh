#!/bin/sh -e

root="$(git rev-parse --show-toplevel)"

scripts()
{
  echo ack
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
  printf " \tMeevax\tGauche\tChibi\n"

  for each in $(scripts)
  do
    time_m=$(real meevax       "$root/benchmark/$each.ss")
    time_g=$(real gosh         "$root/benchmark/$each.ss")
    time_c=$(real chibi-scheme "$root/benchmark/$each.ss")

    printf "%s\t%s\t%s\t%s\n" \
      "$each" \
      "$time_m" \
      "$time_g ($(quotient "$time_m" "$time_g"))" \
      "$time_c ($(quotient "$time_m" "$time_c"))"
  done
}

tsv | column -t -s"$(printf '\t')"
