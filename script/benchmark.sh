#!/bin/sh -e

root="$(git rev-parse --show-toplevel)"

implementations()
{
  echo Meevax
  echo Chibi # sudo apt install chibi-scheme
  echo Guile # sudo apt install guile-3.0
}

scripts()
{
  echo ack
  echo cpstak
  echo ctak
  echo deriv
  echo fib
  echo tak
  echo takl
  echo tarai
}

real()
{
  (command time -p "$@") 2>&1 | grep -e real | tr -s ' ' | cut -d ' ' -f 2
}

run()
{
  case $1 in
    Meevax)  real meevax "$2" ;;
    Chibi)   real chibi-scheme "$2" ;;
    Guile)   real guile "$2" ;;
    *) exit 1 ;;
  esac
}

for implementation in $(implementations)
do
  printf "\t%s" "$implementation"
done

printf "\n"

for script in $(scripts)
do
  printf "%s" "$script"

  for implementation in $(implementations)
  do
    printf "\t%s" "$(run "$implementation" "$root/benchmark/$script.ss")"
  done

  printf "\n"
done

# for script in $(scripts)
# do
#   for implementation in $(implementations)
#   do
#     printf "%s\t%s\t%s\n" "$script" "$implementation" "$(run "$implementation" "$root/benchmark/$script.ss")"
#   done
# done
