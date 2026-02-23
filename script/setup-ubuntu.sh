#!/bin/sh -e

essential()
{
  echo build-essential
  echo libgmp-dev
}

optional()
{
  echo shellcheck # GitHub Actions
  echo valgrind
}

sudo apt update

if test "$#" -eq 0
then
  required | xargs sudo apt install
else
  for each in "$@"
  do
    case "$each" in
      -a | --all       ) ( essential && optional ) | xargs sudo apt install --yes ;;
      -e | --essential ) ( essential             ) | xargs sudo apt install --yes ;;
      -o | --optional  ) (              optional ) | xargs sudo apt install --yes ;;
    esac
  done
fi
