#!/bin/sh -e

required()
{
  echo build-essential
  echo libgmp-dev
}

optional()
{
  echo kcachegrind
  echo massif-visualizer
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
      -a | --all      ) ( required && optional ) | xargs sudo apt install --yes ;;
      -o | --optional ) (             optional ) | xargs sudo apt install --yes ;;
      -r | --required ) ( required             ) | xargs sudo apt install --yes ;;
    esac
  done
fi
