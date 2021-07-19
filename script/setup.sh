#!/bin/sh -e

required()
{
  echo libboost-all-dev
  echo libgmp-dev
}

optional()
{
  echo kcachegrind
  echo massif-visualizer
  echo valgrind
}

if test "$#" -eq 0
then
  required | xargs sudo apt install
else
  for each in "$@"
  do
    case "$each" in
      -a | --all      ) ( required && optional ) | xargs sudo apt install ;;
      -o | --optional ) (             optional ) | xargs sudo apt install ;;
      -r | --required ) ( required             ) | xargs sudo apt install ;;
      -u | --update   )                                  sudo apt update  ;;
    esac
  done
fi
