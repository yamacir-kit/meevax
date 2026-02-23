#!/bin/sh -e

essential()
{
  echo gmp
}

optional()
{
  echo gcc
  echo shellcheck # GitHub Actions
}

brew update

if test "$#" -eq 0
then
  required | xargs brew install
else
  for each in "$@"
  do
    case "$each" in
      -a | --all       ) ( essential && optional ) | xargs brew install ;;
      -e | --essential ) ( essential             ) | xargs brew install ;;
      -o | --optional  ) (              optional ) | xargs brew install ;;
    esac
  done
fi
