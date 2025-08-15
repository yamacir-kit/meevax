#!/bin/sh -e

required()
{
  echo gmp
}

optional()
{
  echo gcc
  echo shellcheck # GitHub Actions
}

documentation()
{
  echo bibtex2html # script/references.sh
  echo doxygen
  echo pandoc # script/references.sh
}

brew update

if test "$#" -eq 0
then
  required | xargs brew install
else
  for each in "$@"
  do
    case "$each" in
      -a | --all           ) ( required && optional && documentation ) | xargs brew install ;;
      -d | --documentation ) (                         documentation ) | xargs brew install ;;
      -o | --optional      ) (             optional                  ) | xargs brew install ;;
      -r | --required      ) ( required                              ) | xargs brew install ;;
    esac
  done
fi
