#!/bin/sh -e

required()
{
  echo build-essential
  echo libgmp-dev
}

optional()
{
  echo kcachegrind # script/callgrind.sh
  echo massif-visualizer # script/massif.sh
  echo shellcheck # GitHub Actions
  echo valgrind
}

documentation()
{
  wget -q https://github.com/jgm/pandoc/releases/download/3.3/pandoc-3.3-1-amd64.deb -P /tmp

  echo bibtex2html # script/references.sh
  echo doxygen
  echo /tmp/pandoc-3.3-1-amd64.deb # script/references.sh
}

sudo apt update

if test "$#" -eq 0
then
  required | xargs sudo apt install
else
  for each in "$@"
  do
    case "$each" in
      -a | --all           ) ( required && optional && documentation ) | xargs sudo apt install --yes ;;
      -d | --documentation ) (                         documentation ) | xargs sudo apt install --yes ;;
      -o | --optional      ) (             optional                  ) | xargs sudo apt install --yes ;;
      -r | --required      ) ( required                              ) | xargs sudo apt install --yes ;;
    esac
  done
fi
