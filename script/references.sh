#!/bin/sh

convert()
{
  bibtex2html -nodoc \
              -nofooter \
              -noheader \
              -nolinks \
              -o - \
              -q \
              "$(git rev-parse --show-toplevel)/configure/references.bib"
}

filter()
{
  grep -v -e '<table>' \
          -e '</table>' \
          -e '<tr[^>]*>' \
          -e '</tr>' \
          -e '<td[^>]*>' \
          -e '</td>'
}

convert | filter | sed -e 's/&nbsp;/ /g'
