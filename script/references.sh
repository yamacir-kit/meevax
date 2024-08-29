#!/bin/sh

bibtex2html -nodoc -nofooter -noheader -nolinks -dl -o - -q "$(git rev-parse --show-toplevel)/configure/references.bib" | pandoc -f html -t gfm --wrap=none
