#!/bin/sh -eu

project=$(cd "$(dirname $0)"; pwd) # script path

for each in "$@"
do
  case "$@" in
    "--rebuild" )
      sh $project/build.sh
      break;;
  esac
done

build="$project/build"

if test ! -e $build
then
  sh $project/build.sh
fi

tests="$project/tests"
script="$tests/test.scm"

$build/meevax < $script #| sed 's/\x1b\[[0-9;]*[a-zA-Z]//g' | tee $script.result

gprof $build/meevax | grep -e "std::shared_ptr<meevax::system::pair>::shared_ptr(\|std::shared_ptr<meevax::system::pair>::operator=(" | grep -v "\["

# gprof $build/meevax | $build/gprof2dot.py -w | dot -Tpng -o $build/output.png
# eog $build/output.png

