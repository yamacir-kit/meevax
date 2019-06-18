#!/bin/sh -eu

project="$(cd "$(dirname $0)"; pwd)" # script path

for each in "$@"
do
  case "$@" in
    "-b" | "--build" )
      sh $project/build.sh
      break;;
  esac
done

build="$project/build"

if test ! -e $build
then
  sh $project/build.sh
fi

script="$project/test/test.scm"

# sudo perf record -- $build/meevax < $script
sudo perf stat -- $build/meevax < $script
# sudo perf report


# T='std::shared_ptr<meevax::system::pair>'
#
# gprof $build/meevax \
#   | grep -e "$T::shared_ptr(\|$T::operator=(" \
#   | grep -v '\[' \
#   | sed "s/$T:://g" \
#   | sed "s/$T/object/g"
#
# gprof $build/meevax | $build/gprof2dot.py -w | dot -Tpng -o $build/output.png
# eog $build/output.png

