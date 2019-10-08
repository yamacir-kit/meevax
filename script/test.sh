#!/bin/sh -eu

repository="$(git rev-parse --show-toplevel)" # repository root path

for each in "$@"
do
  case "$@" in
    "-b" | "--build" )
      sh $repository/script/build.sh
      break;;
  esac
done

if test ! -e $repository/build
then
  sh $repository/script/build.sh
fi

# sudo time perf record -- $repository/build/bin/meevax < $repository/test/test.scm
# sudo perf report

# perf stat -- $repository/build/bin/meevax --verbose-reader < $repository/test/meta-circular-evaluator.meevax
perf stat -- $repository/build/bin/meevax --verbose < $repository/test/test.scm

# T='std::shared_ptr<meevax::kernel::pair>'
#
# gprof $build/meevax \
#   | grep -e "$T::shared_ptr(\|$T::operator=(" \
#   | grep -v '\[' \
#   | sed "s/$T:://g" \
#   | sed "s/$T/object/g"
#
# gprof $build/meevax | $build/gprof2dot.py -w | dot -Tpng -o $build/output.png
# eog $build/output.png

