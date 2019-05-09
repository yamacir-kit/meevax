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
script="$tests/meta-circular-evaluator.scm"

$build/meevax < $script | sed 's/\x1b\[[0-9;]*[a-zA-Z]//g' | tee $script.result

