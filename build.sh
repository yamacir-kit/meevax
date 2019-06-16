#!/bin/sh -eu

project=$(cd "$(dirname $0)"; pwd) # script path

build="$project/build"

mkdir -vp $build
cd $build

if test -e $build/Makefile
then
  make clean
fi

cmake .. -DCMAKE_BUILD_TYPE=debug
make

