#!/bin/sh -eu

script=$(cd "$(dirname $0)"; pwd) # script path

build="$(git rev-parse --show-toplevel)/build"

mkdir -vp $build
cd $build

if test -e $build/Makefile
then
  make clean
fi

cmake .. -DCMAKE_BUILD_TYPE=debug
make

