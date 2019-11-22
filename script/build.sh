#!/bin/sh -eu

# script=$(cd "$(dirname $0)"; pwd) # script path

build="$(git rev-parse --show-toplevel)/build"

mkdir -vp "$build"
cd "$build"

if test -e "$build/Makefile"
then
  make clean
fi

# type="Debug"
type="Release"

cmake .. -DCMAKE_BUILD_TYPE="$type" -DCMAKE_CXX_COMPILER=clang++-6.0
make -j #-j4

make clean

cmake .. -DCMAKE_BUILD_TYPE="$type" -DCMAKE_CXX_COMPILER=g++-7
make -j #-j4

