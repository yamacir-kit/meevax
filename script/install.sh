#!/bin/sh -e

root="$(git rev-parse --show-toplevel)"

make()
{
  rm -rf "$2"
  cmake -B "$2" -S "$(dirname "$2")" -DCMAKE_BUILD_TYPE=Release -DCMAKE_CXX_COMPILER=clang++
  cmake --build "$2" --target "$1"
}

if dpkg -s meevax
then
  sudo apt remove --yes meevax
fi

make safe-install.deb "$root/build"

make demo "$root/example/build"
