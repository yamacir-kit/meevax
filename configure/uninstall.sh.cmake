#!/bin/sh -eu

script=$(cd "$(dirname $0)"; pwd) # script path

install_prefix='${CMAKE_INSTALL_PREFIX}'

if test -e $install_prefix/bin/meevax
then
  echo "sudo rm $install_prefix/bin/meevax"
        sudo rm $install_prefix/bin/meevax
fi

