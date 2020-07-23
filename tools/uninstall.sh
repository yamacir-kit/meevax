#!/bin/sh

repository="$(git rev-parse --show-toplevel)"

sudo rm -rfv /usr/local/bin/ice
sudo rm -rfv /usr/local/include/meevax
sudo rm -rfv /usr/local/lib/libmeevax.so*
sudo rm -rfv /usr/local/share/cmake/meevax
