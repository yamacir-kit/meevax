# meevax

[![Build Status](https://travis-ci.org/yamacir-kit/meevax.svg?branch=master)](https://travis-ci.org/yamacir-kit/meevax)

Version 0.1.363 in development.

## Installation

Installation has been tested on Ubuntu 16.04 only.

### Dependency

- **C++17**
- **Boost C++ Libraries 1.58.0** (Ubuntu 16.04 default) or later
- **CMake 3.5.1** (Ubuntu 16.04 default) or later
- **GNU Multi-Precision Library**

### Ubuntu 16.04

``` bash
sudo add-apt-repository -y ppa:ubuntu-toolchain-r/test
sudo apt update
sudo apt install g++-7 libboost-all-dev

git clone -b master http://github.com/yamacir-kit/meevax.git
mkdir -p meevax/build
cd meevax/build
cmake .. -DCMAKE_CXX_COMPILER=/usr/bin/g++-7
make
```

