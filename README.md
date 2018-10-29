# meevax

[![Build Status](https://travis-ci.org/yamacir-kit/meevax.svg?branch=master)](https://travis-ci.org/yamacir-kit/meevax)

Version 0.1.1 in development.

## Installation

Installation has been tested on Ubuntu 16.04 only.

### Dependency

- **C++17**
- **Boost C++ Libraries 1.58.0** (Ubuntu 16.04 default) or later
- **CMake 3.5.1** (Ubuntu 16.04 default) or later

### Ubuntu (16.04 or later)

``` bash
sudo add-apt-repository -y ppa:ubuntu-toolchain-r/test
sudo apt update
sudo apt install g++-7 libboost-all-dev
sudo update-alternatives --install /usr/bin/g++ g++ /usr/bin/g++-7 90

git clone -b master http://github.com/yamacir-kit/meevax.git
mkdir -p meevax/build
cd meevax/build
cmake ..
make
```

