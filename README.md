![Meevax Lisp System](https://github.com/yamacir-kit/meevax/wiki/svg/meevax-logo.v5.png)

<div align="center">
  <a href="https://travis-ci.org/yamacir-kit/meevax">
    <img src="https://travis-ci.org/yamacir-kit/meevax.svg?branch=master" alt="Travis Status"/>
  </a>

  <a href="https://github.com/yamacir-kit/meevax/actions">
    <img src="https://github.com/yamacir-kit/meevax/workflows/CI/badge.svg" alt="GitHub Actions Status"/>
  </a>

  <a href="https://www.codacy.com/manual/yamacir-kit/meevax?utm_source=github.com&amp;utm_medium=referral&amp;utm_content=yamacir-kit/meevax&amp;utm_campaign=Badge_Grade">
    <img src="https://api.codacy.com/project/badge/Grade/ebd3aba61f1141049229031b7f068de9"/>
  </a>

  *HEAD Version 0.2.214*
</div>

## Contents

*   [Introduction](#Introduction)
*   [Usage](#Usage)
*   [Dependency](#Dependency)
*   [Installation](#Installation)

<br/>

## Introduction

Meevax Lisp is a Lisp-1 programming language written in C++17.
<!-- This language aiming highly binary level compatibility for modern C++ program. -->

Meevax gives
*   NOT "ad-hoc informally-specified",
*   NOT "bug-ridden",
*   ~~NOT "slow"~~

implementation of ~~FULL of~~ **Scheme** to C++.

### Characteristic Features

*   C++ RTTI based dynamic typing
*   C++ standard smart pointer based **reference counting GC**
*   Simple SECD machine based execution

### Supported SRFI

*   SRFI 62: S-expression comments

<br/>

## Usage

| Option            | Description                           |
|:------------------|:--------------------------------------|
| `-v`, `--version` | Display version information and exit. |
| `-h`, `--help`    | Display this help text and exit.      |

<br/>

## Dependency

### Build Requirements

*   C++17
*   CMake

### Library Dependencies

*   Boost C++ Libraries
*   The GNU Multiple Precision Arithmetic Library (a.k.a GMP)
*   The GNU Multiple Precision Floating-Point Reliably Library (a.k.a GNU MPFR)

<br/>

## Installation

Installation has been tested on Ubuntu 16.04, 18.04.
See `.github/workflows/main.yml` for the latest procedure.

### Ubuntu 16.04

``` bash
sudo apt-add-repository -y ppa:ubuntu-toolchain-r/test # for g++-7
sudo apt update
sudo apt install g++-7 \
                 libboost-all-dev \
                 libgmp-dev \
                 libmpfr-dev

git clone https://github.com/yamacir-kit/meevax.git
cd meevax

mkdir -p build
cd build

cmake .. -DCMAKE_CXX_COMPILER=/usr/bin/g++-7
make
```

### Ubuntu 18.04

``` bash
sudo apt update
sudo apt install libboost-all-dev \
                 libgmp-dev \
                 libmpfr-dev

git clone https://github.com/yamacir-kit/meevax.git
cd meevax

mkdir -p build
cd build

cmake ..
make
```

<br/>

## References

*   TinyScheme ([http://tinyscheme.sourceforge.net/](http://tinyscheme.sourceforge.net/))
*   SECDR-Scheme ([http://www.maroon.dti.ne.jp/nagar17/mulasame/](http://www.maroon.dti.ne.jp/nagar17/mulasame/))
