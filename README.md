<h1 align="center">
  Meevax Lisp System 0
</h1>

<h3 align="center">
  The Meta Extensible EVAluator and eXtensions
</h3>

<div align="center">
  <a href="https://travis-ci.org/yamacir-kit/meevax">
    <img src="https://travis-ci.org/yamacir-kit/meevax.svg?branch=master" alt="Build Status"/>
  </a>

  *HEAD Version 0.2.73*
</div>

<br/>

## Contents

- [Introduction](#Introduction)
- [Usage](#Usage)
- [Dependency](#Dependency)
- [Installation](#Installation)

<br/>

## Introduction

Meevax Lisp is a lisp-1 programming language written in C++17.
This language aiming highly binary level compatibility for modern C++ program.

Meevax gives
- NOT "ad-hoc informally-specified",
- NOT "bug-ridden",
- ~~NOT "slow"~~

implementation of ~~FULL of~~ **Scheme** to C++.

### Characteristic Features

- C++ RTTI based dynamic typing
- C++ standard smart pointer based **reference counting GC**
- Simple SECD machine based execution

### Supported SRFI

- SRFI 62: S-expression comments

<br/>

## Usage

| Option | Example&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; | Effects |
|:-:|:--|:--|
| None | `$ meevax` | Start interactive mode ~~with the standard library already imported.~~ |
| None | `$ meevax < example.scm` | Execute the code in the file as if manually input in interactive mode, and then exit the program in the same way as if you input EOF. |

<br/>

## Dependency

### Build Requirements

- C++17
- CMake

### Library Dependencies

- Boost C++ Libraries
- The GNU Multiple Precision Arithmetic Library (a.k.a GMP)
- The GNU Multiple Precision Floating-Point Reliably Library (a.k.a GNU MPFR)

<br/>

## Installation

Installation has been tested on Ubuntu 16.04 only.

### Ubuntu 16.04

``` bash
sudo add-apt-repository -y ppa:ubuntu-toolchain-r/test # for g++-7
sudo apt update
sudo apt install g++-7 libboost-all-dev

git clone -b master http://github.com/yamacir-kit/meevax.git
mkdir -p meevax/build
cd meevax/build
cmake .. -DCMAKE_CXX_COMPILER=/usr/bin/g++-7
make
```

<br/>

## References

- TinyScheme ([http://tinyscheme.sourceforge.net/](http://tinyscheme.sourceforge.net/))
- SECDR-Scheme ([http://www.maroon.dti.ne.jp/nagar17/mulasame/](http://www.maroon.dti.ne.jp/nagar17/mulasame/))
