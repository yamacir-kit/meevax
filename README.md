<br/>
<h1 align="center">
  <a href="https://github.com/yamacir-kit/meevax/">
    <img src="https://github.com/yamacir-kit/meevax/wiki/svg/meevax-logo.v8.png"
         alt="Meevax Lisp System"/>
  </a>
</h1>

<h3 align="center">
  <img src="https://github.com/yamacir-kit/meevax/wiki/svg/description.png"
       alt="A programmable programming lanugage."/>
</h3>

<p align="center">
  <a href="https://github.com/yamacir-kit/meevax/actions">
    <img src="https://github.com/yamacir-kit/meevax/workflows/CI/badge.svg"
         alt="GitHub Actions Status"/>
  </a>
  <a href="https://www.codacy.com/manual/yamacir-kit/meevax?utm_source=github.com&amp;utm_medium=referral&amp;utm_content=yamacir-kit/meevax&amp;utm_campaign=Badge_Grade">
    <img src="https://api.codacy.com/project/badge/Grade/ebd3aba61f1141049229031b7f068de9"/>
  </a>
</p>

Meevax is a programming language classified as Lisp-1.
This language inherits the minimalism of Scheme, which is summarized in the following sentence:

> Programming languages should be designed not by piling feature on top of feature, but by removing the weaknesses and restrictions that make additional features appear necessary.
> <p align="right"> --
>   <a href="https://bitbucket.org/cowan/r7rs/raw/tip/rnrs/r7rs.pdf">
>     Revised<sup>7</sup> Report on the Algorithmic Language Scheme [1]
>   </a>
> </p>

HEAD version: 0.2.349.

## Contents

[Introduction](#Introduction)    <br/>
[1. Usage](#Usage)               <br/>
[2. Dependency](#Dependency)     <br/>
[3. Installation](#Installation) <br/>

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

|                                                Number | Name                       | Implemented as |
|------------------------------------------------------:|:---------------------------|:---------------|
|  [10](https://srfi.schemers.org/srfi-10/srfi-10.html) | #, external form           | Built-in
|  [62](https://srfi.schemers.org/srfi-62/srfi-62.html) | S-expression comments      | Built-in

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
*   GNU MP (Multiple Precision Arithmetic Library)
*   GNU MPFR (Multiple Precision Floating-Point Reliably Library)

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

- [1] A.shinn, J.Cowan, A. A. Greckler, editors, "<cite><a href="https://bitbucket.org/cowan/r7rs/raw/tip/rnrs/r7rs.pdf">Revised<sup>7</sup> Report on the Algorithmic Language Scheme</a></cite>", Technical report, 2013.

<!--
*   TinyScheme ([http://tinyscheme.sourceforge.net/](http://tinyscheme.sourceforge.net/))
*   SECDR-Scheme ([http://www.maroon.dti.ne.jp/nagar17/mulasame/](http://www.maroon.dti.ne.jp/nagar17/mulasame/))
-->
