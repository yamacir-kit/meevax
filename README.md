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

Meevax provides

*   not "**ad-hoc informally-specified**",
*   not "**bug-ridden**",
*   ~~not "**slow**"~~

implementation **R7RS Scheme** subset to C++.

<br/>

## Contents

0. [Overview](#Overview)
1. [Usage](#Usage)
2. [Dependency](#Dependency)
3. [Installation](#Installation)

<br/>

## Overview

### Latest Version

HEAD: 0.2.500.

### Characteristic Features

*   C++ RTTI based **dynamic typing**
*   C++ RAII based **lexical garbage collection**
*   Simple SECD machine based execution

### R7RS Support

R7RS-small subset.

### SRFI Support

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

*   [**Boost C++ Libraries**](https://www.boost.org/)
    * [Boost.Multiprecision](https://www.boost.org/doc/libs/release/libs/multiprecision/)
    * [Boost.IOStreams](https://www.boost.org/doc/libs/release/libs/iostreams/)
*   [**GNU Multiple Precision Arithmetic Library** (MP)](https://gmplib.org/)
*   [**GNU Multiple Precision Floating-Point Reliably Library** (MPFR)](https://www.mpfr.org/)

### License

| Name                | License                                  |
|:--------------------|:-----------------------------------------|
| Boost C++ Libraries | Boost Software License                   |
| GNU MP              | GNU Lesser General Public License (LGPL) |
| GNU MPFR            | GNU Lesser General Public License (LGPL) |

<br/>

## Installation

Installation has been tested on Ubuntu 18.04.
See `.github/workflows/main.yml` for the latest procedure.

**Installation on Ubuntu 16.04 is sufficient, but has been removed from maintenance**.

### Ubuntu 18.04

``` shell
sudo apt update
sudo apt install libboost-all-dev libgmp-dev libmpfr-dev

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

