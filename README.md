<h1 align="center">
  <a href="https://github.com/yamacir-kit/meevax/">
    <img src="https://github.com/yamacir-kit/meevax/wiki/svg/meevax-logo.v8.png" alt="Meevax Lisp System"/>
  </a>
</h1>

<h3 align="center">
  <img src="https://github.com/yamacir-kit/meevax/wiki/svg/description.png" alt="A programmable programming lanugage."/>
</h3>

<p align="center">
  <a href="https://github.com/yamacir-kit/meevax/actions">
    <img src="https://github.com/yamacir-kit/meevax/workflows/CI/badge.svg" alt="GitHub Actions Status"/>
  </a>
  <a href="https://www.codacy.com/manual/yamacir-kit/meevax?utm_source=github.com&amp;utm_medium=referral&amp;utm_content=yamacir-kit/meevax&amp;utm_campaign=Badge_Grade">
    <img src="https://api.codacy.com/project/badge/Grade/ebd3aba61f1141049229031b7f068de9"/>
  </a>
</p>

Meevax is a programming language classified as Lisp-1.
This language inherits the minimalism of Scheme, which is summarized in the following sentence:

> Programming languages should be designed not by piling feature on top of feature, but by removing the weaknesses and restrictions that make additional features appear necessary.
> <p align="right"> --
>   <a href="https://bitbucket.org/cowan/r7rs/raw/tip/rnrs/r7rs.pdf"> Revised<sup>7</sup> Report on the Algorithmic Language Scheme [1] </a>
> </p>

Meevax provides

*   not "**ad-hoc informally-specified**",
*   not "**bug-ridden**",
*   ~~not "**slow**"~~

subset of **R7RS Scheme** implementation.

<br/>


## Contents

0. [Overview](#Overview)
1. [Requirements](#Requirements)
2. [Installation](#Installation)
3. [License](#License)
4. [Resources](#Resources)

<br/>


## Overview

There is no stable version.

Development HEAD: 0.2.646.

### Characteristic Features

-   **Architecture** - SECD virtual machine.
-   **Modern C++ compatible dynamic typing** - Meevax provides RTTI-based language runtime library.
-   **Lexical garbage collection**

### Standards

An subset of R7RS-small.

### Extensions

| Number                                                | Name                       | Implemented as |
|------------------------------------------------------:|:---------------------------|:---------------|
|  [10](https://srfi.schemers.org/srfi-10/srfi-10.html) | Sharp-comma external form  | Built-in
|  [62](https://srfi.schemers.org/srfi-62/srfi-62.html) | S-expression comments      | Built-in

<br/>


## Requirements

### System

-   Ubuntu 18.04 or later

### Tools

-   Compiler supports C++17 (GCC 7.5.0 or later, Clang 6.0.0 or later)
-   CMake 3.10.2
-   GNU Make
-   GNU Binutils
-   [**Boost C++ Libraries**](https://www.boost.org/) (1.65.1 or later)
    - [Boost.Multiprecision](https://www.boost.org/doc/libs/release/libs/multiprecision/)
    - [Boost.IOStreams](https://www.boost.org/doc/libs/release/libs/iostreams/)
-   [**GNU Multiple Precision Arithmetic Library** (GMP)](https://gmplib.org/)

<br/>


## Installation

Installation has been tested on Ubuntu 18.04.
See `.github/workflows/main.yaml` for the latest procedure.

### Ubuntu 18.04

``` shell
sudo apt update
sudo apt install libboost-all-dev libgmp-dev

git clone https://github.com/yamacir-kit/meevax.git
cd meevax

mkdir -p build
cd build

cmake ..
make
```

<br/>


## License

***TODO: Meevax license notation (probably Apache License 2.0 will be set)***

| Name                | License                                  |
|:--------------------|:-----------------------------------------|
| Boost C++ Libraries | Boost Software License                   |
| GNU MP              | GNU Lesser General Public License (LGPL) |

<br/>


## Resources

- [1] A.shinn, J.Cowan, A. A. Greckler, editors, "<cite><a href="https://bitbucket.org/cowan/r7rs/raw/tip/rnrs/r7rs.pdf">Revised<sup>7</sup> Report on the Algorithmic Language Scheme</a></cite>", Technical report, 2013.

<!--
*   TinyScheme ([http://tinyscheme.sourceforge.net/](http://tinyscheme.sourceforge.net/))
*   SECDR-Scheme ([http://www.maroon.dti.ne.jp/nagar17/mulasame/](http://www.maroon.dti.ne.jp/nagar17/mulasame/))
-->
