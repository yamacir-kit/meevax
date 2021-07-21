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
3. [References](#References)
4. [Resources](#Resources)

<br/>


## Overview

There is no stable version.

Development HEAD: 0.3.265.

### Characteristic Features

-   **Architecture** - TR-SECD virtual machine.
-   **Modern C++ compatible dynamic typing** - Meevax provides RTTI-based language runtime library.
-   **Lexical garbage collection**

### Standards

An subset of R7RS-small.

### SRFI

| Number                                                | Name                                                     | Import from | Note       |
|------------------------------------------------------:|:---------------------------------------------------------|:------------|:-----------|
|  [ 1](https://srfi.schemers.org/srfi-1/srfi-1.html)   | List Library                                             | built-in    |            |
|  [ 5](https://srfi.schemers.org/srfi-5/srfi-5.html)   | A compatible let form with signatures and rest arguments | built-in    | R7RS 4.2.4 |
|  [ 6](https://srfi.schemers.org/srfi-6/srfi-6.html)   | Basic String Ports                                       | built-in    | R7RS 6.13  |
|  [ 8](https://srfi.schemers.org/srfi-8/srfi-8.html)   | receive: Binding to multiple values                      | built-in    |            |
|  [10](https://srfi.schemers.org/srfi-10/srfi-10.html) | #, external form                                         | built-in    |            |
|  [23](https://srfi.schemers.org/srfi-23/srfi-23.html) | Error reporting mechanism                                | built-in    | R7RS 6.11  |
|  [39](https://srfi.schemers.org/srfi-39/srfi-39.html) | Parameter objects                                        | built-in    | R7RS 4.2.6 |
|  [45](https://srfi.schemers.org/srfi-45/srfi-45.html) | Primitives for Expressing Iterative Lazy Algorithms      | built-in    | [#296](https://github.com/yamacir-kit/meevax/issues/296)
|  [62](https://srfi.schemers.org/srfi-62/srfi-62.html) | S-expression comments                                    | built-in    | R7RS 2.2   |
|  [78](https://srfi.schemers.org/srfi-78/srfi-78.html) | Lightweight testing                                      | built-in    | Except `check-ec`
|  [87](https://srfi.schemers.org/srfi-87/srfi-87.html) | => in case clauses                                       | built-in    | R7RS 4.2.1 |

<br/>


## Requirements

### System

-   Ubuntu 18.04 or later

### Tools

-   Compiler with C++17 support (GCC >= 7.5.0, Clang >= 6.0.0)
-   CMake (>= 3.10.2) <!-- Ubuntu 18.04 LTS default CMake version -->
-   GNU Make
-   GNU Binutils
-   [**GNU Multiple Precision Arithmetic Library** (GMP)](https://gmplib.org/)
-   [**Boost C++ Libraries**](https://www.boost.org/) (1.65.1 or later)

<br/>


## Installation

### 0. Clone

``` bash
git clone https://github.com/yamacir-kit/meevax.git
cd meevax
```

### 1. Setup

``` bash
sudo apt update
sudo apt install libboost-all-dev libgmp-dev
```

### 2. Configure

``` bash
cmake -B build -DCMAKE_BUILD_TYPE=Release
cd build
```

### 3. Make

``` bash
make
```

### 4. Install

``` bash
sudo make install
sudo ldconfig
```

### 5. Test (Optional)

``` bash
sudo apt install valgrind kcachegrind massif-visualizer
make test
```


<br/>


## Liscence

See [LICENSE](./LICENSE)

| Name                | License                                  |
|:--------------------|:-----------------------------------------|
| Boost C++ Libraries | Boost Software License                   |
| GNU MP              | GNU Lesser General Public License (LGPL) |

<br/>


## References

- [1] A.shinn, J.Cowan, A. A. Greckler, editors, "<cite><a href="https://bitbucket.org/cowan/r7rs/raw/tip/rnrs/r7rs.pdf">Revised<sup>7</sup> Report on the Algorithmic Language Scheme</a></cite>", Technical report, 2013.

<br/>


## Resources

*   [TinyScheme](http://tinyscheme.sourceforge.net/)
*   [SECDR-Scheme](http://www.maroon.dti.ne.jp/nagar17/mulasame/)
