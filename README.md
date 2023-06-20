<p align="center">
  <img src="https://github.com/yamacir-kit/meevax/wiki/svg/meevax-logo.v8.png" alt="Meevax Lisp System"/>
  <br/>
  <img src="https://github.com/yamacir-kit/meevax/wiki/svg/description.png" alt="A programmable programming lanugage."/>
</p>
<hr/>
<p align="center">
  <img src="https://github.com/yamacir-kit/meevax/actions/workflows/build.yaml/badge.svg"/>
  <img src="https://github.com/yamacir-kit/meevax/actions/workflows/release.yaml/badge.svg"/>
</p>
<p align="center">
  <b>
    <a href="#Overview">Overview</a>
  </b>
  &nbsp;|&nbsp;
  <b>
    <a href="#Installation">Installation</a>
  </b>
  &nbsp;|&nbsp;
  <b>
    <a href="#Usage">Usage</a>
  </b>
  &nbsp;|&nbsp;
  <b>
    <a href="#License">License</a>
  </b>
  &nbsp;|&nbsp;
  <b>
    <a href="#References">References</a>
  </b>
</p>

## Overview

> Programming languages should be designed not by piling feature on top of feature, but by removing the weaknesses and restrictions that make additional features appear necessary.
> <div align="right">Revised<sup>7</sup> Report on the Algorithmic Language Scheme [1]</div>

Meevax is an implementation of Lisp-1 programming language, supporting subset of the [Scheme](http://www.scheme-reports.org/) (R7RS) and [SRFI](https://srfi.schemers.org/)s.

### Releases

Latest release is [here](https://github.com/yamacir-kit/meevax/releases).

### Features

-   Architecture - SECD machine.
-   Modern C++ compatible dynamic typing - Meevax provides RTTI-based language runtime library.

### Standards

Subset of R7RS-small.

### SRFIs

| Number                                                  | Title                                                  | Library name                        | Note              |
|--------------------------------------------------------:|:-------------------------------------------------------|:------------------------------------|:------------------|
| [  0](https://srfi.schemers.org/srfi-0/srfi-0.html)     | Feature-based conditional expansion construct          | [`(srfi 0)`](./basis/srfi-0.ss)     | R7RS 4.2.1        |
| [  1](https://srfi.schemers.org/srfi-1/srfi-1.html)     | List Library                                           | [`(srfi 1)`](./basis/srfi-1.ss)     |                   |
| [  4](https://srfi.schemers.org/srfi-4/srfi-4.html)     | Homogeneous numeric vector datatypes                   | [`(srfi 4)`](./basis/srfi-4.ss)     | R7RS 6.9          |
| [  6](https://srfi.schemers.org/srfi-6/srfi-6.html)     | Basic String Ports                                     | [`(srfi 6)`](./basis/srfi-6.ss)     | R7RS 6.13         |
| [  8](https://srfi.schemers.org/srfi-8/srfi-8.html)     | receive: Binding to multiple values                    | [`(srfi 8)`](./basis/srfi-8.ss)     |                   |
| [  9](https://srfi.schemers.org/srfi-9/srfi-9.html)     | Defining Record Types                                  | [`(srfi 9)`](./basis/srfi-9.ss)     | R7RS 5.5          |
| [ 10](https://srfi.schemers.org/srfi-10/srfi-10.html)   | #, external form                                       |                                     |                   |
| [ 11](https://srfi.schemers.org/srfi-11/srfi-11.html)   | Syntax for receiving multiple values                   | [`(srfi 11)`](./basis/srfi-11.ss)   | R7RS 4.2.2        |
| [ 23](https://srfi.schemers.org/srfi-23/srfi-23.html)   | Error reporting mechanism                              | [`(srfi 23)`](./basis/srfi-23.ss)   | R7RS 6.11         |
| [ 30](https://srfi.schemers.org/srfi-30/srfi-30.html)   | Nested Multi-line Comments                             |                                     | R7RS 2.2          |
| [ 31](https://srfi.schemers.org/srfi-31/srfi-31.html)   | A special form rec for recursive evaluation            | [`(srfi 31)`](./basis/srfi-31.ss)   |                   |
| [ 34](https://srfi.schemers.org/srfi-34/srfi-34.html)   | Exception Handling for Programs                        | [`(srfi 34)`](./basis/srfi-34.ss)   | R7RS 6.11         |
| [ 38](https://srfi.schemers.org/srfi-38/srfi-38.html)   | External Representation for Data With Shared Structure | [`(srfi 38)`](./basis/srfi-38.ss)   | R7RS 6.13.3       |
| [ 39](https://srfi.schemers.org/srfi-39/srfi-39.html)   | Parameter objects                                      | [`(srfi 39)`](./basis/srfi-39.ss)   | R7RS 4.2.6        |
| [ 45](https://srfi.schemers.org/srfi-45/srfi-45.html)   | Primitives for Expressing Iterative Lazy Algorithms    | [`(srfi 45)`](./basis/srfi-45.ss)   | R7RS 4.2.5        |
| [ 62](https://srfi.schemers.org/srfi-62/srfi-62.html)   | S-expression comments                                  |                                     | R7RS 2.2          |
| [ 78](https://srfi.schemers.org/srfi-78/srfi-78.html)   | Lightweight testing                                    | [`(srfi 78)`](./basis/srfi-78.ss)   | Except `check-ec` |
| [ 87](https://srfi.schemers.org/srfi-87/srfi-87.html)   | => in case clauses                                     |                                     | R7RS 4.2.1        |
| [ 98](https://srfi.schemers.org/srfi-98/srfi-98.html)   | An interface to access environment variables           | [`(srfi 98)`](./basis/srfi-98.ss)   | R7RS 6.14         |
| [149](https://srfi.schemers.org/srfi-149/srfi-149.html) | Basic syntax-rules template extensions                 | [`(srfi 149)`](./basis/srfi-149.ss) | R7RS 4.3.2        |

## Installation

### Requirements

-   [GCC](https://gcc.gnu.org/) (>= 9.4.0) or [Clang](https://clang.llvm.org/) (>= 11.0.0)
-   [CMake](https://cmake.org/) (>= 3.16.3)
-   [GNU Make](http://savannah.gnu.org/projects/make)
-   [GNU Binutils](https://www.gnu.org/software/binutils/)
-   [GNU Multiple Precision Arithmetic Library (GMP)](https://gmplib.org/)

### Install

``` bash
cmake -B build -DCMAKE_BUILD_TYPE=Release
cd build
make install.deb
```

or

``` bash
cmake -B build -DCMAKE_BUILD_TYPE=Release
cd build
make install
```

### Uninstall

If you installed with `make install.deb`,

``` bash
sudo apt remove meevax
```

or if you installed with `make install`,

``` bash
sudo rm -rf /usr/local/bin/meevax
sudo rm -rf /usr/local/include/meevax
sudo rm -rf /usr/local/lib/libmeevax*
sudo rm -rf /usr/local/share/meevax
```

### CMake targets

| Target Name        | Description
|--------------------|---
| `all` (default)    | Build shared-library `libmeevax.0.4.728.so` and executable `meevax`
| `test`             | Test executable `meevax`
| `package`          | Generate debian package `meevax_0.4.728_amd64.deb`
| `install`          | Copy files into `/usr/local`
| `install.deb`      | `all` + `package` + `sudo apt install <meevax>.deb`

## Usage

```
Meevax Lisp 0.4.728

Usage:
  meevax [option...] [file...]

Options:
  -e, --evaluate=<string>  read and evaluate <string> on interaction-environment
  -h, --help               display this help and exit
  -i, --interactive        enter an interactive session
  -l, --load=<file>        load <file> into interaction-environment
  -v, --version            display version information and exit
  -w, --write=<string>     same as `(write (read <string>))`

```

## License

See [LICENSE](./LICENSE).

## References

- [1] A.shinn, J.Cowan, A. A. Greckler, editors, "[Revised<sup>7</sup> Report on the Algorithmic Language Scheme](https://bitbucket.org/cowan/r7rs/raw/tip/rnrs/r7rs.pdf)", Technical report, 2013.

### Resources

*   [Chibi-Scheme](https://github.com/ashinn/chibi-scheme)
*   [SECDR-Scheme](http://www.maroon.dti.ne.jp/nagar17/mulasame/)
*   [TinyScheme](http://tinyscheme.sourceforge.net/)
