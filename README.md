<p align="center">
  <img src="https://github.com/yamacir-kit/meevax/wiki/svg/meevax-logo.v8.png" alt="Meevax Lisp System"/>
  <br/>
  <img src="https://github.com/yamacir-kit/meevax/wiki/svg/description.png" alt="A programmable programming lanugage."/>
</p>
<hr color=#c1ab05/>
<p align="center">
  <img src="https://github.com/yamacir-kit/meevax/actions/workflows/build.yaml/badge.svg"/>
  <img src="https://github.com/yamacir-kit/meevax/actions/workflows/release.yaml/badge.svg"/>
</p>
<p align="center">
  <b><a href="#Overview" >Overview</a></b> &nbsp;|&nbsp;
  <b><a href="#Requirements">Requirements</a></b> &nbsp;|&nbsp;
  <b><a href="#Installation">Installation</a></b> &nbsp;|&nbsp;
  <b><a href="#Usage">Usage</a></b> &nbsp;|&nbsp;
  <b><a href="#License">License</a></b> &nbsp;|&nbsp;
  <b><a href="#References">References</a></b>
</p>

## Overview

> Programming languages should be designed not by piling feature on top of feature, but by removing the weaknesses and restrictions that make additional features appear necessary.
> <div align="right">
>   Revised<sup>7</sup> Report on the Algorithmic Language Scheme [1]
> </div>

Meevax is an implementation of Lisp-1 programming language, supporting subset of the [Scheme](http://www.scheme-reports.org/) (R7RS) and [SRFI](https://srfi.schemers.org/)s.

### Releases

Latest release is [here](https://github.com/yamacir-kit/meevax/releases).

### Features

-   Architecture - TR-SECD virtual machine.
-   Modern C++ compatible dynamic typing - Meevax provides RTTI-based language runtime library.

### Standards

Subset of R7RS-small.

### SRFIs

| Number                                                  | Title                                                    | Import from                                                                                                                                                                    | Note       |
|--------------------------------------------------------:|:---------------------------------------------------------|:-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|:-----------|
| [  1](https://srfi.schemers.org/srfi-1/srfi-1.html)     | List Library                                             | [`(srfi 1)`](./basis/srfi-1.ss)                                                                                                                                                |            |
| [  5](https://srfi.schemers.org/srfi-5/srfi-5.html)     | A compatible let form with signatures and rest arguments | [`(scheme r4rs essential)`](./basis/r4rs-essential.ss) <br> [`(scheme r4rs)`](./basis/r4rs.ss) <br> [`(scheme r5rs)`](./basis/r5rs.ss) <br> [`(scheme base)`](./basis/r7rs.ss) | R7RS 4.2.4 |
| [  6](https://srfi.schemers.org/srfi-6/srfi-6.html)     | Basic String Ports                                       | [`(srfi 6)`](./basis/srfi-6.ss) <br> [`(scheme base)`](./basis/r7rs.ss)                                                                                                        | R7RS 6.13  |
| [  8](https://srfi.schemers.org/srfi-8/srfi-8.html)     | receive: Binding to multiple values                      | [`(srfi 8)`](./basis/srfi-8.ss)                                                                                                                                                |            |
| [ 10](https://srfi.schemers.org/srfi-10/srfi-10.html)   | #, external form                                         | built-in                                                                                                                                                                       |            |
| [ 11](https://srfi.schemers.org/srfi-11/srfi-11.html)   | Syntax for receiving multiple values                     | [`(srfi 11)`](./basis/srfi-11.ss) <br> [`(scheme base)`](./basis/r7rs.ss)                                                                                                      | R7RS 4.2.2 |
| [ 23](https://srfi.schemers.org/srfi-23/srfi-23.html)   | Error reporting mechanism                                | [`(srfi 23)`](./basis/srfi-23.ss) <br> [`(scheme base)`](./basis/r7rs.ss)                                                                                                      | R7RS 6.11  |
| [ 34](https://srfi.schemers.org/srfi-34/srfi-34.html)   | Exception Handling for Programs                          | [`(srfi 34)`](./basis/srfi-34.ss) <br> [`(scheme base)`](./basis/r7rs.ss)                                                                                                      | R7RS 6.11  |
| [ 39](https://srfi.schemers.org/srfi-39/srfi-39.html)   | Parameter objects                                        | [`(srfi 39)`](./basis/srfi-39.ss) <br> [`(scheme base)`](./basis/r7rs.ss)                                                                                                      | R7RS 4.2.6 |
| [ 45](https://srfi.schemers.org/srfi-45/srfi-45.html)   | Primitives for Expressing Iterative Lazy Algorithms      | [`(srfi 45)`](./basis/srfi-45.ss)                                                                                                                                              | [#296](https://github.com/yamacir-kit/meevax/issues/296)
| [ 62](https://srfi.schemers.org/srfi-62/srfi-62.html)   | S-expression comments                                    | built-in                                                                                                                                                                       | R7RS 2.2   |
| [ 78](https://srfi.schemers.org/srfi-78/srfi-78.html)   | Lightweight testing                                      | [`(srfi 78)`](./basis/srfi-78.ss)                                                                                                                                              | Except `check-ec`
| [ 87](https://srfi.schemers.org/srfi-87/srfi-87.html)   | => in case clauses                                       | [`(scheme r4rs essential)`](./basis/r4rs-essential.ss) <br> [`(scheme r4rs)`](./basis/r4rs.ss) <br> [`(scheme r5rs)`](./basis/r5rs.ss) <br> [`(scheme base)`](./basis/r7rs.ss) | R7RS 4.2.1 |
| [149](https://srfi.schemers.org/srfi-149/srfi-149.html) | Basic `syntax-rules` template extensions                 | [`(srfi 149)`](./basis/srfi-149.ss) <br> [`(scheme r5rs)`](./basis/r5rs.ss) <br> [`(scheme base)`](./basis/r7rs.ss)                                                            | R7RS 4.3.2 |
| [211](https://srfi.schemers.org/srfi-211/srfi-211.html) | Scheme Macro Libraries                                   | [`(srfi 211 explicit-renaming)`](./basis/srfi-211.ss)                                                                                                                          |            |

## Requirements

### Software

-   [GCC](https://gcc.gnu.org/) (>= 7.5.0) or [Clang](https://clang.llvm.org/) (>= 9.0.0)
-   [CMake](https://cmake.org/) (>= 3.10.2)
-   [GNU Make](http://savannah.gnu.org/projects/make)
-   [GNU Binutils](https://www.gnu.org/software/binutils/)
-   [GNU Multiple Precision Arithmetic Library (GMP)](https://gmplib.org/)

To install the above software, it is easy to use the following script.

``` bash
$ ./script/setup.sh --required
```

## Installation

### Install

``` bash
$ cmake -B build -DCMAKE_BUILD_TYPE=Release
$ cd build
$ make install.deb
```

### Uninstall

``` bash
$ sudo apt remove meevax
```
<!--
or
``` bash
sudo rm -rf /usr/local/bin/meevax
sudo rm -rf /usr/local/include/meevax
sudo rm -rf /usr/local/lib/libmeevax*
sudo rm -rf /usr/local/share/meevax
```
-->

### CMake targets

| Target Name        | Description
|:-------------------|:--
| `all` (default)    | Build shared-library `libmeevax.0.4.156.so` and executable `meevax`.
| `test`             | Test executable `meevax`.
| `package`          | Generate debian package `meevax_0.4.156_amd64.deb`.
| `install`          | Copy files into `/usr/local` __(1)__.
| `install.deb`      | `all` + `package` + `sudo apt install <meevax>.deb`
| `safe-install.deb` | `all` + `test` + `package` + `sudo apt install <meevax>.deb`

__(1)__ Meevax installed by `make install` cannot be uninstalled by the system's package manager (for example, `apt remove meevax`). You need to manually delete the following files to uninstall:

- `/usr/local/bin/meevax`
- `/usr/local/include/meevax`
- `/usr/local/lib/libmeevax*`
- `/usr/local/share/meevax`

## Usage

```
Meevax Lisp System, version 0.4.156

Usage: meevax [OPTION...] [FILE...]

Options:
  -b, --batch            Suppress any system output.
  -d, --debug            Deprecated.
  -e, --evaluate=STRING  Read and evaluate given STRING at configuration step.
  -h, --help             Display this help text and exit.
  -i, --interactive      Take over control of root environment.
  -l, --load=FILENAME    Same as -e '(load FILENAME)'
  -t, --trace            Display stacks of virtual machine for each steps.
  -v, --version          Display version information and exit.
  -w, --write=OBJECT     Same as -e '(write OBJECT)'
```

| Example                                    | Effects |
|:-------------------------------------------|:--|
| `$ meevax -i`                              | Start interactive session. You can exit the session by input `(exit)` or Ctrl+C or Ctrl+D.
| `$ meevax foo.ss`                          | Evaluate a script `foo.ss`. |
| `$ meevax -e '(+ 1 2 3)'`                  | Display `6`.
| `$ meevax -e "(define home \"$HOME\")" -i` | Define value of shell-environment variable `$HOME` as string typed Scheme variable `home`, and then start interactive session on environment includes the variable `home`.

## License

See [LICENSE](./LICENSE).

## References

- [1] A.shinn, J.Cowan, A. A. Greckler, editors, "[Revised<sup>7</sup> Report on the Algorithmic Language Scheme](https://bitbucket.org/cowan/r7rs/raw/tip/rnrs/r7rs.pdf)", Technical report, 2013.

### Resources

*   [TinyScheme](http://tinyscheme.sourceforge.net/)
*   [SECDR-Scheme](http://www.maroon.dti.ne.jp/nagar17/mulasame/)
