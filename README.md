## Overview

> Programming languages should be designed not by piling feature on top of
> feature, but by removing the weaknesses and restrictions that make additional
> features appear necessary.
> <div align="right">Revised<sup>7</sup> Report on the Algorithmic Language Scheme</div>

Meevax is an implementation of Lisp-1 programming language, supporting the
latest [Scheme](http://www.scheme-reports.org/) language standard and some
[SRFI](https://srfi.schemers.org/)s (SRFI; Scheme requests for implementation).
This implementation is focused on integration with modern C++ and practicality:
it not only works as an interpreter with support for the latest Scheme
standard, but also provides a flexible Lisp-1 kernel as a C++ library. The
library is installed as a CMake package for [easy
linking](./example/CMakeLists.txt), and [any C++ classes can be used from
Lisp-1 scripts](./example/example.ss) [via simple stubs](example/example.cpp).

### Releases

Latest release is [here](https://github.com/yamacir-kit/meevax/releases).

### Features

- Traditional SECD machine [[Landin 1964](#Landin-1964)].
- Low-level hygienic macro system, known as *syntactic closures* [[Bawden and
  Rees 1988](#Bawden-and-Rees-1988); [Hanson 1991](#Hanson-1991)] and *explicit
  renaming* [[Clinger 1991](#Clinger-1991)]. For these, the well-known macro
  transformers `sc-macro-transformer`, `rsc-macro-transformer`, and
  `er-macro-transformer` from the library [`(meevax
  macro-transformer)`](./basis/meevax.ss) are provided. Note that these are
  non-Scheme standards.
- C++ friendly precise garbage collection [[Kempf 2001a](#Kempf-2001a); [Kempf
  2001b](#Kempf-2001b)]

### Standards

Meevax can be used as an interpreter that supports the Scheme standard specified by the following report:

- Revised<sup>4</sup> Report on the Algorithmic Language Scheme (R4RS) [[Clinger and Rees 1991a](#Clinger-and-Rees-1991a)]
- Revised<sup>5</sup> Report on the Algorithmic Language Scheme (R5RS) [[Kelsey, Clinger and Rees 1998](#Kelsey-Clinger-and-Rees-1998)]
- Revised<sup>7</sup> Report on the Algorithmic Language Scheme (R7RS) [[Shinn, Cowan and Gleckler 2013](#Shinn-Cowan-and-Gleckler-2013)]

Procedures for each standard are provided by the following R7RS-style libraries:

| Language | Library name |
|:--------:|--------------|
| R4RS     | [`(scheme r4rs)`](./basis/r4rs.ss)
| R5RS     | [`(scheme r5rs)`](./basis/r5rs.ss)
| R7RS     | [`(scheme base)`](./basis/r7rs.ss) [`(scheme box)`](./basis/r7rs.ss) [`(scheme case-lambda)`](./basis/r7rs.ss) [`(scheme char)`](./basis/r7rs.ss) [`(scheme complex)`](./basis/r7rs.ss) [`(scheme cxr)`](./basis/r7rs.ss) [`(scheme eval)`](./basis/r7rs.ss) [`(scheme file)`](./basis/r7rs.ss) [`(scheme inexact)`](./basis/r7rs.ss) [`(scheme lazy)`](./basis/r7rs.ss) [`(scheme list)`](./basis/r7rs.ss) [`(scheme load)`](./basis/r7rs.ss) [`(scheme process-context)`](./basis/r7rs.ss) [`(scheme read)`](./basis/r7rs.ss) [`(scheme repl)`](./basis/r7rs.ss) [`(scheme time)`](./basis/r7rs.ss) [`(scheme write)`](./basis/r7rs.ss)

### SRFIs

| Number                                                  | Title                                                  | Library name                        | Note                               |
|--------------------------------------------------------:|--------------------------------------------------------|-------------------------------------|------------------------------------|
| [  0](https://srfi.schemers.org/srfi-0/srfi-0.html)     | Feature-based conditional expansion construct          | [`(srfi 0)`](./basis/srfi-0.ss)     | R7RS 4.2.1                         |
| [  1](https://srfi.schemers.org/srfi-1/srfi-1.html)     | List Library                                           | [`(srfi 1)`](./basis/srfi-1.ss)     | [`(scheme list)`](./basis/r7rs.ss) |
| [  4](https://srfi.schemers.org/srfi-4/srfi-4.html)     | Homogeneous numeric vector datatypes                   | [`(srfi 4)`](./basis/srfi-4.ss)     | R7RS 6.9                           |
| [  6](https://srfi.schemers.org/srfi-6/srfi-6.html)     | Basic String Ports                                     | [`(srfi 6)`](./basis/srfi-6.ss)     | R7RS 6.13                          |
| [  8](https://srfi.schemers.org/srfi-8/srfi-8.html)     | receive: Binding to multiple values                    | [`(srfi 8)`](./basis/srfi-8.ss)     |                                    |
| [  9](https://srfi.schemers.org/srfi-9/srfi-9.html)     | Defining Record Types                                  | [`(srfi 9)`](./basis/srfi-9.ss)     | R7RS 5.5                           |
| [ 10](https://srfi.schemers.org/srfi-10/srfi-10.html)   | #, external form                                       |                                     |                                    |
| [ 11](https://srfi.schemers.org/srfi-11/srfi-11.html)   | Syntax for receiving multiple values                   | [`(srfi 11)`](./basis/srfi-11.ss)   | R7RS 4.2.2                         |
| [ 16](https://srfi.schemers.org/srfi-16/srfi-16.html)   | Syntax for procedures of variable arity                | [`(srfi 16)`](./basis/srfi-16.ss)   | R7RS 4.2.9                         |
| [ 23](https://srfi.schemers.org/srfi-23/srfi-23.html)   | Error reporting mechanism                              | [`(srfi 23)`](./basis/srfi-23.ss)   | R7RS 6.11                          |
| [ 30](https://srfi.schemers.org/srfi-30/srfi-30.html)   | Nested Multi-line Comments                             |                                     | R7RS 2.2                           |
| [ 31](https://srfi.schemers.org/srfi-31/srfi-31.html)   | A special form rec for recursive evaluation            | [`(srfi 31)`](./basis/srfi-31.ss)   |                                    |
| [ 34](https://srfi.schemers.org/srfi-34/srfi-34.html)   | Exception Handling for Programs                        | [`(srfi 34)`](./basis/srfi-34.ss)   | R7RS 6.11                          |
| [ 38](https://srfi.schemers.org/srfi-38/srfi-38.html)   | External Representation for Data With Shared Structure | [`(srfi 38)`](./basis/srfi-38.ss)   | R7RS 6.13.3                        |
| [ 39](https://srfi.schemers.org/srfi-39/srfi-39.html)   | Parameter objects                                      | [`(srfi 39)`](./basis/srfi-39.ss)   | R7RS 4.2.6                         |
| [ 45](https://srfi.schemers.org/srfi-45/srfi-45.html)   | Primitives for Expressing Iterative Lazy Algorithms    | [`(srfi 45)`](./basis/srfi-45.ss)   | R7RS 4.2.5                         |
| [ 62](https://srfi.schemers.org/srfi-62/srfi-62.html)   | S-expression comments                                  |                                     | R7RS 2.2                           |
| [ 78](https://srfi.schemers.org/srfi-78/srfi-78.html)   | Lightweight testing                                    | [`(srfi 78)`](./basis/srfi-78.ss)   | Except `check-ec`                  |
| [ 87](https://srfi.schemers.org/srfi-87/srfi-87.html)   | => in case clauses                                     |                                     | R7RS 4.2.1                         |
| [ 98](https://srfi.schemers.org/srfi-98/srfi-98.html)   | An interface to access environment variables           | [`(srfi 98)`](./basis/srfi-98.ss)   | R7RS 6.14                          |
| [111](https://srfi.schemers.org/srfi-111/srfi-111.html) | Boxes                                                  | [`(srfi 111)`](./basis/srfi-111.ss) | [`(scheme box)`](./basis/r7rs.ss)  |
| [149](https://srfi.schemers.org/srfi-149/srfi-149.html) | Basic syntax-rules template extensions                 | [`(srfi 149)`](./basis/srfi-149.ss) | R7RS 4.3.2                         |

## Installation

### Requirements

- [GCC](https://gcc.gnu.org/) (>= 9.4.0) or [Clang](https://clang.llvm.org/) (>= 11.0.0)
- [CMake](https://cmake.org/) (>= 3.16.3)
- [GNU Make](http://savannah.gnu.org/projects/make)
- [GNU Binutils](https://www.gnu.org/software/binutils/)
- [GNU Multiple Precision Arithmetic Library (GMP)](https://gmplib.org/)

### Install

``` bash
cmake -B build -DCMAKE_BUILD_TYPE=Release
cd build
make package
sudo apt install build/meevax_0.5.77_amd64.deb
```

or

``` bash
cmake -B build -DCMAKE_BUILD_TYPE=Release
cd build
make install
```

### Uninstall

If you installed with `sudo apt install`,

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

| Target Name | Description
|-------------|-------------
| `all`       | Build shared-library `libmeevax.0.5.77.so` and executable `meevax`
| `test`      | Test executable `meevax`
| `package`   | Generate debian package `meevax_0.5.77_amd64.deb`
| `install`   | Copy files into `/usr/local` directly

## Usage

```
Usage:
  meevax [OPTION...] [FILE...]

Options:
  -e, --evaluate=STRING  read and evaluate STRING on interaction-environment
  -h, --help             display this help and exit
  -i, --interactive      enter an interactive session
  -l, --load=FILE        load FILE into interaction-environment
  -v, --version          display version information and exit
  -w, --write=STRING     same as `(write (read STRING))`
```

## License

See [LICENSE](./LICENSE).

## References

| Authors                                                                                                | Year | Title                                                                                                                                     | Journal Title / Publisher                                                                                                                                 | Pages          |
|--------------------------------------------------------------------------------------------------------|:----:|-------------------------------------------------------------------------------------------------------------------------------------------|-----------------------------------------------------------------------------------------------------------------------------------------------------------|:--------------:|
| <a id="McCarthy-1960"                ></a> John McCarthy                                               | 1960 | [Recursive functions of symbolic expressions and their computation by machine, Part I](https://dl.acm.org/doi/10.1145/367177.367199)      | [Communications of the ACM, Volume 3, Issue 4](https://dl.acm.org/toc/cacm/1960/3/4)                                                                      | 184&#x2011;195 |
| <a id="Landin-1964"                  ></a> P. J. Landin                                                | 1964 | [The Mechanical Evaluation of Expressions](https://academic.oup.com/comjnl/article/6/4/308/375725)                                        | [The Computor Journal, Volume 6, Issue 4](https://academic.oup.com/comjnl/issue/6/4)                                                                      | 308&#x2011;320 |
| <a id="Henderson-1980"               ></a> Peter Henderson                                             | 1980 | [Functional Programming: Application and Implementation](https://archive.org/details/functionalprogra0000hend/mode/2up)                   | Prentice Hall                                                                                                                                             |                |
| <a id="Bawden-and-Rees-1988"         ></a> Alan Bawden and Jonathan Rees                               | 1988 | [Syntactic Closures](https://dl.acm.org/doi/10.1145/62678.62687)                                                                          | [LFP '88: Proceedings of the 1988 ACM Conference on LISP and Functional Programming](https://dl.acm.org/doi/proceedings/10.1145/62678)                    | 86&#x2011;95   |
| <a id="Clinger-and-Rees-1991a"       ></a> William Clinger and Jonathan Rees (Editors)                 | 1991 | [Revised<sup>4</sup> Report on the Algorithmic Language Scheme](https://dl.acm.org/doi/10.1145/382130.382133)                             | [ACM SIGPLAN LISP Pointers, Volume IV, Issue 3](https://dl.acm.org/toc/sigplan-lisppointers/1991/IV/3)                                                    | 1&#x2011;55    |
| <a id="Hanson-1991"                  ></a> Chris Hanson                                                | 1991 | [A Syntactic Closures Macro Facility](https://dl.acm.org/doi/10.1145/1317265.1317267)                                                     | [ACM SIGPLAN LISP Pointers, Volume IV, Issue 4](https://dl.acm.org/toc/sigplan-lisppointers/1991/IV/4)                                                    | 9&#x2011;16    |
| <a id="Clinger-1991"                 ></a> William Clinger                                             | 1991 | [Hygienic Macros Through Explicit Renaming](https://dl.acm.org/doi/10.1145/1317265.1317269)                                               | [ACM SIGPLAN LISP Pointers, Volume IV, Issue 4](https://dl.acm.org/toc/sigplan-lisppointers/1991/IV/4)                                                    | 25&#x2011;28   |
| <a id="Clinger-and-Rees-1991b"       ></a> William Clinger and Jonathan Rees                           | 1991 | [Macros That Work](https://dl.acm.org/doi/10.1145/99583.99607)                                                                            | [POPL '91: Proceedings of the 18th ACM SIGPLAN-SIGACT Symposium on Principles of Programming Languages](https://dl.acm.org/doi/proceedings/10.1145/99583) | 155&#x2011;162 |
| <a id="Kelsey-Clinger-and-Rees-1998" ></a> Rechard Kelsey, William Clinger and Jonathan Rees (Editors) | 1998 | [Revised<sup>5</sup> Report on the Algorithmic Language Scheme](https://dl.acm.org/doi/10.1145/290229.290234)                             | [ACM SIGPLAN Notices, Volume 33, Issue 9](https://dl.acm.org/toc/sigplan/1998/33/9)                                                                       | 26&#x2011;76   |
| <a id="Kempf-2001a"                  ></a> William E. Kempf                                            | 2001 | [A garbage collection framework for C++](https://www.codeproject.com/Articles/912/A-garbage-collection-framework-for-C)                   | https://www.codeproject.com/Articles/912/A-garbage-collection-framework-for-C                                                                             |                |
| <a id="Kempf-2001b"                  ></a> William E. Kempf                                            | 2001 | [A garbage collection framework for C++ - Part II](https://www.codeproject.com/Articles/938/A-garbage-collection-framework-for-C-Part-II) | https://www.codeproject.com/Articles/938/A-garbage-collection-framework-for-C-Part-II                                                                     |                |
| <a id="Adams-and-Dybvig-2008"        ></a> Michael D. Adams and R. Kent Dybvig                         | 2008 | [Efficient Nondestructive Equality Checking for Trees and Graphs](https://dl.acm.org/doi/10.1145/1411204.1411230)                         | [ICFP '08: Proceedings of the 13th ACM SIGPLAN International Conference on Functional Programming](https://dl.acm.org/doi/proceedings/10.1145/1411204)    | 179&#x2011;188 |
| <a id="Shinn-Cowan-and-Gleckler-2013"></a> Alex Shinn, John Cowan and Arthur A. Gleckler (Editors)     | 2013 | [Revised<sup>7</sup> Report on the Algorithmic Language Scheme](https://standards.scheme.org/official/r7rs.pdf)                           | http://www.scheme-reports.org/                                                                                                                            |                |
