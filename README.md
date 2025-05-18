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

### Features

- Traditional SECD machine [[2](#Landin-1964)].
- Low-level hygienic macro system, known as *syntactic closures*
  [[4](#Bawden-and-Rees-1988), [6](#Hanson-1991)] and *explicit renaming*
  [[5](#Clinger-1991)]. For these, the well-known macro transformers
  `sc-macro-transformer`, `rsc-macro-transformer` and `er-macro-transformer`
  from the library [`(meevax macro-transformer)`](./basis/meevax.ss) are
  provided. Note that these are non-Scheme standards.
- C++ friendly precise garbage collection [[10](#Kempf-2001a),
  [11](#Kempf-2001b)]

### Standards

Meevax can be used as an interpreter that supports the Scheme standard specified by the following report:

- Revised<sup>4</sup> Report on the Algorithmic Language Scheme (R4RS) [[5](#Clinger-and-Rees-1991a)]
- Revised<sup>5</sup> Report on the Algorithmic Language Scheme (R5RS) [[9](#Kelsey-Clinger-and-Rees-1998)]
- Revised<sup>7</sup> Report on the Algorithmic Language Scheme (R7RS) [[13](#Shinn-Cowan-and-Gleckler-2013)]

Procedures for each standard are provided by the following R7RS-style libraries:

| Language | Library name |
|:--------:|--------------|
| R4RS     | [`(scheme r4rs)`](./basis/r4rs.ss)
| R5RS     | [`(scheme r5rs)`](./basis/r5rs.ss)
| R7RS     | [`(scheme base)`](./basis/r7rs.ss) [`(scheme box)`](./basis/r7rs.ss) [`(scheme case-lambda)`](./basis/r7rs.ss) [`(scheme char)`](./basis/r7rs.ss) [`(scheme complex)`](./basis/r7rs.ss) [`(scheme cxr)`](./basis/r7rs.ss) [`(scheme division)`](./basis/r7rs.ss) [`(scheme eval)`](./basis/r7rs.ss) [`(scheme file)`](./basis/r7rs.ss) [`(scheme flonum)`](./basis/r7rs.ss) [`(scheme inexact)`](./basis/r7rs.ss) [`(scheme lazy)`](./basis/r7rs.ss) [`(scheme list)`](./basis/r7rs.ss) [`(scheme load)`](./basis/r7rs.ss) [`(scheme process-context)`](./basis/r7rs.ss) [`(scheme read)`](./basis/r7rs.ss) [`(scheme repl)`](./basis/r7rs.ss) [`(scheme time)`](./basis/r7rs.ss) [`(scheme write)`](./basis/r7rs.ss)

### SRFIs

| Number                                                  | Title                                                  | Library name                        | Note                                   |
|--------------------------------------------------------:|--------------------------------------------------------|-------------------------------------|----------------------------------------|
| [  0](https://srfi.schemers.org/srfi-0/srfi-0.html)     | Feature-based conditional expansion construct          | [`(srfi 0)`](./basis/srfi-0.ss)     | R7RS 4.2.1                             |
| [  1](https://srfi.schemers.org/srfi-1/srfi-1.html)     | List Library                                           | [`(srfi 1)`](./basis/srfi-1.ss)     | [`(scheme list)`](./basis/r7rs.ss)     |
| [  4](https://srfi.schemers.org/srfi-4/srfi-4.html)     | Homogeneous numeric vector datatypes                   | [`(srfi 4)`](./basis/srfi-4.ss)     | R7RS 6.9                               |
| [  6](https://srfi.schemers.org/srfi-6/srfi-6.html)     | Basic String Ports                                     | [`(srfi 6)`](./basis/srfi-6.ss)     | R7RS 6.13                              |
| [  8](https://srfi.schemers.org/srfi-8/srfi-8.html)     | receive: Binding to multiple values                    | [`(srfi 8)`](./basis/srfi-8.ss)     |                                        |
| [  9](https://srfi.schemers.org/srfi-9/srfi-9.html)     | Defining Record Types                                  | [`(srfi 9)`](./basis/srfi-9.ss)     | R7RS 5.5                               |
| [ 10](https://srfi.schemers.org/srfi-10/srfi-10.html)   | #, external form                                       |                                     |                                        |
| [ 11](https://srfi.schemers.org/srfi-11/srfi-11.html)   | Syntax for receiving multiple values                   | [`(srfi 11)`](./basis/srfi-11.ss)   | R7RS 4.2.2                             |
| [ 16](https://srfi.schemers.org/srfi-16/srfi-16.html)   | Syntax for procedures of variable arity                | [`(srfi 16)`](./basis/srfi-16.ss)   | R7RS 4.2.9                             |
| [ 23](https://srfi.schemers.org/srfi-23/srfi-23.html)   | Error reporting mechanism                              | [`(srfi 23)`](./basis/srfi-23.ss)   | R7RS 6.11                              |
| [ 30](https://srfi.schemers.org/srfi-30/srfi-30.html)   | Nested Multi-line Comments                             |                                     | R7RS 2.2                               |
| [ 31](https://srfi.schemers.org/srfi-31/srfi-31.html)   | A special form rec for recursive evaluation            | [`(srfi 31)`](./basis/srfi-31.ss)   |                                        |
| [ 34](https://srfi.schemers.org/srfi-34/srfi-34.html)   | Exception Handling for Programs                        | [`(srfi 34)`](./basis/srfi-34.ss)   | R7RS 6.11                              |
| [ 38](https://srfi.schemers.org/srfi-38/srfi-38.html)   | External Representation for Data With Shared Structure | [`(srfi 38)`](./basis/srfi-38.ss)   | R7RS 6.13.3                            |
| [ 39](https://srfi.schemers.org/srfi-39/srfi-39.html)   | Parameter objects                                      | [`(srfi 39)`](./basis/srfi-39.ss)   | R7RS 4.2.6                             |
| [ 45](https://srfi.schemers.org/srfi-45/srfi-45.html)   | Primitives for Expressing Iterative Lazy Algorithms    | [`(srfi 45)`](./basis/srfi-45.ss)   | R7RS 4.2.5                             |
| [ 62](https://srfi.schemers.org/srfi-62/srfi-62.html)   | S-expression comments                                  |                                     | R7RS 2.2                               |
| [ 78](https://srfi.schemers.org/srfi-78/srfi-78.html)   | Lightweight testing                                    | [`(srfi 78)`](./basis/srfi-78.ss)   | Except `check-ec`                      |
| [ 87](https://srfi.schemers.org/srfi-87/srfi-87.html)   | => in case clauses                                     |                                     | R7RS 4.2.1                             |
| [ 98](https://srfi.schemers.org/srfi-98/srfi-98.html)   | An interface to access environment variables           | [`(srfi 98)`](./basis/srfi-98.ss)   | R7RS 6.14                              |
| [111](https://srfi.schemers.org/srfi-111/srfi-111.html) | Boxes                                                  | [`(srfi 111)`](./basis/srfi-111.ss) | [`(scheme box)`](./basis/r7rs.ss)      |
| [141](https://srfi.schemers.org/srfi-141/srfi-141.html) | Integer division                                       | [`(srfi 141)`](./basis/srfi-141.ss) | [`(scheme division)`](./basis/r7rs.ss) |
| [144](https://srfi.schemers.org/srfi-144/srfi-144.html) | Flonums                                                | [`(srfi 144)`](./basis/srfi-144.ss) | [`(scheme flonum)`](./basis/r7rs.ss)   |
| [149](https://srfi.schemers.org/srfi-149/srfi-149.html) | Basic syntax-rules template extensions                 | [`(srfi 149)`](./basis/srfi-149.ss) | R7RS 4.3.2                             |

## Installation

### Requirements

- [GCC](https://gcc.gnu.org/) (>= 11.4.0) or [Clang](https://clang.llvm.org/) (>= 14.0.0)
- [CMake](https://cmake.org/) (>= 3.22.1)
- [GNU Make](http://savannah.gnu.org/projects/make)
- [GNU Binutils](https://www.gnu.org/software/binutils/)
- [GNU Multiple Precision Arithmetic Library (GMP)](https://gmplib.org/)

### Releases

Latest release is [here](https://github.com/yamacir-kit/meevax/releases).

### Instruction

First, generate a Makefile using CMake with the following command:

``` bash
cmake -B build -DCMAKE_BUILD_TYPE=Release
cd build
```

Then, select one of the following targets and `make` it according to your purpose. In most cases, `make install` will be the one you choose.

| Target      | Description
|-------------|-------------
| `all`       | Build shared-library `libmeevax.0.5.352.so` and executable `meevax`.
| `install`   | Copy files into `/usr/local` directly.
| `package`   | Generate debian package `meevax_0.5.352_amd64.deb` (only Ubuntu). The generated package can be installed by `sudo apt install build/meevax_0.5.352_amd64.deb`.
| `test`      | Test executable `meevax`. This target requires Valgrind to be installed.
| `uninstall` | Remove files copied to `/usr/local` directly by target `install`.

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

[<span id="McCarthy-1960">1</span>] John McCarthy. [Recursive functions of symbolic expressions and their computation by machine, Part I](https://dl.acm.org/doi/10.1145/367177.367199). *[Communications of the ACM](https://dl.acm.org/toc/cacm/1960/3/4)*, 3(4):184--195, 1960.

[<span id="Landin-1964">2</span>] Peter J. Landin. [The Mechanical Evaluation of Expressions](https://academic.oup.com/comjnl/article/6/4/308/375725). *[The Computor Journal](https://academic.oup.com/comjnl/issue/6/4)*, 6(4):308--320, 1964.

[<span id="Henderson-1980">3</span>] Peter Henderson. *[Functional Programming: Application and Implementation](https://archive.org/details/functionalprogra0000hend/mode/2up)*. Prentice Hall, 1980.

[<span id="Bawden-and-Rees-1988">4</span>] Alan Bawden and Jonathan Rees. [Syntactic Closures](https://dl.acm.org/doi/10.1145/62678.62687). In *[LFP '88: Proceedings of the 1988 ACM Conference on LISP and Functional Programming](https://dl.acm.org/doi/proceedings/10.1145/62678)*, pages 86--95, 1988.

[<span id="Clinger-and-Rees-1991a">5</span>] William Clinger and Jonathan Rees (Editors). [Revised<sup>4</sup> Report on the Algorithmic Language Scheme](https://dl.acm.org/doi/10.1145/382130.382133). *[ACM SIGPLAN LISP Pointers](https://dl.acm.org/toc/sigplan-lisppointers/1991/IV/3)*, IV(3):1--55, 1991.

[<span id="Hanson-1991">6</span>] Chris Hanson. [A Syntactic Closures Macro Facility](https://dl.acm.org/doi/10.1145/1317265.1317267). *[ACM SIGPLAN LISP Pointers](https://dl.acm.org/toc/sigplan-lisppointers/1991/IV/4)*, IV(4):9--16, 1991.

[<span id="Clinger-1991">7</span>] William Clinger. [Hygienic Macros Through Explicit Renaming](https://dl.acm.org/doi/10.1145/1317265.1317269). *[ACM SIGPLAN LISP Pointers](https://dl.acm.org/toc/sigplan-lisppointers/1991/IV/4)*, IV(4):25--28, 1991.

[<span id="Clinger-and-Rees-1991b">8</span>] William Clinger and Jonathan Rees. [Macros That Work](https://dl.acm.org/doi/10.1145/99583.99607). In *[POPL '91: Proceedings of the 18th ACM SIGPLAN-SIGACT Symposium on Principles of Programming Languages](https://dl.acm.org/doi/proceedings/10.1145/99583)*, pages 155--162, 1991.

[<span id="Kelsey-Clinger-and-Rees-1998">9</span>] Rechard Kelsey, William Clinger, and Jonathan Rees (Editors). [Revised<sup>5</sup> Report on the Algorithmic Language Scheme](https://dl.acm.org/doi/10.1145/290229.290234). *[ACM SIGPLAN Notices](https://dl.acm.org/toc/sigplan/1998/33/9)*, 33(9):26--76, 1998.

[<span id="Kempf-2001a">10</span>] William E. Kempf. [A garbage collection framework for C++](https://www.codeproject.com/Articles/912/A-garbage-collection-framework-for-C), 2001.

[<span id="Kempf-2001b">11</span>] William E. Kempf. [A garbage collection framework for C++ - Part II](https://www.codeproject.com/Articles/938/A-garbage-collection-framework-for-C-Part-II), 2001.

[<span id="Adams-and-Dybvig-2008">12</span>] Michael D. Adams and R. Kent Dybvig. [Efficient Nondestructive Equality Checking for Trees and Graphs](https://dl.acm.org/doi/10.1145/1411204.1411230). In *[ICFP '08: Proceedings of the 13th ACM SIGPLAN International Conference on Functional Programming](https://dl.acm.org/doi/proceedings/10.1145/1411204)*, pages 179--188, 2008.

[<span id="Shinn-Cowan-and-Gleckler-2013">13</span>] Alex Shinn, John Cowan, and Arthur A. Gleckler (Editors). [Revised<sup>7</sup> Report on the Algorithmic Language Scheme](https://standards.scheme.org/official/r7rs.pdf). Technical report, http://www.scheme-reports.org/, 2013.
