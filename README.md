## Overview

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

- Compilation through continuation-passing style (CPS), targeting a stack-based
  virtual machine.
- Low-level hygienic macro system, known as [*syntactic closures*][Bawden-and-Rees-1988]
  and [*explicit renaming*][Clinger-1991]. For these, the well-known [macro transformers][Hanson-1991]
  `sc-macro-transformer`, `rsc-macro-transformer` and `er-macro-transformer`
  from the library [`(meevax macro-transformer)`](./library/meevax.ss) are
  provided. Note that these are non-Scheme standards.
- [C++ friendly precise garbage collection][Kempf-2001a]

### Standards

Meevax can be used as an interpreter that supports the Scheme standard specified by the following report:

- [Revised<sup>4</sup> Report on the Algorithmic Language Scheme (R4RS)][Clinger-and-Rees-1991a]
- [Revised<sup>5</sup> Report on the Algorithmic Language Scheme (R5RS)][Kelsey-Clinger-and-Rees-1998]
- [Revised<sup>7</sup> Report on the Algorithmic Language Scheme (R7RS)][Shinn-Cowan-and-Gleckler-2013]

Procedures for each standard are provided by the following R7RS-style libraries:

| Language | Library name |
|:--------:|--------------|
| R4RS     | [`(scheme r4rs)`](./library/r4rs.ss)
| R5RS     | [`(scheme r5rs)`](./library/r5rs.ss)
| R7RS     | [`(scheme base)`](./library/r7rs.ss) [`(scheme bitwise)`](./library/r7rs.ss) [`(scheme box)`](./library/r7rs.ss) [`(scheme case-lambda)`](./library/r7rs.ss) [`(scheme char)`](./library/r7rs.ss) [`(scheme complex)`](./library/r7rs.ss) [`(scheme cxr)`](./library/r7rs.ss) [`(scheme division)`](./library/r7rs.ss) [`(scheme eval)`](./library/r7rs.ss) [`(scheme file)`](./library/r7rs.ss) [`(scheme fixnum)`](./library/r7rs.ss) [`(scheme flonum)`](./library/r7rs.ss) [`(scheme inexact)`](./library/r7rs.ss) [`(scheme lazy)`](./library/r7rs.ss) [`(scheme list)`](./library/r7rs.ss) [`(scheme load)`](./library/r7rs.ss) [`(scheme process-context)`](./library/r7rs.ss) [`(scheme read)`](./library/r7rs.ss) [`(scheme repl)`](./library/r7rs.ss) [`(scheme time)`](./library/r7rs.ss) [`(scheme write)`](./library/r7rs.ss)

### SRFIs

| #                                                       | Title                                                  | Library name                          | Note                                     |
|--------------------------------------------------------:|--------------------------------------------------------|---------------------------------------|------------------------------------------|
| [  0](https://srfi.schemers.org/srfi-0/srfi-0.html)     | Feature-based conditional expansion construct          | [`(srfi 0)`](./library/srfi-0.ss)     | R7RS 4.2.1                               |
| [  1](https://srfi.schemers.org/srfi-1/srfi-1.html)     | List Library                                           | [`(srfi 1)`](./library/srfi-1.ss)     | [`(scheme list)`](./library/r7rs.ss)     |
| [  4](https://srfi.schemers.org/srfi-4/srfi-4.html)     | Homogeneous numeric vector datatypes                   | [`(srfi 4)`](./library/srfi-4.ss)     | R7RS 6.9                                 |
| [  6](https://srfi.schemers.org/srfi-6/srfi-6.html)     | Basic String Ports                                     | [`(srfi 6)`](./library/srfi-6.ss)     | R7RS 6.13                                |
| [  8](https://srfi.schemers.org/srfi-8/srfi-8.html)     | receive: Binding to multiple values                    | [`(srfi 8)`](./library/srfi-8.ss)     |                                          |
| [  9](https://srfi.schemers.org/srfi-9/srfi-9.html)     | Defining Record Types                                  | [`(srfi 9)`](./library/srfi-9.ss)     | R7RS 5.5                                 |
| [ 10](https://srfi.schemers.org/srfi-10/srfi-10.html)   | #, external form                                       |                                       |                                          |
| [ 11](https://srfi.schemers.org/srfi-11/srfi-11.html)   | Syntax for receiving multiple values                   | [`(srfi 11)`](./library/srfi-11.ss)   | R7RS 4.2.2                               |
| [ 16](https://srfi.schemers.org/srfi-16/srfi-16.html)   | Syntax for procedures of variable arity                | [`(srfi 16)`](./library/srfi-16.ss)   | R7RS 4.2.9                               |
| [ 23](https://srfi.schemers.org/srfi-23/srfi-23.html)   | Error reporting mechanism                              | [`(srfi 23)`](./library/srfi-23.ss)   | R7RS 6.11                                |
| [ 30](https://srfi.schemers.org/srfi-30/srfi-30.html)   | Nested Multi-line Comments                             |                                       | R7RS 2.2                                 |
| [ 31](https://srfi.schemers.org/srfi-31/srfi-31.html)   | A special form rec for recursive evaluation            | [`(srfi 31)`](./library/srfi-31.ss)   |                                          |
| [ 34](https://srfi.schemers.org/srfi-34/srfi-34.html)   | Exception Handling for Programs                        | [`(srfi 34)`](./library/srfi-34.ss)   | R7RS 6.11                                |
| [ 38](https://srfi.schemers.org/srfi-38/srfi-38.html)   | External Representation for Data With Shared Structure | [`(srfi 38)`](./library/srfi-38.ss)   | R7RS 6.13.3                              |
| [ 39](https://srfi.schemers.org/srfi-39/srfi-39.html)   | Parameter objects                                      | [`(srfi 39)`](./library/srfi-39.ss)   | R7RS 4.2.6                               |
| [ 45](https://srfi.schemers.org/srfi-45/srfi-45.html)   | Primitives for Expressing Iterative Lazy Algorithms    | [`(srfi 45)`](./library/srfi-45.ss)   | R7RS 4.2.5                               |
| [ 62](https://srfi.schemers.org/srfi-62/srfi-62.html)   | S-expression comments                                  |                                       | R7RS 2.2                                 |
| [ 78](https://srfi.schemers.org/srfi-78/srfi-78.html)   | Lightweight testing                                    | [`(srfi 78)`](./library/srfi-78.ss)   | Except `check-ec`                        |
| [ 87](https://srfi.schemers.org/srfi-87/srfi-87.html)   | => in case clauses                                     |                                       | R7RS 4.2.1                               |
| [ 98](https://srfi.schemers.org/srfi-98/srfi-98.html)   | An interface to access environment variables           | [`(srfi 98)`](./library/srfi-98.ss)   | R7RS 6.14                                |
| [111](https://srfi.schemers.org/srfi-111/srfi-111.html) | Boxes                                                  | [`(srfi 111)`](./library/srfi-111.ss) | [`(scheme box)`](./library/r7rs.ss)      |
| [138](https://srfi.schemers.org/srfi-138/srfi-138.html) | Compiling Scheme programs to executables               |                                       |                                          |
| [141](https://srfi.schemers.org/srfi-141/srfi-141.html) | Integer division                                       | [`(srfi 141)`](./library/srfi-141.ss) | [`(scheme division)`](./library/r7rs.ss) |
| [143](https://srfi.schemers.org/srfi-143/srfi-143.html) | Fixnums                                                | [`(srfi 143)`](./library/srfi-143.ss) | [`(scheme fixnum)`](./library/r7rs.ss)   |
| [144](https://srfi.schemers.org/srfi-144/srfi-144.html) | Flonums                                                | [`(srfi 144)`](./library/srfi-144.ss) | [`(scheme flonum)`](./library/r7rs.ss)   |
| [149](https://srfi.schemers.org/srfi-149/srfi-149.html) | Basic syntax-rules template extensions                 | [`(srfi 149)`](./library/srfi-149.ss) | R7RS 4.3.2                               |
| [151](https://srfi.schemers.org/srfi-151/srfi-151.html) | Bitwise Operations                                     | [`(srfi 151)`](./library/srfi-151.ss) | [`(scheme bitwise)`](./library/r7rs.ss)  |

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
| `all`       | Build shared-library `libmeevax.0.6.0.so` and executable `meevax`.
| `install`   | Copy files into `/usr/local` directly.
| `package`   | Generate debian package `meevax_0.6.0_amd64.deb` (only Ubuntu). The generated package can be installed by `sudo apt install build/meevax_0.6.0_amd64.deb`.
| `test`      | Test executable `meevax`. This target requires Valgrind to be installed.
| `uninstall` | Remove files copied to `/usr/local` directly by target `install`.

## License

See [LICENSE](./LICENSE).

<!--
  @ARTICLE {
    McCarthy-1960,
    author  = "John McCarthy",
    title   = "\href{https://dl.acm.org/doi/10.1145/367177.367199}{Recursive functions of symbolic expressions and their computation by machine, Part I}",
    journal = "\href{https://dl.acm.org/toc/cacm/1960/3/4}{Communications of the ACM}",
    year    = "1960",
    volume  = "3",
    number  = "4",
    pages   = "184-195"
  }
-->
[McCarthy-1960]: https://dl.acm.org/doi/10.1145/367177.367199

<!--
  @ARTICLE {
    Landin-1964,
    author  = "Peter J. Landin",
    title   = "\href{https://academic.oup.com/comjnl/article/6/4/308/375725}{The Mechanical Evaluation of Expressions}",
    journal = "\href{https://academic.oup.com/comjnl/issue/6/4}{The Computor Journal}",
    year    = "1964",
    volume  = "6",
    number  = "4",
    pages   = "308-320",
    note    = "Reference for the earlier SECD implementation."
  }
-->
[Landin-1964]: https://doi.org/10.1093/comjnl/6.4.308

<!--
  @BOOK {
    Henderson-1980,
    author    = "Peter Henderson",
    title     = "\href{https://archive.org/details/functionalprogra0000hend/mode/2up}{Functional Programming: Application and Implementation}",
    publisher = "Prentice Hall",
    year      = "1980"
  }
-->
[Henderson-1980]: https://archive.org/details/functionalprogra0000hend/mode/2up

<!--
  @INPROCEEDINGS {
    Bawden-and-Rees-1988,
    author    = "Alan Bawden and Jonathan Rees",
    title     = "\href{https://dl.acm.org/doi/10.1145/62678.62687}{Syntactic Closures}",
    booktitle = "\href{https://dl.acm.org/doi/proceedings/10.1145/62678}{LFP '88: Proceedings of the 1988 ACM Conference on LISP and Functional Programming}",
    year      = "1988",
    pages     = "86-95"
  }
-->
[Bawden-and-Rees-1988]: https://dl.acm.org/doi/10.1145/62678.62687

<!--
  @ARTICLE {
    Clinger-and-Rees-1991a,
    author  = "William Clinger and Jonathan Rees (Editors)",
    title   = "\href{https://dl.acm.org/doi/10.1145/382130.382133}{Revised$^4$ Report on the Algorithmic Language Scheme}",
    journal = "\href{https://dl.acm.org/toc/sigplan-lisppointers/1991/IV/3}{ACM SIGPLAN LISP Pointers}",
    year    = "1991",
    volume  = "IV",
    number  = "3",
    pages   = "1-55"
  }
-->
[Clinger-and-Rees-1991a]: https://dl.acm.org/doi/10.1145/382130.382133

<!--
  @ARTICLE {
    Hanson-1991,
    author  = "Chris Hanson",
    title   = "\href{https://dl.acm.org/doi/10.1145/1317265.1317267}{A Syntactic Closures Macro Facility}",
    journal = "\href{https://dl.acm.org/toc/sigplan-lisppointers/1991/IV/4}{ACM SIGPLAN LISP Pointers}",
    year    = "1991",
    volume  = "IV",
    number  = "4",
    pages   = "9-16"
  }
-->
[Hanson-1991]: https://dl.acm.org/doi/10.1145/1317265.1317267

<!--
  @ARTICLE {
    Clinger-1991,
    author  = "William Clinger",
    title   = "\href{https://dl.acm.org/doi/10.1145/1317265.1317269}{Hygienic Macros Through Explicit Renaming}",
    journal = "\href{https://dl.acm.org/toc/sigplan-lisppointers/1991/IV/4}{ACM SIGPLAN LISP Pointers}",
    year    = "1991",
    volume  = "IV",
    number  = "4",
    pages   = "25-28"
  }
-->
[Clinger-1991]: https://dl.acm.org/doi/10.1145/1317265.1317269

<!--
  @INPROCEEDINGS {
    Clinger-and-Rees-1991b,
    author    = "William Clinger and Jonathan Rees",
    title     = "\href{https://dl.acm.org/doi/10.1145/99583.99607}{Macros That Work}",
    booktitle = "\href{https://dl.acm.org/doi/proceedings/10.1145/99583}{POPL '91: Proceedings of the 18th ACM SIGPLAN-SIGACT Symposium on Principles of Programming Languages}",
    year      = "1991",
    pages     = "155-162"
  }
-->
[Clinger-and-Rees-1991b]: https://dl.acm.org/doi/10.1145/99583.99607

<!--
  @ARTICLE {
    Kelsey-Clinger-and-Rees-1998,
    author  = "Rechard Kelsey and William Clinger and Jonathan Rees (Editors)",
    title   = "\href{https://dl.acm.org/doi/10.1145/290229.290234}{Revised$^5$ Report on the Algorithmic Language Scheme}",
    journal = "\href{https://dl.acm.org/toc/sigplan/1998/33/9}{ACM SIGPLAN Notices}",
    year    = "1998",
    volume  = "33",
    number  = "9",
    pages   = "26-76"
  }
-->
[Kelsey-Clinger-and-Rees-1998]: https://dl.acm.org/doi/10.1145/290229.290234

<!--
  @ONLINE {
    Kempf-2001a,
    author = "William E. Kempf",
    title  = "\href{https://www.codeproject.com/Articles/912/A-garbage-collection-framework-for-C}{A garbage collection framework for C++}",
    year   = "2001"
  }
-->
[Kempf-2001a]: https://www.codeproject.com/Articles/912/A-garbage-collection-framework-for-C

<!--
  @MISC {
    Kempf-2001b,
    author = "William E. Kempf",
    title  = "\href{https://www.codeproject.com/Articles/938/A-garbage-collection-framework-for-C-Part-II}{A garbage collection framework for C++ - Part II}",
    year   = "2001"
  }
-->
[Kempf-2001b]: https://www.codeproject.com/Articles/938/A-garbage-collection-framework-for-C-Part-II

<!--
  @INPROCEEDINGS {
    Adams-and-Dybvig-2008,
    author    = "Michael D. Adams and R. Kent Dybvig",
    title     = "\href{https://dl.acm.org/doi/10.1145/1411204.1411230}{Efficient Nondestructive Equality Checking for Trees and Graphs}",
    booktitle = "\href{https://dl.acm.org/doi/proceedings/10.1145/1411204}{ICFP '08: Proceedings of the 13th ACM SIGPLAN International Conference on Functional Programming}",
    year      = "2008",
    pages     = "179-188"
  }
-->
[Adams-and-Dybvig-2008]: https://dl.acm.org/doi/10.1145/1411204.1411230

<!--
  @TECHREPORT {
    Shinn-Cowan-and-Gleckler-2013,
    author      = "Alex Shinn and John Cowan and Arthur A. Gleckler (Editors)",
    title       = "\href{https://standards.scheme.org/official/r7rs.pdf}{Revised$^7$ Report on the Algorithmic Language Scheme}",
    institution = "http://www.scheme-reports.org/",
    year        = "2013"
  }
-->
[Shinn-Cowan-and-Gleckler-2013]: https://standards.scheme.org/official/r7rs.pdf
