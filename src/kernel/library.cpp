/*
   Copyright 2018-2022 Tatsuya Yamasaki.

   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at

       http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License.
*/

#include <meevax/kernel/library.hpp>

namespace meevax
{
inline namespace kernel
{
  std::map<std::string, library> libraries {};

  auto bootstrap() -> void
  {
    define_library("(scheme base)", [](library &)
    {
    });

    define_library("(scheme char)", [](library &)
    {
    });

    define_library("(scheme cxr)", [](library & lib)
    {
      /* -------------------------------------------------------------------------
       *
       *  (caaar pair)                                      cxr library procedure
       *  (caadr pair)                                      cxr library procedure
       *       .                                                           .
       *       .                                                           .
       *       .                                                           .
       *  (cdddar pair)                                     cxr library procedure
       *  (cddddr pair)                                     cxr library procedure
       *
       *  These twenty-four procedures are further compositions of car and cdr on
       *  the same principles. For example, caddr could be defined by
       *
       *      (define caddr (lambda (x) (car (cdr (cdr x))))).
       *
       *  Arbitrary compositions up to four deep are provided.
       *
       * ---------------------------------------------------------------------- */

      lib.define<procedure>("caaar", [](let const& xs) { return caaar(car(xs)); });
      lib.define<procedure>("caadr", [](let const& xs) { return caadr(car(xs)); });
      lib.define<procedure>("cadar", [](let const& xs) { return cadar(car(xs)); });
      lib.define<procedure>("caddr", [](let const& xs) { return caddr(car(xs)); });
      lib.define<procedure>("cdaar", [](let const& xs) { return cdaar(car(xs)); });
      lib.define<procedure>("cdadr", [](let const& xs) { return cdadr(car(xs)); });
      lib.define<procedure>("cddar", [](let const& xs) { return cddar(car(xs)); });
      lib.define<procedure>("cdddr", [](let const& xs) { return cdddr(car(xs)); });

      lib.define<procedure>("caaaar", [](let const& xs) { return caaaar(car(xs)); });
      lib.define<procedure>("caaadr", [](let const& xs) { return caaadr(car(xs)); });
      lib.define<procedure>("caadar", [](let const& xs) { return caadar(car(xs)); });
      lib.define<procedure>("caaddr", [](let const& xs) { return caaddr(car(xs)); });
      lib.define<procedure>("cadaar", [](let const& xs) { return cadaar(car(xs)); });
      lib.define<procedure>("cadadr", [](let const& xs) { return cadadr(car(xs)); });
      lib.define<procedure>("caddar", [](let const& xs) { return caddar(car(xs)); });
      lib.define<procedure>("cadddr", [](let const& xs) { return cadddr(car(xs)); });
      lib.define<procedure>("cdaaar", [](let const& xs) { return cdaaar(car(xs)); });
      lib.define<procedure>("cdaadr", [](let const& xs) { return cdaadr(car(xs)); });
      lib.define<procedure>("cdadar", [](let const& xs) { return cdadar(car(xs)); });
      lib.define<procedure>("cdaddr", [](let const& xs) { return cdaddr(car(xs)); });
      lib.define<procedure>("cddaar", [](let const& xs) { return cddaar(car(xs)); });
      lib.define<procedure>("cddadr", [](let const& xs) { return cddadr(car(xs)); });
      lib.define<procedure>("cdddar", [](let const& xs) { return cdddar(car(xs)); });
      lib.define<procedure>("cddddr", [](let const& xs) { return cddddr(car(xs)); });

      lib.export_("caaar",
                  "caadr",
                  "cadar",
                  "caddr",
                  "cdaar",
                  "cdadr",
                  "cddar",
                  "cdddr",
                  "caaaar",
                  "caaadr",
                  "caadar",
                  "caaddr",
                  "cadaar",
                  "cadadr",
                  "caddar",
                  "cadddr",
                  "cdaaar",
                  "cdaadr",
                  "cdadar",
                  "cdaddr",
                  "cddaar",
                  "cddadr",
                  "cdddar",
                  "cddddr");
    });

    define_library("(meevax gc)", [](library & meevax_gc)
    {
      meevax_gc.define<procedure>("gc-collect", [](auto&&...)
      {
        return make<exact_integer>(gc.collect());
      });

      meevax_gc.define<procedure>("gc-count", [](auto&&...)
      {
        return make<exact_integer>(gc.count());
      });

      meevax_gc.export_("gc-collect", "gc-count");
    });

    define_library("(meevax foreign-function-interface)", [](library & meevax_ffi)
    {
      meevax_ffi.define<procedure>("foreign-function", [](let const& xs)
      {
        return make<procedure>(cadr(xs).as<string>(), car(xs).as<string>());
      });

      meevax_ffi.export_("foreign-function");
    });

    define_library("(meevax foo)", [](library & library)
    {
      library.export_("a");
      library.export_("b");
      library.export_("c");

      library.define<procedure>("a", [](let const&)
      {
        LINE();
        return unit;
      });

      library.define("b", make<exact_integer>(42));

      library.define("c", make<symbol>("dummy"));
    });
  }
} // namespace kernel
} // namespace meevax
