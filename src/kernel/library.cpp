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

#include <functional>
#include <meevax/kernel/basis.hpp>
#include <meevax/kernel/interaction_environment.hpp>
#include <meevax/kernel/library.hpp>
#include <stdexcept>

namespace meevax
{
inline namespace kernel
{
  library::library(const_reference declarations)
    : declarations { declarations }
  {}

  auto library::boot() -> void
  {
    define_library("(meevax character)", [](library & library)
    {
      library.define<predicate>("char?", [](let const& xs)
      {
        return car(xs).is<character>();
      });

      library.define<procedure>("char->integer", [](let const& xs)
      {
        return make<exact_integer>(car(xs).as<character>().codepoint);
      });

      library.define<procedure>("char-codepoint", [](let const& xs) -> value_type
      {
        if (auto c = car(xs).as<character>(); std::isdigit(c.codepoint))
        {
          return make<exact_integer>(c.codepoint - '0');
        }
        else
        {
          return f;
        }
      });

      library.export_("char?");
      library.export_("char->integer");
      library.export_("char-codepoint");
    });

    define_library("(meevax complex)", [](library & library)
    {
      library.define<procedure>("make-rectangular", [](let const& xs)
      {
        assert(apply<is_real>(car(xs)));
        assert(apply<is_real>(cadr(xs)));

        return make<complex>(car(xs), cadr(xs));
      });

      library.define<procedure>("real-part", [](let const& xs)
      {
        return car(xs).as<complex>().real();
      });

      library.define<procedure>("imag-part", [](let const& xs)
      {
        return car(xs).as<complex>().imag();
      });

      library.export_("make-rectangular");
      library.export_("real-part");
      library.export_("imag-part");
    });

    define_library("(meevax context)", [](library & library)
    {
      library.define<procedure>("emergency-exit", [](let const& xs) -> value_type
      {
        if (let const& status = car(xs); status.is<null>())
        {
          throw success;
        }
        else if (status.is<bool>())
        {
          throw select(status) ? success : failure;
        }
        else
        {
          throw static_cast<int>(status.as<exact_integer>());
        }
      });

      library.export_("emergency-exit");
    });

    define_library("(meevax function)", [](library & library)
    {
      library.define<predicate>("closure?", [](let const& xs)
      {
        return car(xs).is<closure>();
      });

      library.define<predicate>("continuation?", [](let const& xs)
      {
        return car(xs).is<continuation>();
      });

      library.define<procedure>("foreign-function", [](let const& xs)
      {
        return make<procedure>(cadr(xs).as<string>(), car(xs).as<string>());
      });

      library.define<predicate>("foreign-function?", [](let const& xs)
      {
        return car(xs).is<procedure>();
      });

      library.export_("closure?");
      library.export_("continuation?");
      library.export_("foreign-function");
      library.export_("foreign-function?");
    });

    define_library("(meevax environment)", [](library & library)
    {
      library.define<procedure>("environment", [](let const& xs)
      {
        let const e = make<environment>();

        for (let const& x : xs)
        {
          e.as<environment>().import_(x);
        }

        return e;
      });

      library.define<procedure>("eval", [](let const& xs)
      {
        return cadr(xs).as<environment>().evaluate(car(xs));
      });

      library.define<procedure>("interaction-environment", [](auto&&...)
      {
        return interaction_environment();
      });

      library.define<procedure>("load", [](let const& xs)
      {
        return car(xs).as<environment>().load(cadr(xs).as<string>());
      });

      library.export_("environment");
      library.export_("eval");
      library.export_("interaction-environment");
      library.export_("load");
    });

    define_library("(meevax dynamic-environment)", [](library & library)
    {
      library.define<syntax>("store-auxiliary", store_auxiliary);
      library.define<syntax>("load-auxiliary", load_auxiliary);

      library.export_("store-auxiliary");
      library.export_("load-auxiliary");
    });

    define_library("(meevax comparator)", [](library & library)
    {
      library.define<predicate>("identity=?", [](let const& xs)
      {
        return eq(car(xs), cadr(xs));
      });

      library.define<predicate>("normally=?", [](let const& xs)
      {
        return eqv(car(xs), cadr(xs));
      });

      library.export_("identity=?");
      library.export_("normally=?");
    });

    define_library("(meevax error)", [](library & library)
    {
      library.define<procedure>("throw", [](let const& xs) -> value_type
      {
        throw car(xs);
      });

      library.define<procedure>("make-error", [](let const& xs)
      {
        return make<error>(car(xs), cdr(xs));
      });

      library.define<predicate>("error?", [](let const& xs)
      {
        return car(xs).is<error>();
      });

      library.define<predicate>("read-error?", [](let const& xs)
      {
        return car(xs).is<read_error>();
      });

      library.define<predicate>("file-error?", [](let const& xs)
      {
        return car(xs).is<file_error>();
      });

      library.export_("throw");
      library.export_("make-error");
      library.export_("error?");
      library.export_("read-error?");
      library.export_("file-error?");
    });

    define_library("(meevax experimental)", [](library & library)
    {
      library.define<procedure>("type-of", [](let const& xs)
      {
        std::cout << car(xs).type().name() << std::endl;
        return standard_output;
      });

      library.define<procedure>("disassemble", [](let const& xs)
      {
        if (0 < length(xs))
        {
          if (let const& f = car(xs); f.is<closure>())
          {
            disassemble(std::cout, car(f));
          }
        }

        return standard_output;
      });

      library.define<predicate>("ieee-float?", [](auto&&...)
      {
        return std::numeric_limits<double>::is_iec559;
      });

      library.export_("type-of");
      library.export_("disassemble");
      library.export_("ieee-float?");
    });

    define_library("(meevax garbage-collector)", [](library & library)
    {
      library.define<procedure>("gc-collect", [](auto&&...)
      {
        return make<exact_integer>(gc.collect());
      });

      library.define<procedure>("gc-count", [](auto&&...)
      {
        return make<exact_integer>(gc.count());
      });

      library.export_("gc-collect");
      library.export_("gc-count");
    });

    define_library("(meevax inexact)", [](library & library)
    {
      library.define<procedure>("finite?", [](let const& xs)
      {
        try
        {
          return apply<is_finite>(car(xs));
        }
        catch (std::out_of_range const&)
        {
          return f;
        }
      });

      library.define<procedure>("infinite?", [](let const& xs)
      {
        try
        {
          return apply<is_infinite>(car(xs));
        }
        catch (std::out_of_range const&)
        {
          return f;
        }
      });

      library.define<procedure>("nan?", [](let const& xs)
      {
        try
        {
          return apply<is_nan>(car(xs));
        }
        catch (std::out_of_range const&)
        {
          return f;
        }
      });

      library.define<procedure>("exp", [](let const& xs)
      {
        return apply<exp>(car(xs));
      });

      library.define<procedure>("sqrt", [](let const& xs)
      {
        return apply<sqrt>(car(xs));
      });

      library.define<procedure>("log", [](let const& xs)
      {
        return cdr(xs).is<pair>() ? apply<log>(car(xs)) / apply<log>(cadr(xs))
                                  : apply<log>(car(xs));
      });

      library.define<procedure>("sin", [](let const& xs)
      {
        return apply<sin>(car(xs));
      });

      library.define<procedure>("cos", [](let const& xs)
      {
        return apply<cos>(car(xs));
      });

      library.define<procedure>("tan", [](let const& xs)
      {
        return apply<tan>(car(xs));
      });

      library.define<procedure>("asin", [](let const& xs)
      {
        return apply<asin>(car(xs));
      });

      library.define<procedure>("acos", [](let const& xs)
      {
        return apply<acos>(car(xs));
      });

      library.define<procedure>("atan", [](let const& xs)
      {
        return cdr(xs).is<pair>() ? apply<atan2>(car(xs), cadr(xs))
                                  : apply<atan>(car(xs));
      });

      library.define<procedure>("sinh", [](let const& xs)
      {
        return apply<sinh>(car(xs));
      });

      library.define<procedure>("cosh", [](let const& xs)
      {
        return apply<cosh>(car(xs));
      });

      library.define<procedure>("tanh", [](let const& xs)
      {
        return apply<tanh>(car(xs));
      });

      library.define<procedure>("asinh", [](let const& xs)
      {
        return apply<asinh>(car(xs));
      });

      library.define<procedure>("acosh", [](let const& xs)
      {
        return apply<acosh>(car(xs));
      });

      library.define<procedure>("atanh", [](let const& xs)
      {
        return apply<atanh>(car(xs));
      });

      library.export_("finite?");
      library.export_("infinite?");
      library.export_("nan?");
      library.export_("exp");
      library.export_("sqrt");
      library.export_("log");
      library.export_("sin");
      library.export_("asin");
      library.export_("sinh");
      library.export_("asinh");
      library.export_("cos");
      library.export_("acos");
      library.export_("cosh");
      library.export_("acosh");
      library.export_("tan");
      library.export_("atan");
      library.export_("tanh");
      library.export_("atanh");
    });

    define_library("(meevax list)", [](library & library)
    {
      library.define<predicate>("null?", [](let const& xs)
      {
        return car(xs).is<null>();
      });

      library.define<procedure>("append", [](let const& xs)
      {
        return std::accumulate(std::begin(xs), std::end(xs), unit, append2);
      });

      library.define<procedure>("list->vector", [](let const& xs)
      {
        return make<vector>(car(xs));
      });

      library.export_("null?");
      library.export_("append");
      library.export_("list->vector");
    });

    define_library("(meevax macro)", [](library & library)
    {
      library.define<predicate>("identifier?", [](let const& xs)
      {
        return car(xs).is_also<identifier>();
      });

      library.define<procedure>("identifier->symbol", [](let const& xs)
      {
        if (let const& x = car(xs); x.is<syntactic_closure>())
        {
          return x.as<syntactic_closure>().expression;
        }
        else
        {
          return x;
        }
      });

      library.define<predicate>("transformer?", [](let const& xs)
      {
        return car(xs).is_also<transformer>();
      });

      library.define<predicate>("syntactic-closure?", [](let const& xs)
      {
        return car(xs).is<syntactic_closure>();
      });

      library.define<procedure>("make-syntactic-closure", [](let const& xs)
      {
        return make<syntactic_closure>(car(xs), cadr(xs), caddr(xs));
      });

      library.export_("identifier?");
      library.export_("identifier->symbol");
      library.export_("transformer?");
      library.export_("syntactic-closure?");
      library.export_("make-syntactic-closure");
    });

    define_library("(meevax number)", [](library & library)
    {
      library.define<procedure>("number?", [](let const& xs)
      {
        try
        {
          return apply<is_complex>(car(xs));
        }
        catch (std::out_of_range const&)
        {
          return f;
        }
      });

      library.define<procedure>("complex?", [](let const& xs)
      {
        try
        {
          return apply<is_complex>(car(xs));
        }
        catch (std::out_of_range const&)
        {
          return f;
        }
      });

      library.define<procedure>("real?", [](let const& xs)
      {
        try
        {
          return apply<is_real>(car(xs));
        }
        catch (std::out_of_range const&)
        {
          return f;
        }
      });

      library.define<procedure>("rational?", [](let const& xs)
      {
        try
        {
          return apply<is_rational>(car(xs));
        }
        catch (std::out_of_range const&)
        {
          return f;
        }
      });

      library.define<procedure>("integer?", [](let const& xs)
      {
        try
        {
          return apply<is_integer>(car(xs));
        }
        catch (std::out_of_range const&)
        {
          return f;
        }
      });

      library.define<predicate>("exact-integer?", [](let const& xs)
      {
        return car(xs).is<exact_integer>();
      });

      library.define<predicate>("%complex?", [](let const& xs)
      {
        return car(xs).is<complex>();
      });

      library.define<predicate>("ratio?", [](let const& xs)
      {
        return car(xs).is<ratio>();
      });

      library.define<predicate>("single-float?", [](let const& xs)
      {
        return car(xs).is<float>();
      });

      library.define<predicate>("double-float?", [](let const& xs)
      {
        return car(xs).is<double>();
      });

      #define DEFINE(SYMBOL, COMPARE)                                          \
      library.define<predicate>(#SYMBOL, [](let const& xs)                     \
      {                                                                        \
        return std::adjacent_find(                                             \
                 std::begin(xs), std::end(xs), [](let const& a, let const& b)  \
                 {                                                             \
                   return not apply<COMPARE>(a, b).as<bool>();                 \
                 }) == std::end(xs);                                           \
      })

      DEFINE(= , equal_to     );
      DEFINE(< , less         );
      DEFINE(<=, less_equal   );
      DEFINE(> , greater      );
      DEFINE(>=, greater_equal);

      #undef DEFINE

      library.define<procedure>("+", [](let const& xs)
      {
        return std::accumulate(std::begin(xs), std::end(xs), e0, plus());
      });

      library.define<procedure>("*", [](let const& xs)
      {
        return std::accumulate(std::begin(xs), std::end(xs), e1, multiplies());
      });

      #define DEFINE(SYMBOL, FUNCTION, BASIS)                                  \
      library.define<procedure>(SYMBOL, [](let const& xs)                      \
      {                                                                        \
        return cdr(xs).is<pair>() ? std::accumulate(std::next(std::begin(xs)), std::end(xs), car(xs), [](auto&& a, auto&& b) \
                                    {                                          \
                                      return FUNCTION()(a, b);                 \
                                    })                                         \
                                  : FUNCTION()(BASIS, car(xs));                \
      })

      DEFINE("-", minus  , e0);
      DEFINE("/", divides, e1);
      DEFINE("%", modulus, e1);

      #undef DEFINE

      library.define<procedure>("ratio-numerator", [](let const& xs)
      {
        return make(car(xs).as<ratio>().numerator());
      });

      library.define<procedure>("ratio-denominator", [](let const& xs)
      {
        return make(car(xs).as<ratio>().denominator());
      });

      library.define<procedure>("floor", [](let const& xs)
      {
        return apply<floor>(car(xs));
      });

      library.define<procedure>("ceiling", [](let const& xs)
      {
        return apply<ceil>(car(xs));
      });

      library.define<procedure>("truncate", [](let const& xs)
      {
        return apply<trunc>(car(xs));
      });

      library.define<procedure>("round", [](let const& xs)
      {
        return apply<round>(car(xs));
      });

      library.define<procedure>("exact", [](let const& xs)
      {
        return apply<exact>(car(xs));
      });

      library.define<procedure>("inexact", [](let const& xs)
      {
        return apply<inexact>(car(xs));
      });

      library.define<procedure>("expt", [](let const& xs)
      {
        return apply<expt>(car(xs), cadr(xs));
      });

      library.define<procedure>("integer->char", [](let const& xs)
      {
        return make<character>(car(xs).as<exact_integer>());
      });

      library.define<procedure>("number->string", [](let const& xs)
      {
        switch (cdr(xs).is<pair>() ? cadr(xs).as<exact_integer>() : 10)
        {
        case 2:
          return apply<number_to_string<2>>(car(xs));

        case 8:
          return apply<number_to_string<8>>(car(xs));

        case 10: default:
          return apply<number_to_string<10>>(car(xs));

        case 16:
          return apply<number_to_string<16>>(car(xs));
        }
      });

      library.export_("number?");
      library.export_("complex?");
      library.export_("real?");
      library.export_("rational?");
      library.export_("integer?");
      library.export_("exact-integer?");
      library.export_("%complex?");
      library.export_("ratio?");
      library.export_("single-float?");
      library.export_("double-float?");
      library.export_("=");
      library.export_("!=");
      library.export_("<");
      library.export_("<=");
      library.export_(">");
      library.export_(">=");
      library.export_("+");
      library.export_("*");
      library.export_("-");
      library.export_("/");
      library.export_("%");
      library.export_("ratio-numerator");
      library.export_("ratio-denominator");
      library.export_("floor");
      library.export_("ceiling");
      library.export_("truncate");
      library.export_("round");
      library.export_("expt");
      library.export_("exact");
      library.export_("inexact");
      library.export_("integer->char");
      library.export_("number->string");
    });

    define_library("(meevax pair)", [](library & library)
    {
      library.define<predicate>("pair?", [](let const& xs)
      {
        return car(xs).is<pair>();
      });

      library.define<intrinsic>("cons", cons_, [](let const& xs)
      {
        return cons(car(xs), cadr(xs));
      });

      library.define<procedure>("car", [](let const& xs) { return caar(xs); });
      library.define<procedure>("cdr", [](let const& xs) { return cdar(xs); });

      library.define<procedure>("caar", [](let const& xs) { return caar(car(xs)); });
      library.define<procedure>("cadr", [](let const& xs) { return cadr(car(xs)); });
      library.define<procedure>("cdar", [](let const& xs) { return cdar(car(xs)); });
      library.define<procedure>("cddr", [](let const& xs) { return cddr(car(xs)); });

      library.define<procedure>("caaar", [](let const& xs) { return caaar(car(xs)); });
      library.define<procedure>("caadr", [](let const& xs) { return caadr(car(xs)); });
      library.define<procedure>("cadar", [](let const& xs) { return cadar(car(xs)); });
      library.define<procedure>("caddr", [](let const& xs) { return caddr(car(xs)); });
      library.define<procedure>("cdaar", [](let const& xs) { return cdaar(car(xs)); });
      library.define<procedure>("cdadr", [](let const& xs) { return cdadr(car(xs)); });
      library.define<procedure>("cddar", [](let const& xs) { return cddar(car(xs)); });
      library.define<procedure>("cdddr", [](let const& xs) { return cdddr(car(xs)); });

      library.define<procedure>("caaaar", [](let const& xs) { return caaaar(car(xs)); });
      library.define<procedure>("caaadr", [](let const& xs) { return caaadr(car(xs)); });
      library.define<procedure>("caadar", [](let const& xs) { return caadar(car(xs)); });
      library.define<procedure>("caaddr", [](let const& xs) { return caaddr(car(xs)); });
      library.define<procedure>("cadaar", [](let const& xs) { return cadaar(car(xs)); });
      library.define<procedure>("cadadr", [](let const& xs) { return cadadr(car(xs)); });
      library.define<procedure>("caddar", [](let const& xs) { return caddar(car(xs)); });
      library.define<procedure>("cadddr", [](let const& xs) { return cadddr(car(xs)); });
      library.define<procedure>("cdaaar", [](let const& xs) { return cdaaar(car(xs)); });
      library.define<procedure>("cdaadr", [](let const& xs) { return cdaadr(car(xs)); });
      library.define<procedure>("cdadar", [](let const& xs) { return cdadar(car(xs)); });
      library.define<procedure>("cdaddr", [](let const& xs) { return cdaddr(car(xs)); });
      library.define<procedure>("cddaar", [](let const& xs) { return cddaar(car(xs)); });
      library.define<procedure>("cddadr", [](let const& xs) { return cddadr(car(xs)); });
      library.define<procedure>("cdddar", [](let const& xs) { return cdddar(car(xs)); });
      library.define<procedure>("cddddr", [](let const& xs) { return cddddr(car(xs)); });

      library.define<procedure>("set-car!", [](auto&& xs) { return caar(xs) = cadr(xs); });
      library.define<procedure>("set-cdr!", [](auto&& xs) { return cdar(xs) = cadr(xs); });

      library.export_("pair?");
      library.export_("cons");
      library.export_("car");
      library.export_("cdr");
      library.export_("set-car!");
      library.export_("set-cdr!");
      library.export_("caaaar");
      library.export_("caaadr");
      library.export_("caaar");
      library.export_("caadar");
      library.export_("caaddr");
      library.export_("caadr");
      library.export_("caar");
      library.export_("cadaar");
      library.export_("cadadr");
      library.export_("cadar");
      library.export_("caddar");
      library.export_("cadddr");
      library.export_("caddr");
      library.export_("cadr");
      library.export_("cdaaar");
      library.export_("cdaadr");
      library.export_("cdaar");
      library.export_("cdadar");
      library.export_("cdaddr");
      library.export_("cdadr");
      library.export_("cdar");
      library.export_("cddaar");
      library.export_("cddadr");
      library.export_("cddar");
      library.export_("cdddar");
      library.export_("cddddr");
      library.export_("cdddr");
      library.export_("cddr");
    });

    define_library("(meevax port)", [](library & library)
    {
      library.define<predicate>("input-port?", [](let const& xs)
      {
        return car(xs).is_also<std::istream>();
      });

      library.define<predicate>("output-port?", [](let const& xs)
      {
        return car(xs).is_also<std::ostream>();
      });

      library.define<predicate>("binary-port?", [](auto&&...)
      {
        return false;
      });

      library.define<predicate>("textual-port?", [](let const& xs)
      {
        return car(xs).is_also<std::ios>();
      });

      library.define<predicate>("port?", [](let const& xs)
      {
        return car(xs).is_also<std::ios>();
      });

      library.define<predicate>("input-port-open?", [](let const& xs)
      {
        if (let const& x = car(xs); x.is_also<std::ifstream>())
        {
          return x.as<std::ifstream>().is_open();
        }
        else
        {
          return x.is_also<std::istream>();
        }
      });

      library.define<predicate>("output-port-open?", [](let const& xs)
      {
        if (let const& x = car(xs); x.is_also<std::ofstream>())
        {
          return x.as<std::ofstream>().is_open();
        }
        else
        {
          return x.is_also<std::ostream>();
        }
      });

      library.define<procedure>("standard-input-port", [](auto&&...)
      {
        return standard_input;
      });

      library.define<procedure>("standard-output-port", [](auto&&...)
      {
        return standard_output;
      });

      library.define<procedure>("standard-error-port", [](auto&&...)
      {
        return standard_error;
      });

      library.define<procedure>("open-input-file", [](let const& xs)
      {
        return make<input_file_port>(car(xs).as<string>());
      });

      library.define<procedure>("open-output-file", [](let const& xs)
      {
        return make<output_file_port>(car(xs).as<string>());
      });

      library.define<procedure>("close-input-port", [](let const& xs)
      {
        if (let const& x = car(xs); x.is_also<std::ifstream>())
        {
          x.as<std::ifstream>().close();
        }

        return unspecified;
      });

      library.define<procedure>("close-output-port", [](let const& xs)
      {
        if (let const& x = car(xs); x.is_also<std::ofstream>())
        {
          x.as<std::ofstream>().close();
        }

        return unspecified;
      });

      library.define<procedure>("input-string-open", [](let const& xs)
      {
        return cdr(xs).is<pair>() ? make<input_string_port>(car(xs).as<string>())
                                  : make<input_string_port>();
      });

      library.define<procedure>("open-output-string", [](let const& xs)
      {
        return cdr(xs).is<pair>() ? make<output_string_port>(car(xs).as<string>())
                                  : make<output_string_port>();
      });

      library.define<procedure>("get-output-string", [](let const& xs)
      {
        return make<string>(car(xs).as<std::ostringstream>().str());
      });

      library.define<predicate>("get-ready?", [](let const& xs)
      {
        return static_cast<bool>(car(xs).as<std::istream>());
      });

      library.define<procedure>("get-char", [](let const& xs) -> value_type
      {
        try
        {
          auto const g = car(xs).as<std::istream>().tellg();
          let const c = make<character>(get_codepoint(car(xs).as<std::istream>()));
          car(xs).as<std::istream>().seekg(g);
          return c;
        }
        catch (eof const&)
        {
          return eof_object;
        }
        catch (read_error const& error)
        {
          return make(error);
        }
      });

      library.define<procedure>("get-char!", [](let const& xs) -> value_type
      {
        try
        {
          return make<character>(get_codepoint(car(xs).as<std::istream>()));
        }
        catch (eof const&)
        {
          return eof_object;
        }
        catch (read_error const& error)
        {
          return make(error);
        }
      });

      library.define<predicate>("eof-object?", [](let const& xs)
      {
        return car(xs).is<eof>();
      });

      library.define<procedure>("eof-object", [](auto&&...)
      {
        return eof_object;
      });

      library.define<procedure>("get-string!", [](let const& xs)
      {
        auto read_k = [](string & string, std::size_t k, std::istream & is)
        {
          for (std::size_t i = 0; i < k and is; ++i)
          {
            string.codepoints.emplace_back(get_codepoint(is));
          }
        };

        let const s = make<string>();

        read_k(s.as<string>(), car(xs).as<exact_integer>(), cadr(xs).as<std::istream>());

        return s;
      });

      library.define<procedure>("put-char", [](let const& xs)
      {
        cadr(xs).as<std::ostream>() << static_cast<std::string>(car(xs).as<character>());
        return unspecified;
      });

      library.define<procedure>("put-string", [](let const& xs)
      {
        cadr(xs).as<std::ostream>() << static_cast<std::string>(car(xs).as<string>());
        return unspecified;
      });

      library.define<procedure>("output-port-flush", [](let const& xs)
      {
        car(xs).as<std::ostream>() << std::flush;
        return unspecified;
      });

      library.export_("input-port?");
      library.export_("output-port?");
      library.export_("binary-port?");
      library.export_("textual-port?");
      library.export_("port?");
      library.export_("input-port-open?");
      library.export_("output-port-open?");
      library.export_("standard-input-port");
      library.export_("standard-output-port");
      library.export_("standard-error-port");
      library.export_("open-input-file");
      library.export_("open-output-file");
      library.export_("close-input-port");
      library.export_("close-output-port");
      library.export_("input-string-open");
      library.export_("open-output-string");
      library.export_("get-output-string");
      library.export_("eof-object?");
      library.export_("eof-object");
      library.export_("get-ready?");
      library.export_("get-char");
      library.export_("get-char!");
      library.export_("get-string!");
      library.export_("put-char");
      library.export_("put-string");
      library.export_("output-port-flush");
    });

    define_library("(meevax read)", [](library & library)
    {
      library.define<procedure>("%read", [](let const& xs) mutable -> value_type
      {
        try
        {
          return interaction_environment().as<environment>().read(car(xs));
        }
        catch (eof const&)
        {
          return eof_object;
        }
        catch (read_error const& error)
        {
          return make(error);
        }
      });

      library.export_("%read");
    });

    define_library("(meevax string)", [](library & library)
    {
      library.define<predicate>("string?", [](let const& xs)
      {
        return car(xs).is<string>();
      });

      library.define<procedure>("make-string", [](let const& xs)
      {
        return make<string>(car(xs), cdr(xs).is<pair>() ? cadr(xs) : make<character>());
      });

      library.define<procedure>("string-length", [](let const& xs)
      {
        return car(xs).as<string>().length();
      });

      library.define<procedure>("string-ref", [](let const& xs)
      {
        return car(xs).as<string>().ref(cadr(xs));
      });

      library.define<procedure>("string-set!", [](let const& xs)
      {
        car(xs).as<string>().set(cadr(xs), caddr(xs));
        return car(xs);
      });

      library.define<procedure>("string-append", [](let const& xs)
      {
        /*
           (string-append string ...)                                 procedure

           Returns a newly allocated string whose characters are the
           concatenation of the characters in the given strings.
        */

        auto&& s = string();

        for (let const& x : xs)
        {
          std::copy(std::begin(x.as<string>().codepoints),
                    std::end(x.as<string>().codepoints),
                    std::back_inserter(s.codepoints));
        }

        return make(std::forward<decltype(s)>(s));
      });

      library.define<procedure>("string-copy", [](let const& xs)
      {
        return car(xs).as<string>().copy(cdr(xs).is<pair>() ? cadr(xs) : e0,
                                         cddr(xs).is<pair>() ? caddr(xs) : car(xs).as<string>().length());
      });

      library.define<procedure>("string-copy!", [](let const& xs)
      {
        car(xs).as<string>().copy(list_ref(xs, 1),
                                  list_ref(xs, 2),
                                  list_tail(xs, 3).is<pair>() ? list_ref(xs, 3) : e0,
                                  list_tail(xs, 3).is<pair>() ? list_ref(xs, 4) : car(xs).as<vector>().length());
        return unspecified;
      });

      #define STRING_COMPARE(COMPARE)                                          \
      [](let const& xs)                                                        \
      {                                                                        \
        return std::adjacent_find(                                             \
                 std::begin(xs), std::end(xs), [](let const& a, let const& b)  \
                 {                                                             \
                   return not COMPARE()(a.as_const<string>().codepoints,       \
                                        b.as_const<string>().codepoints);      \
                 }) == std::end(xs);                                           \
      }

      library.define<predicate>("string=?",  STRING_COMPARE(equal_to     ));
      library.define<predicate>("string<?",  STRING_COMPARE(less         ));
      library.define<predicate>("string<=?", STRING_COMPARE(less_equal   ));
      library.define<predicate>("string>?",  STRING_COMPARE(greater      ));
      library.define<predicate>("string>=?", STRING_COMPARE(greater_equal));

      #undef STRING_COMPARE

      library.define<procedure>("string->number", [](let const& xs)
      {
        return string_to_number(car(xs).as<string>(),
                                cdr(xs).is<pair>() ? cadr(xs).as<exact_integer>() : 10);
      });

      library.define<procedure>("string->list", [](let const& xs)
      {
        return car(xs).as<string>().make_list(cdr(xs).is<pair>() ? cadr(xs) : e0,
                                              cddr(xs).is<pair>() ? caddr(xs) : car(xs).as<string>().length());
      });

      library.define<procedure>("string->symbol", [](let const& xs)
      {
        return string_to_symbol(car(xs).as<string>());
      });

      library.define<procedure>("list->string", [](let const& xs)
      {
        return make<string>(car(xs));
      });

      library.define<procedure>("vector->string", [](let const& xs)
      {
        return make<string>(car(xs),
                            cdr(xs).is<pair>() ? cadr(xs) : e0,
                            cddr(xs).is<pair>() ? caddr(xs) : car(xs).as<vector>().length());
      });

      library.export_("string?");
      library.export_("make-string");
      library.export_("string-append");
      library.export_("string-copy");
      library.export_("string-copy!");
      library.export_("string-length");
      library.export_("string-ref");
      library.export_("string-set!");
      library.export_("string=?");
      library.export_("string<?");
      library.export_("string<=?");
      library.export_("string>?");
      library.export_("string>=?");
      library.export_("string->list");
      library.export_("string->number");
      library.export_("string->symbol");
      library.export_("list->string");
      library.export_("vector->string");
    });

    define_library("(meevax symbol)", [](library & library)
    {
      library.define<predicate>("symbol?", [](let const& xs)
      {
        return car(xs).is<symbol>();
      });

      library.define<procedure>("symbol->string", [](let const& xs)
      {
        return make<string>(car(xs).as<symbol>());
      });

      library.export_("symbol?");
      library.export_("symbol->string");
    });

    define_library("(meevax syntax)", [](library & library)
    {
      library.define<syntax>("begin", machine::begin);
      library.define<syntax>("call-with-current-continuation!", call_with_current_continuation);
      library.define<syntax>("define", machine::define);
      library.define<syntax>("define-syntax", define_syntax);
      library.define<syntax>("if", if_);
      library.define<syntax>("lambda", lambda);
      library.define<syntax>("let-syntax", let_syntax);
      library.define<syntax>("letrec", letrec);
      library.define<syntax>("letrec-syntax", letrec_syntax);
      library.define<syntax>("quote", quote);
      library.define<syntax>("quote-syntax", quote_syntax);
      library.define<syntax>("set!", set);

      library.export_("begin");
      library.export_("call-with-current-continuation!");
      library.export_("define");
      library.export_("define-syntax");
      library.export_("if");
      library.export_("lambda");
      library.export_("let-syntax");
      library.export_("letrec");
      library.export_("letrec-syntax");
      library.export_("quote");
      library.export_("quote-syntax");
      library.export_("set!");
    });

    define_library("(meevax vector)", [](library & library)
    {
      library.define<predicate>("vector?", [](let const& xs)
      {
        return car(xs).is<vector>();
      });

      library.define<procedure>("vector", [](let const& xs)
      {
        return make<vector>(xs);
      });

      library.define<procedure>("make-vector", [](let const& xs)
      {
        return make<vector>(car(xs), cdr(xs).is<pair>() ? cadr(xs) : unspecified);
      });

      library.define<procedure>("vector-append", [](let const& xs)
      {
        return vector::append(xs);
      });

      library.define<procedure>("vector-copy", [](let const& xs)
      {
        return car(xs).as<vector>().copy(cdr(xs).is<pair>() ? cadr(xs) : e0,
                                         cddr(xs).is<pair>() ? caddr(xs) : car(xs).as<vector>().length());
      });

      library.define<procedure>("vector-copy!", [](let const& xs)
      {
        car(xs).as<vector>().copy(list_ref(xs, 1),
                                  list_ref(xs, 2),
                                  list_tail(xs, 3).is<pair>() ? list_ref(xs, 3) : e0,
                                  list_tail(xs, 3).is<pair>() ? list_ref(xs, 4) : car(xs).as<vector>().length());
        return unspecified;
      });

      library.define<procedure>("vector-length", [](let const& xs)
      {
        return car(xs).as<vector>().length();
      });

      library.define<procedure>("vector-ref", [](let const& xs)
      {
        return car(xs).as<vector>().ref(cadr(xs));
      });

      library.define<procedure>("vector-set!", [](let const& xs)
      {
        return car(xs).as<vector>().set(cadr(xs), caddr(xs));
      });

      library.define<procedure>("vector-fill!", [](let const& xs)
      {
        car(xs).as<vector>().fill(cdr(xs).is<pair>() ? cadr(xs) : unspecified,
                                  cddr(xs).is<pair>() ? caddr(xs) : e0,
                                  cdddr(xs).is<pair>() ? cadddr(xs) : car(xs).as<vector>().length());
        return unspecified;
      });

      library.define<procedure>("vector->list", [](let const& xs)
      {
        return car(xs).as<vector>().list(cdr(xs).is<pair>() ? cadr(xs) : e0,
                                         cddr(xs).is<pair>() ? caddr(xs) : car(xs).as<vector>().length());
      });

      library.define<procedure>("string->vector", [](let const& xs)
      {
        return make<vector>(car(xs).as<string>());
      });

      library.export_("vector?");
      library.export_("vector");
      library.export_("make-vector");
      library.export_("vector-append");
      library.export_("vector-copy");
      library.export_("vector-copy!");
      library.export_("vector-length");
      library.export_("vector-ref");
      library.export_("vector-set!");
      library.export_("vector-fill!");
      library.export_("vector->list");
      library.export_("string->vector");
    });

    define_library("(meevax version)", [](library & library)
    {
      library.define<procedure>("features", [](auto&&...)
      {
        return features();
      });

      library.export_("features");
    });

    define_library("(meevax write)", [](library & library)
    {
      library.define<procedure>("write", [](let const& xs)
      {
        kernel::write(cadr(xs), car(xs));
        return unspecified;
      });

      library.define<procedure>("write-simple", [](let const& xs)
      {
        kernel::write_simple(cadr(xs), car(xs));
        return unspecified;
      });

      library.define<procedure>("print", [](let const& xs)
      {
        for (let const& x : xs)
        {
          if (x.is<string>())
          {
            std::cout << static_cast<std::string>(x.as<string>());
          }
          else
          {
            std::cout << x;
          }
        }

        std::cout << std::endl;

        return standard_output;
      });

      library.export_("print");
      library.export_("write");
      library.export_("write-simple");
    });

    std::vector<string_view> const codes {
      srfi_211,
      r4rs_essential,
      srfi_45,
      r4rs,
      srfi_149,
      r5rs,
      srfi_6,  // Basic String Ports
      srfi_11, // Syntax for receiving multiple values
      srfi_34, // Exception Handling for Programs
      srfi_23, // Error reporting mechanism
      srfi_38, // External Representation for Data With Shared Structure
      srfi_39, // Parameter objects
      r7rs,
      srfi_8,  // receive: Binding to multiple values
      srfi_1,  // List Library
      srfi_78, // Lightweight testing
    };

    auto sandbox = environment();

    for (auto const& code : codes)
    {
      // NOTE: Since read performs a putback operation on a given stream, it must be copied and used.
      auto port = std::stringstream(std::string(code));

      for (let e = sandbox.read(port); e != eof_object; e = sandbox.read(port))
      {
        sandbox.evaluate(e);
      }
    }
  }

  auto library::build() -> void
  {
    if (export_specs.is<null>())
    {
      for (let const& declaration : declarations)
      {
        evaluate(declaration);
      }
    }
  }

  auto library::evaluate(const_reference declaration) -> void
  {
    if (car(declaration).is<symbol>() and car(declaration).as<symbol>().value == "export")
    {
      for (let const& export_spec : cdr(declaration))
      {
        export_(export_spec);
      }
    }
    else if (car(declaration).is<symbol>() and car(declaration).as<symbol>().value == "begin")
    {
      for (let const& command_or_definition : cdr(declaration))
      {
        evaluate(command_or_definition);
      }
    }
    else
    {
      environment::evaluate(declaration); // Non-standard extension.
    }
  }

  auto library::export_(const_reference export_spec) -> void
  {
    export_specs = cons(export_spec, export_specs);
  }

  auto library::export_(std::string const& export_spec) -> void
  {
    export_(read(export_spec));
  }

  auto library::resolve() -> const_reference
  {
    build();

    auto resolve = [this](let const& export_spec)
    {
      if (car(export_spec).is<symbol>() and car(export_spec).as<symbol>().value == "rename")
      {
        return make<absolute>(caddr(export_spec), (*this)[cadr(export_spec)]);
      }
      else
      {
        return identify(export_spec, unit);
      }
    };

    return identifiers.is<null>() ? identifiers = map1(resolve, export_specs)
                                  : identifiers;
  }

  auto operator <<(std::ostream & os, library const& library) -> std::ostream &
  {
    return os << library.global();
  }

  std::unordered_map<std::string, library> libraries {};
} // namespace kernel
} // namespace meevax
