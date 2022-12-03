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

#include <meevax/kernel/basis.hpp>
#include <meevax/kernel/instruction.hpp>
#include <meevax/kernel/interaction_environment.hpp>
#include <meevax/kernel/library.hpp>

namespace meevax
{
inline namespace kernel
{
  library::library(object const& declarations)
    : declarations { declarations }
  {}

  auto library::boot() -> void
  {
    define_library("(meevax character)", [](library & library)
    {
      library.define<procedure>("char?", [](let const& xs)
      {
        return car(xs).is<character>();
      });

      library.define<procedure>("char-codepoint", [](let const& xs)
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

      library.define<procedure>("integer->char", [](let const& xs)
      {
        return make<character>(car(xs).as<exact_integer>());
      });
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
    });

    define_library("(meevax context)", [](library & library)
    {
      library.define<procedure>("emergency-exit", [](let const& xs) -> object
      {
        if (let const& status = car(xs); status.is<null>())
        {
          throw success;
        }
        else if (status.is<bool>())
        {
          throw is_truthy(status) ? success : failure;
        }
        else
        {
          throw static_cast<int>(status.as<exact_integer>());
        }
      });
    });

    define_library("(meevax function)", [](library & library)
    {
      library.define<procedure>("closure?", [](let const& xs)
      {
        return car(xs).is<closure>();
      });

      library.define<procedure>("continuation?", [](let const& xs)
      {
        return car(xs).is<continuation>();
      });

      library.define<procedure>("foreign-function", [](let const& xs)
      {
        return make<procedure>(cadr(xs).as<string>(), car(xs).as<string>());
      });

      library.define<procedure>("foreign-function?", [](let const& xs)
      {
        return car(xs).is<procedure>();
      });
    });

    define_library("(meevax environment)", [](library & library)
    {
      library.define<procedure>("environment", [](let const& xs)
      {
        let const e = make<environment>();

        for (let const& x : xs)
        {
          e.as<environment>().declare<import_set>(x);
        }

        return e;
      });

      library.define<procedure>("eval", [](let const& xs)
      {
        return cadr(xs).as<environment>().evaluate(car(xs));
      });

      library.define<procedure>("interaction-environment", []()
      {
        return interaction_environment();
      });

      library.define<procedure>("load", [](let const& xs)
      {
        return car(xs).as<environment>().load(cadr(xs).as<string>());
      });
    });

    define_library("(meevax dynamic-environment)", [](library & library)
    {
      library.define<syntax>("store-auxiliary", store_auxiliary);
      library.define<syntax>("load-auxiliary", load_auxiliary);
    });

    define_library("(meevax comparator)", [](library & library)
    {
      library.define<procedure>("identity=?", [](let const& xs)
      {
        return eq(car(xs), cadr(xs));
      });

      library.define<procedure>("normally=?", [](let const& xs)
      {
        return eqv(car(xs), cadr(xs));
      });
    });

    define_library("(meevax error)", [](library & library)
    {
      library.define<procedure>("throw", [](let const& xs) -> object
      {
        throw car(xs);
      });

      library.define<procedure>("error-object", [](let const& xs)
      {
        return make<error>(car(xs), cdr(xs));
      });

      library.define<procedure>("error-object?", [](let const& xs)
      {
        return car(xs).is_also<error>();
      });

      library.define<procedure>("read-error?", [](let const& xs)
      {
        return car(xs).is<read_error>();
      });

      library.define<procedure>("file-error?", [](let const& xs)
      {
        return car(xs).is<file_error>();
      });

      library.define<procedure>("kernel-exception-handler-set!", [](let const& xs)
      {
        return raise = car(xs);
      });
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

      library.define<procedure>("ieee-float?", []()
      {
        return std::numeric_limits<double>::is_iec559;
      });
    });

    define_library("(meevax garbage-collector)", [](library & library)
    {
      library.define<procedure>("gc-collect", []()
      {
        return make<exact_integer>(gc.collect());
      });

      library.define<procedure>("gc-count", []()
      {
        return make<exact_integer>(gc.count());
      });
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
    });

    define_library("(meevax list)", [](library & library)
    {
      library.define<procedure>("null?", [](let const& xs)
      {
        return car(xs).is<null>();
      });

      library.define<procedure>("append", [](let const& xs)
      {
        return std::accumulate(std::begin(xs), std::end(xs), unit, append2);
      });

      library.define<procedure>("string->list", [](let const& xs)
      {
        /*
           (string->list string)                                      procedure
           (string->list string start)                                procedure
           (string->list string start end)                            procedure

           (list->string list)                                        procedure

           It is an error if any element of list is not a character.

           The string->list procedure returns a newly allocated list of the
           characters of string between start and end. list->string returns a
           newly allocated string formed from the elements in the list list. In
           both procedures, order is preserved. string->list and list->string
           are inverses so far as equal? is concerned.
        */

        return std::accumulate(std::prev(std::rend(xs[0].as<string>().codepoints), list_tail(xs, 2).is<pair>() ? xs[2].as<exact_integer>() : xs[0].as<string>().codepoints.size()),
                               std::prev(std::rend(xs[0].as<string>().codepoints), list_tail(xs, 1).is<pair>() ? xs[1].as<exact_integer>() : 0),
                               unit,
                               [](let const& xs, character const& c)
                               {
                                 return cons(make(c), xs);
                               });
      });

      library.define<procedure>("vector->list", [](let const& xs)
      {
        /*
           (vector->list vector)                                      procedure
           (vector->list vector start)                                procedure
           (vector->list vector start end)                            procedure
           (list->vector list)                                        procedure

           The vector->list procedure returns a newly allocated list of the
           objects contained in the elements of vector between start and end.
           The list->vector procedure returns a newly created vector
           initialized to the elements of the list list.

           In both procedures, order is preserved.
        */

        return std::accumulate(std::prev(std::rend(xs[0].as<vector>().objects), list_tail(xs, 2).is<pair>() ? xs[2].as<exact_integer>() : xs[0].as<vector>().objects.size()),
                               std::prev(std::rend(xs[0].as<vector>().objects), list_tail(xs, 1).is<pair>() ? xs[1].as<exact_integer>() : 0),
                               unit,
                               xcons);
      });
    });

    define_library("(meevax macro)", [](library & library)
    {
      library.define<procedure>("identifier?", [](let const& xs)
      {
        return car(xs).is_also<identifier>();
      });

      library.define<procedure>("transformer?", [](let const& xs)
      {
        return car(xs).is_also<transformer>();
      });

      library.define<procedure>("syntactic-closure?", [](let const& xs)
      {
        return car(xs).is<syntactic_closure>();
      });

      library.define<procedure>("make-syntactic-closure", [](let const& xs)
      {
        return make<syntactic_closure>(car(xs), cadr(xs), caddr(xs));
      });
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

      library.define<procedure>("exact-integer?", [](let const& xs)
      {
        return car(xs).is<exact_integer>();
      });

      library.define<procedure>("%complex?", [](let const& xs)
      {
        return car(xs).is<complex>();
      });

      library.define<procedure>("ratio?", [](let const& xs)
      {
        return car(xs).is<ratio>();
      });

      library.define<procedure>("single-float?", [](let const& xs)
      {
        return car(xs).is<float>();
      });

      library.define<procedure>("double-float?", [](let const& xs)
      {
        return car(xs).is<double>();
      });

      #define DEFINE(SYMBOL, COMPARE)                                          \
      library.define<procedure>(#SYMBOL, [](let const& xs)                     \
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

      library.define<procedure>("char->integer", [](let const& xs)
      {
        return make<exact_integer>(car(xs).as<character>().codepoint);
      });

      library.define<procedure>("string->number", [](let const& xs)
      {
        return string_to_number(car(xs).as<string>(),
                                cdr(xs).is<pair>() ? cadr(xs).as<exact_integer>() : 10);
      });
    });

    define_library("(meevax pair)", [](library & library)
    {
      library.define<procedure>("pair?", [](let const& xs)
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
    });

    define_library("(meevax port)", [](library & library)
    {
      library.define<procedure>("input-port?", [](let const& xs)
      {
        return car(xs).is_also<std::istream>();
      });

      library.define<procedure>("output-port?", [](let const& xs)
      {
        return car(xs).is_also<std::ostream>();
      });

      library.define<procedure>("binary-port?", []()
      {
        return false;
      });

      library.define<procedure>("textual-port?", [](let const& xs)
      {
        return car(xs).is_also<std::ios>();
      });

      library.define<procedure>("port?", [](let const& xs)
      {
        return car(xs).is_also<std::ios>();
      });

      library.define<procedure>("open?", [](let const& xs)
      {
        if (let const& x = car(xs); x.is<file_port>())
        {
          return x.as<file_port>().is_open();
        }
        else
        {
          return x.is_also<std::ios>();
        }
      });

      library.define<procedure>("input-port", []()
      {
        return standard_input;
      });

      library.define<procedure>("output-port", []()
      {
        return standard_output;
      });

      library.define<procedure>("error-port", []()
      {
        return standard_error;
      });

      library.define<procedure>("open", [](let const& xs)
      {
        return make<file_port>(car(xs).as<string>());
      });

      library.define<procedure>("close", [](let const& xs)
      {
        car(xs).as<file_port>().close();
      });

      library.define<procedure>("string->port", [](let const& xs)
      {
        return xs.is<pair>() ? make<string_port>(car(xs).as<string>()) : make<string_port>();
      });

      library.define<procedure>("eof-object?", [](let const& xs)
      {
        return car(xs).is<eof>();
      });

      library.define<procedure>("eof-object", []()
      {
        return eof_object;
      });

      library.define<procedure>("flush", [](let const& xs)
      {
        car(xs).as<std::ostream>() << std::flush;
      });
    });

    define_library("(meevax read)", [](library & library)
    {
      library.define<procedure>("get-char", [](let const& xs)
      {
        try
        {
          auto const g = car(xs).as<std::istream>().tellg();
          let c = make<character>(get_codepoint(car(xs).as<std::istream>()));
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

      library.define<procedure>("get-char!", [](let const& xs)
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

      library.define<procedure>("get-ready?", [](let const& xs)
      {
        return static_cast<bool>(car(xs).as<std::istream>());
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

      library.define<procedure>("read", [](let const& xs)
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
    });

    define_library("(meevax string)", [](library & library)
    {
      library.define<procedure>("string?", [](let const& xs)
      {
        return car(xs).is<string>();
      });

      library.define<procedure>("make-string", [](let const& xs)
      {
        /*
           (make-string k)                                            procedure
           (make-string k char)                                       procedure

           The make-string procedure returns a newly allocated string of length
           k. If char is given, then all the characters of the string are
           initialized to char, otherwise the contents of the string are
           unspecified.
        */

        return make<string>(xs[0].as<exact_integer>(),
                            list_tail(xs, 1).is<pair>() ? xs[1].as<character>() : character());
      });

      library.define<procedure>("string-length", [](let const& xs)
      {
        /*
           (string-length string)                                     procedure

           Returns the number of characters in the given string.
        */

        return make<exact_integer>(xs[0].as<string>().codepoints.size());
      });

      library.define<procedure>("string-ref", [](let const& xs)
      {
        /*
           (string-ref string k)                                      procedure

           It is an error if k is not a valid index of string.

           The string-ref procedure returns character k of string using
           zero-origin indexing. There is no requirement for this procedure to
           execute in constant time.
        */

        return make(car(xs).as<string>().codepoints.at(cadr(xs).as<exact_integer>()));
      });

      library.define<procedure>("string-set!", [](let const& xs)
      {
        /*
           (string-set! string k char)                                procedure

           It is an error if k is not a valid index of string.

           The string-set! procedure stores char in element k of string. There
           is no requirement for this procedure to execute in constant time.
        */

        car(xs).as<string>().codepoints.at(cadr(xs).as<exact_integer>()) = caddr(xs).as<character>();

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
        /*
           (string-copy string)                                       procedure
           (string-copy string start)                                 procedure
           (string-copy string start end)                             procedure

           Returns a newly allocated copy of the part of the given string
           between start and end.
        */

        auto&& s = string();

        std::copy(std::next(std::begin(xs[0].as<string>().codepoints), list_tail(xs, 1).is<pair>() ? xs[1].as<exact_integer>() : 0),
                  std::next(std::begin(xs[0].as<string>().codepoints), list_tail(xs, 2).is<pair>() ? xs[2].as<exact_integer>() : xs[0].as<string>().codepoints.size()),
                  std::back_inserter(s.codepoints));

        return make(s);
      });

      library.define<procedure>("string-copy!", [](let const& xs)
      {
        /*
           (string-copy! to at from)                                  procedure
           (string-copy! to at from start)                            procedure
           (string-copy! to at from start end)                        procedure

           It is an error if at is less than zero or greater than the length of
           to. It is also an error if (- (string-length to) at) is less than (-
           end start).

           Copies the characters of string from between start and end to string
           to, starting at at. The order in which characters are copied is
           unspecified, except that if the source and destination overlap,
           copying takes place as if the source is first copied into a
           temporary string and then into the destination. This can be achieved
           without allocating storage by making sure to copy in the correct
           direction in such circumstances.
        */

        auto&& s1 = xs[0].as<string>().codepoints;

        auto&& s2 = xs[2].as<string>().codepoints;

        s1.reserve(s1.size() + s2.size());

        std::copy(std::next(std::begin(s2), list_tail(xs, 3).is<pair>() ? xs[3].as<exact_integer>() : 0),
                  std::next(std::begin(s2), list_tail(xs, 4).is<pair>() ? xs[4].as<exact_integer>() : s2.size()),
                  std::next(std::begin(s1),                               xs[1].as<exact_integer>()));
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

      library.define<procedure>("string=?",  STRING_COMPARE(equal_to     ));
      library.define<procedure>("string<?",  STRING_COMPARE(less         ));
      library.define<procedure>("string<=?", STRING_COMPARE(less_equal   ));
      library.define<procedure>("string>?",  STRING_COMPARE(greater      ));
      library.define<procedure>("string>=?", STRING_COMPARE(greater_equal));

      #undef STRING_COMPARE

      library.define<procedure>("symbol->string", [](let const& xs)
      {
        return make<string>(car(xs).as<symbol>());
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

      library.define<procedure>("list->string", [](let const& xs)
      {
        /*
           (list->string list)                                        procedure

           It is an error if any element of list is not a character.

           The string->list procedure returns a newly allocated list of the
           characters of string between start and end. list->string returns a
           newly allocated string formed from the elements in the list list. In
           both procedures, order is preserved. string->list and list->string
           are inverses so far as equal? is concerned.
        */

        auto&& s = string();

        for (let const& x : xs[0])
        {
          s.codepoints.push_back(x.as<character>());
        }

        return make(std::forward<decltype(s)>(s));
      });

      library.define<procedure>("vector->string", [](let const& xs)
      {
        /*
           (vector->string vector)                                    procedure
           (vector->string vector start)                              procedure
           (vector->string vector start end)                          procedure

           It is an error if any element of vector between start and end is not
           a character.

           The vector->string procedure returns a newly allocated string of the
           objects contained in the elements of vector between start and end.
           The string->vector procedure returns a newly created vector
           initialized to the elements of the string string between start and
           end.

           In both procedures, order is preserved.
        */

        auto&& s = string();

        std::for_each(std::next(std::begin(xs[0].as<vector>().objects), list_tail(xs, 1).is<pair>() ? xs[1].as<exact_integer>() : 0),
                      std::next(std::begin(xs[0].as<vector>().objects), list_tail(xs, 2).is<pair>() ? xs[2].as<exact_integer>() : xs[0].as<vector>().objects.size()),
                      [&](let const& x)
                      {
                        s.codepoints.push_back(x.as<character>());
                      });

        return make(s);
      });

      library.define<procedure>("port->string", [](let const& xs)
      {
        if (car(xs).is<string_port>())
        {
          return make<string>(car(xs).as<string_port>().str());
        }
        else
        {
          return make<string>(std::string(std::istreambuf_iterator<char>(car(xs).as<std::istream>()), {}));
        }
      });
    });

    define_library("(meevax symbol)", [](library & library)
    {
      library.define<procedure>("symbol?", [](let const& xs)
      {
        return car(xs).is<symbol>();
      });

      library.define<procedure>("string->symbol", [](let const& xs)
      {
        return string_to_symbol(car(xs).as<string>());
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
    });

    define_library("(meevax syntax)", [](library & library)
    {
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
      library.define<syntax>("sequence", sequence);
      library.define<syntax>("set!", set);
    });

    define_library("(meevax vector)", [](library & library)
    {
      library.define<procedure>("vector?", [](let const& xs)
      {
        return xs[0].is<vector>();
      });

      library.define<procedure>("vector", [](let const& xs)
      {
        /*
           (vector obj ...)                                           procedure

           Returns a newly allocated vector whose elements contain the given
           arguments. It is analogous to list.
        */

        return make<vector>(xs);
      });

      library.define<procedure>("make-vector", [](let const& xs)
      {
        /*
           (make-vector k)                                            procedure
           (make-vector k fill)                                       procedure

           Returns a newly allocated vector of k elements. If a second argument
           is given, then each element is initialized to fill. Otherwise the
           initial contents of each element is unspecified.
        */

        return make<vector>(xs[0].as<exact_integer>(), list_tail(xs, 1).is<pair>() ? xs[1] : unspecified);
      });

      library.define<procedure>("vector-append", [](let const& xs)
      {
        /*
           (vector-append vector ...)                                 procedure

           Returns a newly allocated vector whose elements are the
           concatenation of the elements of the given vectors.
        */

        auto&& v = vector();

        for (let const& x : xs)
        {
          for (let const& object : x.as<vector>().objects)
          {
            v.objects.push_back(object);
          }
        }

        return make(std::forward<decltype(v)>(v));
      });

      library.define<procedure>("vector-copy", [](let const& xs)
      {
        /*
           (vector-copy vector)                                       procedure
           (vector-copy vector start)                                 procedure
           (vector-copy vector start end)                             procedure

           Returns a newly allocated copy of the elements of the given vector
           between start and end. The elements of the new vector are the same
           (in the sense of eqv?) as the elements of the old.
        */

        auto&& v = vector();

        std::copy(std::next(std::begin(xs[0].as<vector>().objects), list_tail(xs, 1).is<pair>() ? xs[1].as<exact_integer>() : 0),
                  std::next(std::begin(xs[0].as<vector>().objects), list_tail(xs, 2).is<pair>() ? xs[2].as<exact_integer>() : xs[0].as<vector>().objects.size()),
                  std::back_inserter(v.objects));

        return make(std::forward<decltype(v)>(v));
      });

      library.define<procedure>("vector-copy!", [](let const& xs)
      {
        /*
           (vector-copy! to at from)                                  procedure
           (vector-copy! to at from start)                            procedure
           (vector-copy! to at from start end)                        procedure

           It is an error if at is less than zero or greater than the length of
           to. It is also an error if (- (vector-length to) at) is less than (-
           end start).

           Copies the elements of vector from between start and end to vector
           to, starting at at. The order in which elements are copied is
           unspecified, except that if the source and destination overlap,
           copying takes place as if the source is first copied into a
           temporary vector and then into the destination. This can be achieved
           without allocating storage by making sure to copy in the correct
           direction in such circumstances.
        */

        auto&& v1 = xs[0].as<vector>().objects;

        auto&& v2 = xs[2].as<vector>().objects;

        v1.reserve(v1.size() + v2.size());

        std::copy(std::next(std::begin(v2), list_tail(xs, 3).is<pair>() ? xs[3].as<exact_integer>() : 0),
                  std::next(std::begin(v2), list_tail(xs, 4).is<pair>() ? xs[4].as<exact_integer>() : v1.size()),
                  std::next(std::begin(v1),                               xs[1].as<exact_integer>()));
      });

      library.define<procedure>("vector-length", [](let const& xs)
      {
        /*
           (vector-length vector)                                     procedure

           Returns the number of elements in vector as an exact integer.
        */

        return make<exact_integer>(xs[0].as<vector>().objects.size());
      });

      library.define<procedure>("vector-ref", [](let const& xs)
      {
        /*
           (vector-ref vector k)                                      procedure

           It is an error if k is not a valid index of vector. The vector-ref
           procedure returns the contents of element k of vector.
        */

        return xs[0][xs[1].as<exact_integer>()];
      });

      library.define<procedure>("vector-set!", [](let const& xs)
      {
        /*
           (vector-set! vector k obj)                                 procedure

           It is an error if k is not a valid index of vector. The vector-set!
           procedure stores obj in element k of vector.
        */

        return xs[0].as<vector>().objects[xs[1].as<exact_integer>()] = xs[2];
      });

      library.define<procedure>("vector-fill!", [](let const& xs)
      {
        /*
           (vector-fill! vector fill)                                 procedure
           (vector-fill! vector fill start)                           procedure
           (vector-fill! vector fill start end)                       procedure

           The vector-fill! procedure stores fill in the elements of vector
           between start and end.
        */

        std::fill(std::next(std::begin(xs[0].as<vector>().objects), list_tail(xs, 2).is<pair>() ? xs[2].as<exact_integer>() : 0),
                  std::next(std::begin(xs[0].as<vector>().objects), list_tail(xs, 3).is<pair>() ? xs[3].as<exact_integer>() : xs[0].as<vector>().objects.size()),
                  list_tail(xs, 1).is<pair>() ? xs[1] : unspecified);
      });

      library.define<procedure>("list->vector", [](let const& xs)
      {
        return make<vector>(car(xs));
      });

      library.define<procedure>("string->vector", [](let const& xs)
      {
        auto&& v = vector();

        for (auto&& character : xs[0].as<string>().codepoints)
        {
          v.objects.push_back(make(character));
        }

        return make(std::forward<decltype(v)>(v));
      });
    });

    define_library("(meevax version)", [](library & library)
    {
      library.define<procedure>("features", []()
      {
        return features();
      });
    });

    define_library("(meevax write)", [](library & library)
    {
      library.define<procedure>("put-char", [](let const& xs)
      {
        cadr(xs).as<std::ostream>() << static_cast<std::string>(car(xs).as<character>());
      });

      library.define<procedure>("put-string", [](let const& xs)
      {
        cadr(xs).as<std::ostream>() << static_cast<std::string>(car(xs).as<string>());
      });

      library.define<procedure>("write", [](let const& xs)
      {
        meevax::write(cadr(xs), car(xs));
      });

      library.define<procedure>("write-simple", [](let const& xs)
      {
        write_simple(cadr(xs), car(xs));
      });
    });

    std::vector<string_view> const codes {
      r4rs,
      r4rs_essential,
      r5rs,
      r7rs,
      srfi_1,  // List Library
      srfi_6,  // Basic String Ports
      srfi_8,  // receive: Binding to multiple values
      srfi_11, // Syntax for receiving multiple values
      srfi_23, // Error reporting mechanism
      srfi_34, // Exception Handling for Programs
      srfi_38, // External Representation for Data With Shared Structure
      srfi_39, // Parameter objects
      srfi_45,
      srfi_78, // Lightweight testing
      srfi_149,
      srfi_211,
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

  auto library::evaluate(object const& declaration) -> void
  {
    if (declaration[0].is<symbol>() and declaration[0].as<symbol>() == "export")
    {
      for (let const& form : cdr(declaration))
      {
        declare<export_spec>(form);
      }
    }
    else if (declaration[0].is<symbol>() and declaration[0].as<symbol>() == "begin")
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

  auto library::resolve() -> object const&
  {
    if (not declarations.is<null>())
    {
      for (let const& declaration : declarations)
      {
        evaluate(declaration);
      }

      declarations = unit;
    }

    return subset;
  }

  auto operator <<(std::ostream & os, library const& library) -> std::ostream &
  {
    return os << library.global();
  }

  std::map<std::string, library> libraries {};
} // namespace kernel
} // namespace meevax
