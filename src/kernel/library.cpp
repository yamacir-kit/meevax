/*
   Copyright 2018-2023 Tatsuya Yamasaki.

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

#include <chrono>
#include <filesystem>
#include <numeric>

#include <meevax/kernel/basis.hpp>
#include <meevax/kernel/binary_input_file_port.hpp>
#include <meevax/kernel/binary_output_file_port.hpp>
#include <meevax/kernel/disassemble.hpp>
#include <meevax/kernel/import_set.hpp>
#include <meevax/kernel/input_homogeneous_vector_port.hpp>
#include <meevax/kernel/library.hpp>
#include <meevax/kernel/output_file_port.hpp>
#include <meevax/kernel/output_homogeneous_vector_port.hpp>
#include <meevax/kernel/output_string_port.hpp>
#include <meevax/kernel/standard_error_port.hpp>
#include <meevax/kernel/standard_output_port.hpp>

namespace meevax
{
inline namespace kernel
{
  library::library(object const& declarations)
    : declarations { declarations }
  {}

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

  auto libraries() -> std::map<std::string, library> &
  {
    static auto libraries = std::map<std::string, library>();
    return libraries;
  }

  auto boot() -> void
  {
    define<library>("(meevax character)", [](library & library)
    {
      library.define<procedure>("char?", [](let const& xs)
      {
        return xs[0].is<character>();
      });

      library.define<procedure>("digit-value", [](let const& xs)
      {
        if (auto c = xs[0].as<character>(); std::isdigit(c.codepoint))
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
        return make<character>(xs[0].as<exact_integer>());
      });
    });

    define<library>("(meevax complex)", [](library & library)
    {
      library.define<procedure>("make-rectangular", [](let const& xs)
      {
        assert(is_real(xs[0]));
        assert(is_real(xs[1]));

        return make<complex>(xs[0], xs[1]);
      });

      library.define<procedure>("real-part", [](let const& xs)
      {
        return xs[0].as<complex>().real();
      });

      library.define<procedure>("imag-part", [](let const& xs)
      {
        return xs[0].as<complex>().imag();
      });
    });

    define<library>("(meevax context)", [](library & library)
    {
      library.define<procedure>("emergency-exit", [](let const& xs)
      {
        if (let const& status = xs[0]; status.is<null>())
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

      library.define<procedure>("command-line", [](let const&)
      {
        let xs = unit;

        for (auto&& each : interaction_environment().as<environment>().command_line)
        {
          xs = cons(make<string>(each), xs);
        }

        return reverse(xs);
      });
    });

    define<library>("(meevax comparator)", [](library & library)
    {
      library.define<procedure>("eq?", [](let const& xs)
      {
        return eq(xs[0], xs[1]);
      });

      library.define<procedure>("eqv?", [](let const& xs)
      {
        return eqv(xs[0], xs[1]);
      });
    });

    define<library>("(meevax core)", [](library & library)
    {
      using syntax = environment::syntax;

      library.define<syntax>("begin",                           syntax::sequence);
      library.define<syntax>("call-with-current-continuation!", syntax::call_with_current_continuation);
      library.define<syntax>("current",                         syntax::current);
      library.define<syntax>("define",                          syntax::define);
      library.define<syntax>("define-syntax",                   syntax::define_syntax);
      library.define<syntax>("if",                              syntax::conditional);
      library.define<syntax>("include",                         syntax::include);
      library.define<syntax>("install",                         syntax::install);
      library.define<syntax>("lambda",                          syntax::lambda);
      library.define<syntax>("let-syntax",                      syntax::let_syntax);
      library.define<syntax>("letrec",                          syntax::letrec);
      library.define<syntax>("letrec-syntax",                   syntax::letrec_syntax);
      library.define<syntax>("quote",                           syntax::quote);
      library.define<syntax>("quote-syntax",                    syntax::quote_syntax);
      library.define<syntax>("set!",                            syntax::set);
    });

    define<library>("(meevax environment)", [](library & library)
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
        return xs[1].as<environment>().evaluate(xs[0]);
      });

      library.define<procedure>("interaction-environment", []()
      {
        return interaction_environment();
      });

      library.define<procedure>("load", [](let const& xs)
      {
        return xs[0].as<environment>().load(xs[1].as<string>());
      });
    });

    define<library>("(meevax error)", [](library & library)
    {
      library.define<procedure>("throw", [](let const& xs) -> object
      {
        throw xs[0];
      });

      library.define<procedure>("error-object", [](let const& xs)
      {
        return make<error>(xs[0], cdr(xs));
      });

      library.define<procedure>("error-object?", [](let const& xs)
      {
        return xs[0].is_also<error>();
      });

      library.define<procedure>("read-error?", [](let const& xs)
      {
        return xs[0].is<read_error>();
      });

      library.define<procedure>("file-error?", [](let const& xs)
      {
        return xs[0].is<file_error>();
      });

      library.define<procedure>("kernel-exception-handler-set!", [](let const& xs)
      {
        return environment::raise = xs[0];
      });
    });

    define<library>("(meevax experimental)", [](library & library)
    {
      library.define<procedure>("type-of", [](let const& xs)
      {
        std::cout << xs[0].type().name() << std::endl;
      });

      library.define<procedure>("disassemble", [](let const& xs)
      {
        if (0 < length(xs))
        {
          if (let const& f = xs[0]; f.is<closure>())
          {
            disassemble(std::cout, car(f));
          }
        }
      });

      library.define<procedure>("ieee-float?", []()
      {
        return std::numeric_limits<double>::is_iec559;
      });
    });

    define<library>("(meevax file)", [](library & library)
    {
      library.define<procedure>("file-exists?", [](let const& xs)
      {
        return std::filesystem::exists(static_cast<std::string>(xs[0].as<string>()));
      });

      library.define<procedure>("delete-file", [](let const& xs)
      {
        try
        {
          if (not std::filesystem::remove(static_cast<std::string>(xs[0].as<string>())))
          {
            throw file_error(make<string>("failed to remove file"), xs[0]);
          }
        }
        catch (std::filesystem::filesystem_error const& e)
        {
          throw file_error(make<string>(e.what()), xs[0]);
        }
      });
    });

    define<library>("(meevax function)", [](library & library)
    {
      library.define<procedure>("closure?", [](let const& xs)
      {
        return xs[0].is<closure>();
      });

      library.define<procedure>("continuation?", [](let const& xs)
      {
        return xs[0].is<continuation>();
      });

      library.define<procedure>("foreign-function", [](let const& xs)
      {
        return make<procedure>(xs[1].as<string>(), xs[0].as<string>());
      });

      library.define<procedure>("foreign-function?", [](let const& xs)
      {
        return xs[0].is<procedure>();
      });
    });

    define<library>("(meevax garbage-collector)", [](library & library)
    {
      library.define<procedure>("gc-collect", []()
      {
        gc.collect();
      });

      library.define<procedure>("gc-count", []()
      {
        return make<exact_integer>(gc.count());
      });
    });

    define<library>("(meevax inexact)", [](library & library)
    {
      library.define<procedure>("finite?", [](let const& xs)
      {
        return is_finite(xs[0]);
      });

      library.define<procedure>("infinite?", [](let const& xs)
      {
        return is_infinite(xs[0]);
      });

      library.define<procedure>("nan?", [](let const& xs)
      {
        return is_nan(xs[0]);
      });

      library.define<procedure>("exp", [](let const& xs)
      {
        return exp(xs[0]);
      });

      library.define<procedure>("sqrt", [](let const& xs)
      {
        return sqrt(xs[0]);
      });

      library.define<procedure>("log", [](let const& xs)
      {
        return cdr(xs).is<pair>() ? log(xs[0]) / log(xs[1])
                                  : log(xs[0]);
      });

      library.define<procedure>("sin", [](let const& xs)
      {
        return sin(xs[0]);
      });

      library.define<procedure>("cos", [](let const& xs)
      {
        return cos(xs[0]);
      });

      library.define<procedure>("tan", [](let const& xs)
      {
        return tan(xs[0]);
      });

      library.define<procedure>("asin", [](let const& xs)
      {
        return asin(xs[0]);
      });

      library.define<procedure>("acos", [](let const& xs)
      {
        return acos(xs[0]);
      });

      library.define<procedure>("atan", [](let const& xs)
      {
        return cdr(xs).is<pair>() ? atan(xs[0], xs[1])
                                  : atan(xs[0]);
      });

      library.define<procedure>("sinh", [](let const& xs)
      {
        return sinh(xs[0]);
      });

      library.define<procedure>("cosh", [](let const& xs)
      {
        return cosh(xs[0]);
      });

      library.define<procedure>("tanh", [](let const& xs)
      {
        return tanh(xs[0]);
      });

      library.define<procedure>("asinh", [](let const& xs)
      {
        return asinh(xs[0]);
      });

      library.define<procedure>("acosh", [](let const& xs)
      {
        return acosh(xs[0]);
      });

      library.define<procedure>("atanh", [](let const& xs)
      {
        return atanh(xs[0]);
      });
    });

    define<library>("(meevax list)", [](library & library)
    {
      library.define<procedure>("null?", [](let const& xs)
      {
        return xs[0].is<null>();
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

        return std::accumulate(std::prev(std::rend(xs[0].as<string>().codepoints), tail(xs, 2).is<pair>() ? xs[2].as<exact_integer>() : xs[0].as<string>().codepoints.size()),
                               std::prev(std::rend(xs[0].as<string>().codepoints), tail(xs, 1).is<pair>() ? xs[1].as<exact_integer>() : 0),
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

        return std::accumulate(std::prev(std::rend(xs[0].as<vector>().objects), tail(xs, 2).is<pair>() ? xs[2].as<exact_integer>() : xs[0].as<vector>().objects.size()),
                               std::prev(std::rend(xs[0].as<vector>().objects), tail(xs, 1).is<pair>() ? xs[1].as<exact_integer>() : 0),
                               unit,
                               xcons);
      });
    });

    define<library>("(meevax macro)", [](library & library)
    {
      using syntactic_closure = environment::syntactic_closure;

      library.define<procedure>("identifier?", [](let const& xs)
      {
        return xs[0].is_also<identifier>();
      });

      library.define<procedure>("transformer?", [](let const& xs)
      {
        return xs[0].is<transformer>();
      });

      library.define<procedure>("syntactic-closure?", [](let const& xs)
      {
        return xs[0].is<syntactic_closure>();
      });

      library.define<procedure>("make-syntactic-closure", [](let const& xs)
      {
        return make<syntactic_closure>(xs[0], xs[1], xs[2]);
      });
    });

    define<library>("(meevax number)", [](library & library)
    {
      library.define<procedure>("number?", [](let const& xs)
      {
        return is_complex(xs[0]);
      });

      library.define<procedure>("complex?", [](let const& xs)
      {
        return is_complex(xs[0]);
      });

      library.define<procedure>("real?", [](let const& xs)
      {
        return is_real(xs[0]);
      });

      library.define<procedure>("rational?", [](let const& xs)
      {
        return is_rational(xs[0]);
      });

      library.define<procedure>("integer?", [](let const& xs)
      {
        return is_integer(xs[0]);
      });

      library.define<procedure>("exact-integer?", [](let const& xs)
      {
        return xs[0].is<exact_integer>();
      });

      library.define<procedure>("imaginary?", [](let const& xs)
      {
        return xs[0].is<complex>();
      });

      library.define<procedure>("ratio?", [](let const& xs)
      {
        return xs[0].is<ratio>();
      });

      library.define<procedure>("single-float?", [](let const& xs)
      {
        return xs[0].is<float>();
      });

      library.define<procedure>("double-float?", [](let const& xs)
      {
        return xs[0].is<double>();
      });

      library.define<procedure>("=", [](let const& xs)
      {
        return std::adjacent_find(std::begin(xs), std::end(xs), not_equals) == std::end(xs);
      });

      library.define<procedure>("<", [](let const& xs)
      {
        return std::adjacent_find(std::begin(xs), std::end(xs), greater_than_or_equals) == std::end(xs);
      });

      library.define<procedure>("<=", [](let const& xs)
      {
        return std::adjacent_find(std::begin(xs), std::end(xs), greater_than) == std::end(xs);
      });

      library.define<procedure>(">", [](let const& xs)
      {
        return std::adjacent_find(std::begin(xs), std::end(xs), less_than_or_equals) == std::end(xs);
      });

      library.define<procedure>(">=", [](let const& xs)
      {
        return std::adjacent_find(std::begin(xs), std::end(xs), less_than) == std::end(xs);
      });

      library.define<procedure>("+", [](let const& xs)
      {
        return std::accumulate(std::begin(xs), std::end(xs), e0, std::plus());
      });

      library.define<procedure>("*", [](let const& xs)
      {
        return std::accumulate(std::begin(xs), std::end(xs), e1, std::multiplies());
      });

      library.define<procedure>("-", [](let const& xs)
      {
        if (cdr(xs).is<pair>())
        {
          return std::accumulate(std::next(std::begin(xs)), std::end(xs), xs[0], std::minus());
        }
        else
        {
          return e0 - xs[0];
        }
      });

      library.define<procedure>("/", [](let const& xs)
      {
        if (cdr(xs).is<pair>())
        {
          return std::accumulate(std::next(std::begin(xs)), std::end(xs), xs[0], std::divides());
        }
        else
        {
          return e1 / xs[0];
        }
      });

      library.define<procedure>("%", [](let const& xs)
      {
        return xs[0] % xs[1];
      });

      library.define<procedure>("ratio-numerator", [](let const& xs)
      {
        return make(xs[0].as<ratio>().numerator());
      });

      library.define<procedure>("ratio-denominator", [](let const& xs)
      {
        return make(xs[0].as<ratio>().denominator());
      });

      library.define<procedure>("floor", [](let const& xs)
      {
        return floor(xs[0]);
      });

      library.define<procedure>("ceiling", [](let const& xs)
      {
        return ceil(xs[0]);
      });

      library.define<procedure>("truncate", [](let const& xs)
      {
        return trunc(xs[0]);
      });

      library.define<procedure>("round", [](let const& xs)
      {
        return round(xs[0]);
      });

      library.define<procedure>("exact-integer-square-root", [](let const& xs)
      {
        auto&& [s, r] = exact_integer_sqrt(xs[0].as<exact_integer>());

        return cons(make(std::forward<decltype(s)>(s)),
                    make(std::forward<decltype(r)>(r)));
      });

      library.define<procedure>("expt", [](let const& xs)
      {
        return pow(xs[0], xs[1]);
      });

      library.define<procedure>("exact", [](let const& xs)
      {
        return exact(xs[0]);
      });

      library.define<procedure>("inexact", [](let const& xs)
      {
        return inexact(xs[0]);
      });

      library.define<procedure>("char->integer", [](let const& xs)
      {
        return make<exact_integer>(xs[0].as<character>().codepoint);
      });

      library.define<procedure>("string->number", [](let const& xs)
      {
        return make_number(xs[0].as<string>(),
                           cdr(xs).is<pair>() ? xs[1].as<exact_integer>() : 10);
      });
    });

    define<library>("(meevax pair)", [](library & library)
    {
      library.define<procedure>("pair?", [](let const& xs)
      {
        return xs[0].is<pair>();
      });

      library.define<procedure>("cons", [](let const& xs)
      {
        return cons(xs[0], xs[1]);
      });

      library.define<procedure>("car", [](let const& xs) { return car(xs[0]); });
      library.define<procedure>("cdr", [](let const& xs) { return cdr(xs[0]); });

      library.define<procedure>("caar", [](let const& xs) { return caar(xs[0]); });
      library.define<procedure>("cadr", [](let const& xs) { return cadr(xs[0]); });
      library.define<procedure>("cdar", [](let const& xs) { return cdar(xs[0]); });
      library.define<procedure>("cddr", [](let const& xs) { return cddr(xs[0]); });

      library.define<procedure>("caaar", [](let const& xs) { return caaar(xs[0]); });
      library.define<procedure>("caadr", [](let const& xs) { return caadr(xs[0]); });
      library.define<procedure>("cadar", [](let const& xs) { return cadar(xs[0]); });
      library.define<procedure>("caddr", [](let const& xs) { return caddr(xs[0]); });
      library.define<procedure>("cdaar", [](let const& xs) { return cdaar(xs[0]); });
      library.define<procedure>("cdadr", [](let const& xs) { return cdadr(xs[0]); });
      library.define<procedure>("cddar", [](let const& xs) { return cddar(xs[0]); });
      library.define<procedure>("cdddr", [](let const& xs) { return cdddr(xs[0]); });

      library.define<procedure>("caaaar", [](let const& xs) { return caaaar(xs[0]); });
      library.define<procedure>("caaadr", [](let const& xs) { return caaadr(xs[0]); });
      library.define<procedure>("caadar", [](let const& xs) { return caadar(xs[0]); });
      library.define<procedure>("caaddr", [](let const& xs) { return caaddr(xs[0]); });
      library.define<procedure>("cadaar", [](let const& xs) { return cadaar(xs[0]); });
      library.define<procedure>("cadadr", [](let const& xs) { return cadadr(xs[0]); });
      library.define<procedure>("caddar", [](let const& xs) { return caddar(xs[0]); });
      library.define<procedure>("cadddr", [](let const& xs) { return cadddr(xs[0]); });
      library.define<procedure>("cdaaar", [](let const& xs) { return cdaaar(xs[0]); });
      library.define<procedure>("cdaadr", [](let const& xs) { return cdaadr(xs[0]); });
      library.define<procedure>("cdadar", [](let const& xs) { return cdadar(xs[0]); });
      library.define<procedure>("cdaddr", [](let const& xs) { return cdaddr(xs[0]); });
      library.define<procedure>("cddaar", [](let const& xs) { return cddaar(xs[0]); });
      library.define<procedure>("cddadr", [](let const& xs) { return cddadr(xs[0]); });
      library.define<procedure>("cdddar", [](let const& xs) { return cdddar(xs[0]); });
      library.define<procedure>("cddddr", [](let const& xs) { return cddddr(xs[0]); });

      library.define<procedure>("set-car!", [](auto&& xs) { return car(xs[0]) = xs[1]; });
      library.define<procedure>("set-cdr!", [](auto&& xs) { return cdr(xs[0]) = xs[1]; });
    });

    define<library>("(meevax port)", [](library & library)
    {
      library.define<procedure>("input-port?", [](let const& xs)
      {
        return xs[0].is_also<input_port>();
      });

      library.define<procedure>("output-port?", [](let const& xs)
      {
        return xs[0].is_also<output_port>();
      });

      library.define<procedure>("binary-port?", [](let const& xs)
      {
        return xs[0].is_also<binary_port>();
      });

      library.define<procedure>("textual-port?", [](let const& xs)
      {
        return xs[0].is_also<textual_port>();
      });

      library.define<procedure>("port?", [](let const& xs)
      {
        return xs[0].is_also<port>();
      });

      library.define<procedure>("open?", [](let const& xs)
      {
        return xs[0].as<port>().is_open();
      });

      library.define<procedure>("standard-input-port", []()
      {
        return make<standard_input_port>();
      });

      library.define<procedure>("standard-output-port", []()
      {
        return make<standard_output_port>();
      });

      library.define<procedure>("standard-error-port", []()
      {
        return make<standard_error_port>();
      });

      library.define<procedure>("open-input-file", [](let const& xs)
      {
        return make<input_file_port>(xs[0].as<string>());
      });

      library.define<procedure>("open-output-file", [](let const& xs)
      {
        return make<output_file_port>(xs[0].as<string>());
      });

      library.define<procedure>("open-binary-input-file", [](let const& xs)
      {
        return make<binary_input_file_port>(xs[0].as<string>());
      });

      library.define<procedure>("open-binary-output-file", [](let const& xs)
      {
        return make<binary_output_file_port>(xs[0].as<string>());
      });

      library.define<procedure>("close", [](let const& xs)
      {
        xs[0].as<port>().close();
      });

      library.define<procedure>("open-input-string", [](let const& xs)
      {
        return make<input_string_port>(xs[0].as<string>());
      });

      library.define<procedure>("open-output-string", [](let const&)
      {
        return make<output_string_port>();
      });

      library.define<procedure>("get-output-string", [](let const& xs)
      {
        return make<string>(xs[0].as<output_string_port>().ostringstream.str());
      });

      library.define<procedure>("open-input-u8vector", [](let const& xs)
      {
        return make<input_u8vector_port>(xs[0].as<u8vector>());
      });

      library.define<procedure>("open-output-u8vector", [](let const&)
      {
        return make<output_u8vector_port>();
      });

      library.define<procedure>("get-output-u8vector", [](let const& xs)
      {
        return make<u8vector>(xs[0].as<output_u8vector_port>().vector);
      });

      library.define<procedure>("eof-object?", [](let const& xs)
      {
        return xs[0].is<eof>();
      });

      library.define<procedure>("eof-object", []()
      {
        return eof_object;
      });

      library.define<procedure>("flush", [](let const& xs)
      {
        xs[0].as<output_port>().flush();
      });
    });

    define<library>("(meevax read)", [](library & library)
    {
      library.define<procedure>("get-char", [](let const& xs)
      {
        return xs[0].as<textual_input_port>().get();
      });

      library.define<procedure>("get-char-ready?", [](let const& xs)
      {
        return xs[0].as<textual_input_port>().get_ready();
      });

      library.define<procedure>("get-line", [](let const& xs)
      {
        return xs[0].as<textual_input_port>().get_line();
      });

      library.define<procedure>("get-string", [](let const& xs)
      {
        return xs[1].as<textual_input_port>().get(xs[0].as<exact_integer>());
      });

      library.define<procedure>("peek-char", [](let const& xs)
      {
        return xs[0].as<textual_input_port>().peek();
      });

      library.define<procedure>("get-u8", [](let const& xs)
      {
        return xs[0].as<binary_input_port>().get();
      });

      library.define<procedure>("get-u8-ready?", [](let const& xs)
      {
        return xs[0].as<binary_input_port>().get_ready();
      });

      library.define<procedure>("peek-u8", [](let const& xs)
      {
        return xs[0].as<binary_input_port>().peek();
      });

      library.define<procedure>("get-u8vector", [](let const& xs)
      {
        return xs[1].as<binary_input_port>().get(xs[0].as<exact_integer>());
      });

      library.define<procedure>("read", [](let const& xs)
      {
        return xs[0].as<textual_input_port>().read();
      });
    });

    define<library>("(meevax string)", [](library & library)
    {
      library.define<procedure>("string?", [](let const& xs)
      {
        return xs[0].is<string>();
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
                            tail(xs, 1).is<pair>() ? xs[1].as<character>() : character());
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

        return make(xs[0].as<string>().codepoints.at(xs[1].as<exact_integer>()));
      });

      library.define<procedure>("string-set!", [](let const& xs)
      {
        /*
           (string-set! string k char)                                procedure

           It is an error if k is not a valid index of string.

           The string-set! procedure stores char in element k of string. There
           is no requirement for this procedure to execute in constant time.
        */

        xs[0].as<string>().codepoints.at(xs[1].as<exact_integer>()) = xs[2].as<character>();

        return xs[0];
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

        std::copy(std::next(std::begin(xs[0].as<string>().codepoints), tail(xs, 1).is<pair>() ? xs[1].as<exact_integer>() : 0),
                  std::next(std::begin(xs[0].as<string>().codepoints), tail(xs, 2).is<pair>() ? xs[2].as<exact_integer>() : xs[0].as<string>().codepoints.size()),
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

        std::copy(std::next(std::begin(s2), tail(xs, 3).is<pair>() ? xs[3].as<exact_integer>() : 0),
                  std::next(std::begin(s2), tail(xs, 4).is<pair>() ? xs[4].as<exact_integer>() : s2.size()),
                  std::next(std::begin(s1),                          xs[1].as<exact_integer>()));
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

      library.define<procedure>("string=?",  STRING_COMPARE(std::equal_to     ));
      library.define<procedure>("string<?",  STRING_COMPARE(std::less         ));
      library.define<procedure>("string<=?", STRING_COMPARE(std::less_equal   ));
      library.define<procedure>("string>?",  STRING_COMPARE(std::greater      ));
      library.define<procedure>("string>=?", STRING_COMPARE(std::greater_equal));

      #undef STRING_COMPARE

      library.define<procedure>("symbol->string", [](let const& xs)
      {
        return make<string>(xs[0].as<symbol>());
      });

      library.define<procedure>("number->string", [](let const& xs)
      {
        return number_to_string(xs[0], cdr(xs).is<pair>() ? xs[1].as<exact_integer>() : 10);
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

        std::for_each(std::next(std::begin(xs[0].as<vector>().objects), tail(xs, 1).is<pair>() ? xs[1].as<exact_integer>() : 0),
                      std::next(std::begin(xs[0].as<vector>().objects), tail(xs, 2).is<pair>() ? xs[2].as<exact_integer>() : xs[0].as<vector>().objects.size()),
                      [&](let const& x)
                      {
                        s.codepoints.push_back(x.as<character>());
                      });

        return make(s);
      });
    });

    define<library>("(meevax symbol)", [](library & library)
    {
      library.define<procedure>("symbol?", [](let const& xs)
      {
        return xs[0].is<symbol>();
      });

      library.define<procedure>("string->symbol", [](let const& xs)
      {
        return make_symbol(xs[0].as<string>());
      });

      using syntactic_closure = environment::syntactic_closure;

      library.define<procedure>("identifier->symbol", [](let const& xs)
      {
        if (let const& x = xs[0]; x.is<syntactic_closure>())
        {
          return x.as<syntactic_closure>().expression;
        }
        else
        {
          return x;
        }
      });
    });

    define<library>("(meevax system)", [](library & library)
    {
      library.define<procedure>("get-environment-variable", [](let const& xs)
      {
        if (auto s = std::getenv(static_cast<std::string>(xs[0].as<string>()).c_str()))
        {
          return make<string>(s);
        }
        else
        {
          return f;
        }
      });

      library.define<procedure>("get-environment-variables", [](let const&)
      {
        let alist = unit;

        for (auto iter = environ; *iter; ++iter)
        {
          if (auto const position = std::string_view(*iter).find_first_of("="); position != std::string::npos)
          {
            alist = cons(cons(make<string>(std::string(*iter, position)),
                              make<string>(std::string(*iter + position + 1))),
                         alist);
          }
        }

        return alist;
      });
    });

    define<library>("(meevax time)", [](library & library)
    {
      library.define<procedure>("current-jiffy", [](let const&)
      {
        return make<exact_integer>(std::chrono::high_resolution_clock::now().time_since_epoch().count());
      });

      library.define<procedure>("jiffies-per-second", [](let const&)
      {
        return make<exact_integer>(std::chrono::high_resolution_clock::period::den);
      });
    });

    define<library>("(meevax vector)", [](library & library)
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

        return make<vector>(xs[0].as<exact_integer>(), tail(xs, 1).is<pair>() ? xs[1] : unspecified);
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

        std::copy(std::next(std::begin(xs[0].as<vector>().objects), tail(xs, 1).is<pair>() ? xs[1].as<exact_integer>() : 0),
                  std::next(std::begin(xs[0].as<vector>().objects), tail(xs, 2).is<pair>() ? xs[2].as<exact_integer>() : xs[0].as<vector>().objects.size()),
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

        std::copy(std::next(std::begin(v2), tail(xs, 3).is<pair>() ? xs[3].as<exact_integer>() : 0),
                  std::next(std::begin(v2), tail(xs, 4).is<pair>() ? xs[4].as<exact_integer>() : v1.size()),
                  std::next(std::begin(v1),                          xs[1].as<exact_integer>()));
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

        std::fill(std::next(std::begin(xs[0].as<vector>().objects), tail(xs, 2).is<pair>() ? xs[2].as<exact_integer>() : 0),
                  std::next(std::begin(xs[0].as<vector>().objects), tail(xs, 3).is<pair>() ? xs[3].as<exact_integer>() : xs[0].as<vector>().objects.size()),
                  tail(xs, 1).is<pair>() ? xs[1] : unspecified);
      });

      library.define<procedure>("list->vector", [](let const& xs)
      {
        return make<vector>(xs[0]);
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

    define<library>("(meevax vector homogeneous)", [](library & library)
    {
      #define DEFINE_HOMOGENEOUS_VECTOR(TAG)                                   \
      library.define<procedure>(#TAG "vector?", [](let const& xs)              \
      {                                                                        \
        return xs[0].is<TAG##vector>();                                        \
      });                                                                      \
                                                                               \
      library.define<procedure>("make-" #TAG "vector", [](let const& xs)       \
      {                                                                        \
        return make<TAG##vector>(xs[0].as<exact_integer>(), tail(xs, 1).is<pair>() ? xs[1] : unspecified); \
      });                                                                      \
                                                                               \
      library.define<procedure>(#TAG "vector", [](let const& xs)               \
      {                                                                        \
        return make<TAG##vector>(xs);                                          \
      });                                                                      \
                                                                               \
      library.define<procedure>(#TAG "vector-length", [](let const& xs)        \
      {                                                                        \
        return make<exact_integer>(xs[0].as<TAG##vector>().valarray.size());   \
      });                                                                      \
                                                                               \
      library.define<procedure>(#TAG "vector-ref", [](let const& xs)           \
      {                                                                        \
        return TAG##vector::output_cast(xs[0].as<TAG##vector>().valarray[xs[1].as<exact_integer>()]); \
      });                                                                      \
                                                                               \
      library.define<procedure>(#TAG "vector-set!", [](let const& xs)          \
      {                                                                        \
        xs[0].as<TAG##vector>().valarray[xs[1].as<exact_integer>()] = TAG##vector::input_cast(xs[2]); \
      });                                                                      \
                                                                               \
      library.define<procedure>(#TAG "vector-copy", [](let const& xs)          \
      {                                                                        \
        return make<TAG##vector>(xs[0].as<TAG##vector>(),                      \
                                 tail(xs, 1).is<pair>() ? xs[1].as<exact_integer>() : std::size_t(), \
                                 tail(xs, 2).is<pair>() ? xs[2].as<exact_integer>() : xs[0].as<TAG##vector>().valarray.size()); \
      });                                                                      \
                                                                               \
      library.define<procedure>(#TAG "vector-copy!", [](let const& xs)         \
      {                                                                        \
        auto copy = [](auto&& to, auto&& at, auto&& from, auto&& start, auto&& end) \
        {                                                                      \
          to[std::slice(at, end - start, 1)] = from[std::slice(start, end - start, 1)]; \
        };                                                                     \
                                                                               \
        copy(xs[0].as<TAG##vector>().valarray,                                 \
             xs[1].as<exact_integer>(),                                        \
             xs[2].as<TAG##vector>().valarray,                                 \
             tail(xs, 3).is<pair>() ? xs[3].as<exact_integer>() : 0,           \
             tail(xs, 4).is<pair>() ? xs[4].as<exact_integer>() : xs[2].as<TAG##vector>().valarray.size()); \
      });                                                                      \
                                                                               \
      library.define<procedure>(#TAG "vector-append", [](let const& xs)        \
      {                                                                        \
        return make<TAG##vector>(xs[0].as<TAG##vector>(),                      \
                                 xs[1].as<TAG##vector>());                     \
      });                                                                      \
                                                                               \
      library.define<procedure>(#TAG "vector->list", [](let const& xs)         \
      {                                                                        \
        auto list = [](auto&& v, auto&& a, auto&& b)                           \
        {                                                                      \
          auto xcons = [](auto&& x, auto&& y)                                  \
          {                                                                    \
            return cons(TAG##vector::output_cast(y), x);                       \
          };                                                                   \
                                                                               \
          return reverse(std::accumulate(std::next(std::begin(v), a),          \
                                         std::next(std::begin(v), b), unit, xcons)); \
        };                                                                     \
                                                                               \
        return list(xs[0].as<TAG##vector>().valarray,                          \
                    tail(xs, 1).is<pair>() ? xs[1].as<exact_integer>() : 0,    \
                    tail(xs, 2).is<pair>() ? xs[2].as<exact_integer>() : xs[0].as<TAG##vector>().valarray.size()); \
      });                                                                      \
                                                                               \
      library.define<procedure>("list->" #TAG "vector", [](let const& xs)      \
      {                                                                        \
        return make<TAG##vector>(xs[0]);                                       \
      })

      DEFINE_HOMOGENEOUS_VECTOR(f32);
      DEFINE_HOMOGENEOUS_VECTOR(f64);
      DEFINE_HOMOGENEOUS_VECTOR(s8);
      DEFINE_HOMOGENEOUS_VECTOR(s16);
      DEFINE_HOMOGENEOUS_VECTOR(s32);
      DEFINE_HOMOGENEOUS_VECTOR(s64);
      DEFINE_HOMOGENEOUS_VECTOR(u8);
      DEFINE_HOMOGENEOUS_VECTOR(u16);
      DEFINE_HOMOGENEOUS_VECTOR(u32);
      DEFINE_HOMOGENEOUS_VECTOR(u64);

      library.define<procedure>("u8vector->string", [](let const& xs)
      {
        auto buffer = std::ostringstream();

        std::for_each(std::next(std::begin(xs[0].as<u8vector>().valarray), tail(xs, 1).is<pair>() ? xs[1].as<exact_integer>() : 0),
                      std::next(std::begin(xs[0].as<u8vector>().valarray), tail(xs, 2).is<pair>() ? xs[2].as<exact_integer>() : xs[0].as<u8vector>().valarray.size()),
                      [&](auto const& x)
                      {
                        buffer << x;
                      });

        auto input = input_string_port(buffer.str());

        auto output = string();

        while (static_cast<std::istream &>(input).peek() != std::char_traits<char>::eof())
        {
          output.codepoints.emplace_back(get_codepoint(input));
        }

        return make(output);
      });

      library.define<procedure>("string->u8vector", [](let const& xs)
      {
        auto convert = [](std::string const& s)
        {
          return make<u8vector>(reinterpret_cast<std::uint8_t const*>(s.data()), s.size());
        };

        return convert(xs[0].as<string>());
      });
    });

    define<library>("(meevax version)", [](library & library)
    {
      library.define<procedure>("features", []()
      {
        return features();
      });
    });

    define<library>("(meevax write)", [](library & library)
    {
      library.define<procedure>("put-char", [](let const& xs)
      {
        xs[1].as<textual_output_port>().put(xs[0].as<character>());
      });

      library.define<procedure>("put-string", [](let const& xs)
      {
        xs[1].as<textual_output_port>().put(xs[0].as<string>());
      });

      library.define<procedure>("put-u8", [](let const& xs)
      {
        xs[1].as<binary_output_port>().put(xs[0].as<exact_integer>());
      });

      library.define<procedure>("put-u8vector", [](let const& xs)
      {
        xs[1].as<binary_output_port>().put(xs[0].as<u8vector>());
      });

      library.define<procedure>("write", [](let const& xs)
      {
        xs[1].as<textual_output_port>().write(xs[0]);
      });

      library.define<procedure>("write-simple", [](let const& xs)
      {
        xs[1].as<textual_output_port>().write_simple(xs[0]);
      });
    });

    auto boot_loader = environment();

    for (auto&& each : basis())
    {
      if (auto input = input_string_port(each); input.get_ready())
      {
        while (not static_cast<std::istream &>(input).eof())
        {
          boot_loader.evaluate(boot_loader.read(input));
        }
      }
    }
  }
} // namespace kernel
} // namespace meevax
