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
#include <meevax/kernel/boot.hpp>
#include <meevax/kernel/box.hpp>
#include <meevax/kernel/input_file_port.hpp>
#include <meevax/kernel/input_homogeneous_vector_port.hpp>
#include <meevax/kernel/library.hpp>
#include <meevax/kernel/output_file_port.hpp>
#include <meevax/kernel/output_homogeneous_vector_port.hpp>
#include <meevax/kernel/output_string_port.hpp>
#include <meevax/kernel/standard_error_port.hpp>
#include <meevax/kernel/standard_input_port.hpp>
#include <meevax/kernel/standard_output_port.hpp>
#include <meevax/kernel/vector.hpp>

namespace meevax
{
inline namespace kernel
{
  auto boot() -> void
  {
    define<library>("(meevax box)", [](library & library)
    {
      library.define<function>("box", [](let const& xs)
      {
        return make<box>(car(xs));
      });

      library.define<predicate>("box?", [](let const& xs)
      {
        return car(xs).is<box>();
      });

      library.define<accessor>("box-ref", [](let const& xs) -> auto const&
      {
        return caar(xs);
      });

      library.define<mutation>("box-set!", [](let & xs)
      {
        caar(xs) = cadr(xs);
      });
    });

    define<library>("(meevax character)", [](library & library)
    {
      library.define<predicate>("char?", [](let const& xs)
      {
        return xs[0].is<character>();
      });

      library.define<predicate>("char-alphabetic?", [](let const& xs)
      {
        return xs[0].as<character>().property().is_letter();
      });

      library.define<predicate>("char-numeric?", [](let const& xs)
      {
        return xs[0].as<character>().property().is_numeric();
      });

      library.define<predicate>("char-whitespace?", [](let const& xs)
      {
        return xs[0].as<character>().property().is_whitespace();
      });

      library.define<predicate>("char-upper-case?", [](let const& xs)
      {
        return xs[0].as<character>().property().is_upper_case();
      });

      library.define<predicate>("char-lower-case?", [](let const& xs)
      {
        return xs[0].as<character>().property().is_lower_case();
      });

      library.define<function>("digit-value", [](let const& xs)
      {
        auto digit_value = xs[0].as<character>().digit_value();
        return digit_value ? make<exact_integer>(*digit_value) : f;
      });

      library.define<function>("integer->char", [](let const& xs)
      {
        return make<character>(xs[0].as<exact_integer>());
      });

      library.define<function>("char-upcase", [](let const& xs)
      {
        return make<character>(xs[0].as<character>().upcase());
      });

      library.define<function>("char-downcase", [](let const& xs)
      {
        return make<character>(xs[0].as<character>().downcase());
      });
    });

    define<library>("(meevax complex)", [](library & library)
    {
      library.define<function>("make-rectangular", [](let const& xs)
      {
        assert(is_real(xs[0]));
        assert(is_real(xs[1]));

        return make<complex>(xs[0], xs[1]);
      });

      library.define<accessor>("real-part", [](let const& xs) -> auto const&
      {
        return car(xs[0]);
      });

      library.define<accessor>("imag-part", [](let const& xs) -> auto const&
      {
        return cdr(xs[0]);
      });
    });

    define<library>("(meevax context)", [](library & library)
    {
      library.define<command>("emergency-exit", [](let const& xs)
      {
        if (xs.is<null>())
        {
          throw EXIT_SUCCESS;
        }
        else if (let const& status = car(xs); status.is<bool>())
        {
          throw is_truthy(status) ? EXIT_SUCCESS : EXIT_FAILURE;
        }
        else
        {
          throw static_cast<int>(status.as<exact_integer>());
        }
      });

      library.define<thunk>("command-line", []()
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
      library.define<predicate>("eq?", [](let const& xs)
      {
        return eq(xs[0], xs[1]);
      });

      library.define<predicate>("eqv?", [](let const& xs)
      {
        return eqv(xs[0], xs[1]);
      });

      library.define<predicate>("equal?", [](let const& xs)
      {
        return equal(xs[0], xs[1]);
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
      library.define<syntax>("implementation-dependent",        syntax::implementation_dependent);
      library.define<syntax>("include",                         syntax::include);
      library.define<syntax>("include-case-insensitive",        syntax::include_case_insensitive);
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
      library.define<function>("environment", [](let const& xs)
      {
        auto e = environment();

        for (let const& x : xs)
        {
          e.import(x);
        }

        return make(e);
      });

      library.define<function>("eval", [](let const& xs)
      {
        return xs[1].as<environment>().evaluate(xs[0]);
      });

      library.define<thunk>("interaction-environment", []()
      {
        return interaction_environment();
      });

      library.define<command>("load", [](let const& xs)
      {
        return xs[0].as<environment>().load(xs[1].as<string>());
      });
    });

    define<library>("(meevax error)", [](library & library)
    {
      library.define<command>("throw", [](let const& xs)
      {
        throw xs[0];
      });

      library.define<function>("error-object", [](let const& xs)
      {
        return make<error>(xs[0], cdr(xs));
      });

      library.define<predicate>("error-object?", [](let const& xs)
      {
        return xs[0].is_also<error>();
      });

      library.define<predicate>("read-error?", [](let const& xs)
      {
        return xs[0].is<read_error>();
      });

      library.define<predicate>("file-error?", [](let const& xs)
      {
        return xs[0].is<file_error>();
      });

      library.define<command>("kernel-exception-handler-set!", [](let const& xs)
      {
        environment::raise = xs[0];
      });
    });

    define<library>("(meevax file)", [](library & library)
    {
      library.define<predicate>("file-exists?", [](let const& xs)
      {
        return std::filesystem::exists(static_cast<std::string>(xs[0].as<string>()));
      });

      library.define<command>("delete-file", [](let const& xs)
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
      library.define<predicate>("closure?", [](let const& xs)
      {
        return xs[0].is<closure>();
      });

      library.define<predicate>("continuation?", [](let const& xs)
      {
        return xs[0].is<continuation>();
      });

      library.define<function>("foreign-function", [](let const& xs)
      {
        return make<function>(xs[1].as<string>(), xs[0].as<string>());
      });

      library.define<predicate>("foreign-function?", [](let const& xs)
      {
        return xs[0].is_also<procedure>();
      });
    });

    define<library>("(meevax garbage-collector)", [](library & library)
    {
      library.define<command>("gc-collect", [](let const&)
      {
        gc.collect();
      });

      library.define<thunk>("gc-count", []()
      {
        return make<exact_integer>(gc.count());
      });
    });

    define<library>("(meevax inexact)", [](library & library)
    {
      library.define<predicate>("finite?", [](let const& xs)
      {
        return is_finite(xs[0]);
      });

      library.define<predicate>("infinite?", [](let const& xs)
      {
        return is_infinite(xs[0]);
      });

      library.define<predicate>("nan?", [](let const& xs)
      {
        return is_nan(xs[0]);
      });

      library.define<function>("exp", [](let const& xs)
      {
        return exp(xs[0]);
      });

      library.define<function>("sqrt", [](let const& xs)
      {
        return sqrt(xs[0]);
      });

      library.define<function>("log", [](let const& xs)
      {
        return 1 < length(xs) ? log(xs[0]) / log(xs[1])
                              : log(xs[0]);
      });

      library.define<function>("sin", [](let const& xs)
      {
        return sin(xs[0]);
      });

      library.define<function>("cos", [](let const& xs)
      {
        return cos(xs[0]);
      });

      library.define<function>("tan", [](let const& xs)
      {
        return tan(xs[0]);
      });

      library.define<function>("asin", [](let const& xs)
      {
        return asin(xs[0]);
      });

      library.define<function>("acos", [](let const& xs)
      {
        return acos(xs[0]);
      });

      library.define<function>("atan", [](let const& xs)
      {
        return 1 < length(xs) ? atan(xs[0], xs[1])
                              : atan(xs[0]);
      });

      library.define<function>("sinh", [](let const& xs)
      {
        return sinh(xs[0]);
      });

      library.define<function>("cosh", [](let const& xs)
      {
        return cosh(xs[0]);
      });

      library.define<function>("tanh", [](let const& xs)
      {
        return tanh(xs[0]);
      });

      library.define<function>("asinh", [](let const& xs)
      {
        return asinh(xs[0]);
      });

      library.define<function>("acosh", [](let const& xs)
      {
        return acosh(xs[0]);
      });

      library.define<function>("atanh", [](let const& xs)
      {
        return atanh(xs[0]);
      });
    });

    define<library>("(meevax list)", [](library & library)
    {
      library.define<predicate>("null?", [](let const& xs)
      {
        return xs[0].is<null>();
      });

      library.define<function>("make-list", [](let const& xs)
      {
        return make_list(xs[0].as<exact_integer>(), 1 < length(xs) ? xs[1] : f);
      });

      library.define<function>("length", [](let const& xs)
      {
        return make<exact_integer>(length(xs[0]));
      });

      library.define<function>("append", [](let const& xs)
      {
        return std::accumulate(std::begin(xs), std::end(xs), unit, append);
      });

      library.define<function>("reverse", [](let const& xs)
      {
        return reverse(xs[0]);
      });

      library.define<accessor>("list-tail", [](let const& xs) -> auto const&
      {
        return tail(xs[0], xs[1].as<exact_integer>());
      });

      library.define<accessor>("list-ref", [](let const& xs) -> auto const&
      {
        return xs[0][xs[1].as<exact_integer>()];
      });

      library.define<accessor>("memq", [](let const& xs) -> auto const&
      {
        return memq(xs[0], xs[1]);
      });

      library.define<accessor>("assq", [](let const& xs) -> auto const&
      {
        return assq(xs[0], xs[1]);
      });
    });

    define<library>("(meevax number)", [](library & library)
    {
      library.define<predicate>("number?", [](let const& xs)
      {
        return is_complex(xs[0]);
      });

      library.define<predicate>("complex?", [](let const& xs)
      {
        return is_complex(xs[0]);
      });

      library.define<predicate>("real?", [](let const& xs)
      {
        return is_real(xs[0]);
      });

      library.define<predicate>("rational?", [](let const& xs)
      {
        return is_rational(xs[0]);
      });

      library.define<predicate>("integer?", [](let const& xs)
      {
        return is_integer(xs[0]);
      });

      library.define<predicate>("exact-integer?", [](let const& xs)
      {
        return xs[0].is<exact_integer>();
      });

      library.define<predicate>("imaginary?", [](let const& xs)
      {
        return xs[0].is<complex>();
      });

      library.define<predicate>("ratio?", [](let const& xs)
      {
        return xs[0].is<ratio>();
      });

      library.define<predicate>("single-float?", [](let const& xs)
      {
        return xs[0].is<float>();
      });

      library.define<predicate>("double-float?", [](let const& xs)
      {
        return xs[0].is<double>();
      });

      library.define<predicate>("=", [](let const& xs)
      {
        return std::adjacent_find(std::begin(xs), std::end(xs), not_equals) == std::end(xs);
      });

      library.define<predicate>("<", [](let const& xs)
      {
        return std::adjacent_find(std::begin(xs), std::end(xs), greater_than_or_equals) == std::end(xs);
      });

      library.define<predicate>("<=", [](let const& xs)
      {
        return std::adjacent_find(std::begin(xs), std::end(xs), greater_than) == std::end(xs);
      });

      library.define<predicate>(">", [](let const& xs)
      {
        return std::adjacent_find(std::begin(xs), std::end(xs), less_than_or_equals) == std::end(xs);
      });

      library.define<predicate>(">=", [](let const& xs)
      {
        return std::adjacent_find(std::begin(xs), std::end(xs), less_than) == std::end(xs);
      });

      library.define<function>("+", [](let const& xs)
      {
        return std::accumulate(std::begin(xs), std::end(xs), e0, std::plus());
      });

      library.define<function>("*", [](let const& xs)
      {
        return std::accumulate(std::begin(xs), std::end(xs), e1, std::multiplies());
      });

      library.define<function>("-", [](let const& xs)
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

      library.define<function>("/", [](let const& xs)
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

      library.define<function>("%", [](let const& xs)
      {
        return xs[0] % xs[1];
      });

      library.define<function>("abs", [](let const& xs)
      {
        return abs(xs[0]);
      });

      library.define<function>("ratio-numerator", [](let const& xs)
      {
        return make(xs[0].as<ratio>().numerator());
      });

      library.define<function>("ratio-denominator", [](let const& xs)
      {
        return make(xs[0].as<ratio>().denominator());
      });

      library.define<function>("floor", [](let const& xs)
      {
        return floor(xs[0]);
      });

      library.define<function>("ceiling", [](let const& xs)
      {
        return ceil(xs[0]);
      });

      library.define<function>("truncate", [](let const& xs)
      {
        return trunc(xs[0]);
      });

      library.define<function>("round", [](let const& xs)
      {
        return round(xs[0]);
      });

      library.define<function>("exact-integer-square-root", [](let const& xs)
      {
        auto&& [s, r] = exact_integer_sqrt(xs[0].as<exact_integer>());

        return cons(make(std::forward<decltype(s)>(s)),
                    make(std::forward<decltype(r)>(r)));
      });

      library.define<function>("expt", [](let const& xs)
      {
        return pow(xs[0], xs[1]);
      });

      library.define<function>("exact", [](let const& xs)
      {
        return exact(xs[0]);
      });

      library.define<function>("inexact", [](let const& xs)
      {
        return inexact(xs[0]);
      });

      library.define<function>("char->integer", [](let const& xs)
      {
        return make<exact_integer>(xs[0].as<character>().codepoint);
      });

      library.define<function>("string->number", [](let const& xs)
      {
        return make_number(xs[0].as<string>(), 1 < length(xs) ? xs[1].as<exact_integer>() : 10);
      });
    });

    define<library>("(meevax pair)", [](library & library)
    {
      library.define<predicate>("pair?", [](let const& xs)
      {
        return xs[0].is<pair>();
      });

      library.define<function>("cons", [](let const& xs)
      {
        return cons(xs[0], xs[1]);
      });

      library.define<accessor>("car", [](let const& xs) -> auto const& { return car(xs[0]); });
      library.define<accessor>("cdr", [](let const& xs) -> auto const& { return cdr(xs[0]); });

      library.define<accessor>("caar", [](let const& xs) -> auto const& { return caar(xs[0]); });
      library.define<accessor>("cadr", [](let const& xs) -> auto const& { return cadr(xs[0]); });
      library.define<accessor>("cdar", [](let const& xs) -> auto const& { return cdar(xs[0]); });
      library.define<accessor>("cddr", [](let const& xs) -> auto const& { return cddr(xs[0]); });

      library.define<accessor>("caaar", [](let const& xs) -> auto const& { return caaar(xs[0]); });
      library.define<accessor>("caadr", [](let const& xs) -> auto const& { return caadr(xs[0]); });
      library.define<accessor>("cadar", [](let const& xs) -> auto const& { return cadar(xs[0]); });
      library.define<accessor>("caddr", [](let const& xs) -> auto const& { return caddr(xs[0]); });
      library.define<accessor>("cdaar", [](let const& xs) -> auto const& { return cdaar(xs[0]); });
      library.define<accessor>("cdadr", [](let const& xs) -> auto const& { return cdadr(xs[0]); });
      library.define<accessor>("cddar", [](let const& xs) -> auto const& { return cddar(xs[0]); });
      library.define<accessor>("cdddr", [](let const& xs) -> auto const& { return cdddr(xs[0]); });

      library.define<accessor>("caaaar", [](let const& xs) -> auto const& { return caaaar(xs[0]); });
      library.define<accessor>("caaadr", [](let const& xs) -> auto const& { return caaadr(xs[0]); });
      library.define<accessor>("caadar", [](let const& xs) -> auto const& { return caadar(xs[0]); });
      library.define<accessor>("caaddr", [](let const& xs) -> auto const& { return caaddr(xs[0]); });
      library.define<accessor>("cadaar", [](let const& xs) -> auto const& { return cadaar(xs[0]); });
      library.define<accessor>("cadadr", [](let const& xs) -> auto const& { return cadadr(xs[0]); });
      library.define<accessor>("caddar", [](let const& xs) -> auto const& { return caddar(xs[0]); });
      library.define<accessor>("cadddr", [](let const& xs) -> auto const& { return cadddr(xs[0]); });
      library.define<accessor>("cdaaar", [](let const& xs) -> auto const& { return cdaaar(xs[0]); });
      library.define<accessor>("cdaadr", [](let const& xs) -> auto const& { return cdaadr(xs[0]); });
      library.define<accessor>("cdadar", [](let const& xs) -> auto const& { return cdadar(xs[0]); });
      library.define<accessor>("cdaddr", [](let const& xs) -> auto const& { return cdaddr(xs[0]); });
      library.define<accessor>("cddaar", [](let const& xs) -> auto const& { return cddaar(xs[0]); });
      library.define<accessor>("cddadr", [](let const& xs) -> auto const& { return cddadr(xs[0]); });
      library.define<accessor>("cdddar", [](let const& xs) -> auto const& { return cdddar(xs[0]); });
      library.define<accessor>("cddddr", [](let const& xs) -> auto const& { return cddddr(xs[0]); });

      library.define<mutation>("set-car!", [](let & xs)
      {
        caar(xs) = cadr(xs);
      });

      library.define<mutation>("set-cdr!", [](let & xs)
      {
        cdar(xs) = cadr(xs);
      });
    });

    define<library>("(meevax port)", [](library & library)
    {
      library.define<predicate>("input-port?", [](let const& xs)
      {
        return xs[0].is_also<input_port>();
      });

      library.define<predicate>("output-port?", [](let const& xs)
      {
        return xs[0].is_also<output_port>();
      });

      library.define<predicate>("binary-port?", [](let const& xs)
      {
        return xs[0].is_also<binary_port>();
      });

      library.define<predicate>("textual-port?", [](let const& xs)
      {
        return xs[0].is_also<textual_port>();
      });

      library.define<predicate>("port?", [](let const& xs)
      {
        return xs[0].is_also<port>();
      });

      library.define<predicate>("open?", [](let const& xs)
      {
        return xs[0].as<port>().is_open();
      });

      library.define<thunk>("standard-input-port", []()
      {
        return make<standard_input_port>();
      });

      library.define<thunk>("standard-output-port", []()
      {
        return make<standard_output_port>();
      });

      library.define<thunk>("standard-error-port", []()
      {
        return make<standard_error_port>();
      });

      library.define<function>("open-input-file", [](let const& xs)
      {
        return make<input_file_port>(xs[0].as<string>());
      });

      library.define<function>("open-output-file", [](let const& xs)
      {
        return make<output_file_port>(xs[0].as<string>());
      });

      library.define<function>("open-binary-input-file", [](let const& xs)
      {
        return make<binary_input_file_port>(xs[0].as<string>());
      });

      library.define<function>("open-binary-output-file", [](let const& xs)
      {
        return make<binary_output_file_port>(xs[0].as<string>());
      });

      library.define<command>("close", [](let const& xs)
      {
        xs[0].as<port>().close();
      });

      library.define<function>("open-input-string", [](let const& xs)
      {
        return make<input_string_port>(xs[0].as<string>());
      });

      library.define<function>("open-output-string", [](let const&)
      {
        return make<output_string_port>();
      });

      library.define<function>("get-output-string", [](let const& xs)
      {
        return make<string>(xs[0].as<output_string_port>().ostringstream.str());
      });

      library.define<function>("open-input-u8vector", [](let const& xs)
      {
        return make<input_u8vector_port>(xs[0].as<u8vector>());
      });

      library.define<function>("open-output-u8vector", [](let const&)
      {
        return make<output_u8vector_port>();
      });

      library.define<function>("get-output-u8vector", [](let const& xs)
      {
        return make<u8vector>(xs[0].as<output_u8vector_port>().vector);
      });

      library.define<predicate>("eof-object?", [](let const& xs)
      {
        return xs[0].is<eof>();
      });

      library.define<thunk>("eof-object", []()
      {
        return eof_object;
      });

      library.define<command>("flush", [](let const& xs)
      {
        xs[0].as<output_port>().flush();
      });
    });

    define<library>("(meevax read)", [](library & library)
    {
      library.define<function>("get-char", [](let const& xs)
      {
        return xs[0].as<textual_input_port>().get();
      });

      library.define<predicate>("get-char-ready?", [](let const& xs)
      {
        return xs[0].as<textual_input_port>().get_ready();
      });

      library.define<function>("get-line", [](let const& xs)
      {
        return xs[0].as<textual_input_port>().get_line();
      });

      library.define<function>("get-string", [](let const& xs)
      {
        return xs[1].as<textual_input_port>().get(xs[0].as<exact_integer>());
      });

      library.define<function>("peek-char", [](let const& xs)
      {
        return xs[0].as<textual_input_port>().peek();
      });

      library.define<function>("get-u8", [](let const& xs)
      {
        return xs[0].as<binary_input_port>().get();
      });

      library.define<predicate>("get-u8-ready?", [](let const& xs)
      {
        return xs[0].as<binary_input_port>().get_ready();
      });

      library.define<function>("peek-u8", [](let const& xs)
      {
        return xs[0].as<binary_input_port>().peek();
      });

      library.define<function>("get-u8vector", [](let const& xs)
      {
        return xs[1].as<binary_input_port>().get(xs[0].as<exact_integer>());
      });

      library.define<function>("read", [](let const& xs)
      {
        return xs[0].as<textual_input_port>().read();
      });
    });

    define<library>("(meevax string)", [](library & library)
    {
      library.define<predicate>("string?", [](let const& xs)
      {
        return xs[0].is<string>();
      });

      library.define<function>("make-string", [](let const& xs)
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
                            1 < length(xs) ? xs[1].as<character>() : character());
      });

      library.define<function>("string-length", [](let const& xs)
      {
        /*
           (string-length string)                                     procedure

           Returns the number of characters in the given string.
        */

        return make<exact_integer>(xs[0].as<string>().vector.size());
      });

      library.define<function>("string-ref", [](let const& xs)
      {
        /*
           (string-ref string k)                                      procedure

           It is an error if k is not a valid index of string.

           The string-ref procedure returns character k of string using
           zero-origin indexing. There is no requirement for this procedure to
           execute in constant time.
        */

        return make(xs[0].as<string>().vector.at(xs[1].as<exact_integer>()));
      });

      library.define<mutation>("string-set!", [](let & xs)
      {
        /*
           (string-set! string k char)                                procedure

           It is an error if k is not a valid index of string.

           The string-set! procedure stores char in element k of string. There
           is no requirement for this procedure to execute in constant time.
        */

        xs[0].as<string>().vector.at(xs[1].as<exact_integer>()) = xs[2].as<character>();
      });

      library.define<function>("string-append", [](let const& xs)
      {
        /*
           (string-append string ...)                                 procedure

           Returns a newly allocated string whose characters are the
           concatenation of the characters in the given strings.
        */

        auto&& s = string();

        for (let const& x : xs)
        {
          std::copy(std::begin(x.as<string>().vector),
                    std::end(x.as<string>().vector),
                    std::back_inserter(s.vector));
        }

        return make(std::forward<decltype(s)>(s));
      });

      library.define<function>("string-copy", [](let const& xs)
      {
        /*
           (string-copy string)                                       procedure
           (string-copy string start)                                 procedure
           (string-copy string start end)                             procedure

           Returns a newly allocated copy of the part of the given string
           between start and end.
        */

        auto&& s = string();

        std::copy(std::next(std::begin(xs[0].as<string>().vector), 1 < length(xs) ? xs[1].as<exact_integer>() : 0),
                  std::next(std::begin(xs[0].as<string>().vector), 2 < length(xs) ? xs[2].as<exact_integer>() : xs[0].as<string>().vector.size()),
                  std::back_inserter(s.vector));

        return make(s);
      });

      library.define<command>("string-copy!", [](let const& xs)
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

        auto&& s1 = xs[0].as<string>().vector;

        auto&& s2 = xs[2].as<string>().vector;

        s1.reserve(s1.size() + s2.size());

        std::copy(std::next(std::begin(s2), 3 < length(xs) ? xs[3].as<exact_integer>() : 0),
                  std::next(std::begin(s2), 4 < length(xs) ? xs[4].as<exact_integer>() : s2.size()),
                  std::next(std::begin(s1), xs[1].as<exact_integer>()));
      });

      #define STRING_COMPARE(COMPARE)                                          \
      [](let const& xs)                                                        \
      {                                                                        \
        return std::adjacent_find(                                             \
                 std::begin(xs), std::end(xs), [](let const& a, let const& b)  \
                 {                                                             \
                   return not COMPARE()(a.as_const<string>().vector,           \
                                        b.as_const<string>().vector);          \
                 }) == std::end(xs);                                           \
      }

      library.define<predicate>("string=?",  STRING_COMPARE(std::equal_to     ));
      library.define<predicate>("string<?",  STRING_COMPARE(std::less         ));
      library.define<predicate>("string<=?", STRING_COMPARE(std::less_equal   ));
      library.define<predicate>("string>?",  STRING_COMPARE(std::greater      ));
      library.define<predicate>("string>=?", STRING_COMPARE(std::greater_equal));

      #undef STRING_COMPARE

      library.define<function>("symbol->string", [](let const& xs)
      {
        return make<string>(xs[0].as<symbol>());
      });

      library.define<function>("string->symbol", [](let const& xs)
      {
        return make_symbol(xs[0].as<string>());
      });

      library.define<function>("number->string", [](let const& xs)
      {
        return number_to_string(xs[0], 1 < length(xs) ? xs[1].as<exact_integer>() : 10);
      });

      library.define<function>("string->list", [](let const& xs)
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

        return std::accumulate(std::prev(std::rend(xs[0].as<string>().vector), 2 < length(xs) ? xs[2].as<exact_integer>() : xs[0].as<string>().vector.size()),
                               std::prev(std::rend(xs[0].as<string>().vector), 1 < length(xs) ? xs[1].as<exact_integer>() : 0),
                               unit,
                               [](let const& xs, character const& c)
                               {
                                 return cons(make(c), xs);
                               });
      });

      library.define<function>("list->string", [](let const& xs)
      {
        auto&& s = string();

        for (let const& x : xs[0])
        {
          s.vector.push_back(x.as<character>());
        }

        return make(std::forward<decltype(s)>(s));
      });
    });

    define<library>("(meevax symbol)", [](library & library)
    {
      library.define<predicate>("symbol?", [](let const& xs)
      {
        return xs[0].is<symbol>();
      });

      using syntactic_closure = environment::syntactic_closure;

      library.define<function>("identifier->symbol", [](let const& xs)
      {
        if (let const& x = xs[0]; x.is<syntactic_closure>())
        {
          return cddr(x);
        }
        else
        {
          return x;
        }
      });
    });

    define<library>("(meevax syntactic-closure)", [](library & library)
    {
      using syntactic_closure = environment::syntactic_closure;

      library.define<predicate>("identifier?", [](let const& xs)
      {
        return xs[0].is_also<identifier>();
      });

      library.define<predicate>("transformer?", [](let const& xs)
      {
        return xs[0].is<transformer>();
      });

      library.define<predicate>("syntactic-closure?", [](let const& xs)
      {
        return xs[0].is<syntactic_closure>();
      });

      library.define<function>("make-syntactic-closure", [](let const& xs)
      {
        return make<syntactic_closure>(xs[0], xs[1], xs[2]);
      });
    });

    define<library>("(meevax system)", [](library & library)
    {
      library.define<function>("get-environment-variable", [](let const& xs)
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

      library.define<thunk>("get-environment-variables", []()
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
      library.define<thunk>("current-jiffy", []()
      {
        return make<exact_integer>(std::chrono::high_resolution_clock::now().time_since_epoch().count());
      });

      library.define<thunk>("jiffies-per-second", []()
      {
        return make<exact_integer>(std::chrono::high_resolution_clock::period::den);
      });
    });

    define<library>("(meevax vector)", [](library & library)
    {
      library.define<predicate>("vector?", [](let const& xs)
      {
        return xs[0].is<vector>();
      });

      library.define<function>("vector", [](let const& xs)
      {
        /*
           (vector obj ...)                                           procedure

           Returns a newly allocated vector whose elements contain the given
           arguments. It is analogous to list.
        */

        return make<vector>(xs);
      });

      library.define<function>("make-vector", [](let const& xs)
      {
        /*
           (make-vector k)                                            procedure
           (make-vector k fill)                                       procedure

           Returns a newly allocated vector of k elements. If a second argument
           is given, then each element is initialized to fill. Otherwise the
           initial contents of each element is unspecified.
        */

        return make<vector>(xs[0].as<exact_integer>(), 1 < length(xs) ? xs[1] : unspecified);
      });

      library.define<function>("vector-append", [](let const& xs)
      {
        /*
           (vector-append vector ...)                                 procedure

           Returns a newly allocated vector whose elements are the
           concatenation of the elements of the given vectors.
        */

        auto&& v = vector();

        for (let const& x : xs)
        {
          for (let const& object : x.as<vector>().vector)
          {
            v.vector.push_back(object);
          }
        }

        return make(std::forward<decltype(v)>(v));
      });

      library.define<function>("vector-copy", [](let const& xs)
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

        std::copy(std::next(std::begin(xs[0].as<vector>().vector), 1 < length(xs) ? xs[1].as<exact_integer>() : 0),
                  std::next(std::begin(xs[0].as<vector>().vector), 2 < length(xs) ? xs[2].as<exact_integer>() : xs[0].as<vector>().vector.size()),
                  std::back_inserter(v.vector));

        return make(std::forward<decltype(v)>(v));
      });

      library.define<command>("vector-copy!", [](let const& xs)
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

        auto&& v1 = xs[0].as<vector>().vector;

        auto&& v2 = xs[2].as<vector>().vector;

        v1.reserve(v1.size() + v2.size());

        std::copy(std::next(std::begin(v2), 3 < length(xs) ? xs[3].as<exact_integer>() : 0),
                  std::next(std::begin(v2), 4 < length(xs) ? xs[4].as<exact_integer>() : v1.size()),
                  std::next(std::begin(v1), xs[1].as<exact_integer>()));
      });

      library.define<function>("vector-length", [](let const& xs)
      {
        /*
           (vector-length vector)                                     procedure

           Returns the number of elements in vector as an exact integer.
        */

        return make<exact_integer>(xs[0].as<vector>().vector.size());
      });

      library.define<accessor>("vector-ref", [](let const& xs) -> auto const&
      {
        /*
           (vector-ref vector k)                                      procedure

           It is an error if k is not a valid index of vector. The vector-ref
           procedure returns the contents of element k of vector.
        */

        return xs[0][xs[1].as<exact_integer>()];
      });

      library.define<mutation>("vector-set!", [](let & xs)
      {
        /*
           (vector-set! vector k obj)                                 procedure

           It is an error if k is not a valid index of vector. The vector-set!
           procedure stores obj in element k of vector.
        */

        xs[0].as<vector>().vector[xs[1].as<exact_integer>()] = xs[2];
      });

      library.define<mutation>("vector-fill!", [](let & xs)
      {
        /*
           (vector-fill! vector fill)                                 procedure
           (vector-fill! vector fill start)                           procedure
           (vector-fill! vector fill start end)                       procedure

           The vector-fill! procedure stores fill in the elements of vector
           between start and end.
        */

        std::fill(std::next(std::begin(xs[0].as<vector>().vector), 2 < length(xs) ? xs[2].as<exact_integer>() : 0),
                  std::next(std::begin(xs[0].as<vector>().vector), 3 < length(xs) ? xs[3].as<exact_integer>() : xs[0].as<vector>().vector.size()),
                  1 < length(xs) ? xs[1] : unspecified);
      });

      library.define<function>("vector->list", [](let const& xs)
      {
        return std::accumulate(std::prev(std::rend(xs[0].as<vector>().vector), 2 < length(xs) ? xs[2].as<exact_integer>() : xs[0].as<vector>().vector.size()),
                               std::prev(std::rend(xs[0].as<vector>().vector), 1 < length(xs) ? xs[1].as<exact_integer>() : 0),
                               unit,
                               xcons);
      });

      library.define<function>("list->vector", [](let const& xs)
      {
        return make<vector>(xs[0]);
      });

      library.define<function>("vector->string", [](let const& xs)
      {
        auto s = string();

        std::for_each(std::next(std::begin(xs[0].as<vector>().vector), 1 < length(xs) ? xs[1].as<exact_integer>() : 0),
                      std::next(std::begin(xs[0].as<vector>().vector), 2 < length(xs) ? xs[2].as<exact_integer>() : xs[0].as<vector>().vector.size()),
                      [&](let const& x)
                      {
                        s.vector.push_back(x.as<character>());
                      });

        return make(s);
      });

      library.define<function>("string->vector", [](let const& xs)
      {
        auto v = vector();

        for (auto&& character : xs[0].as<string>().vector)
        {
          v.vector.push_back(make(character));
        }

        return make(v);
      });
    });

    define<library>("(meevax vector homogeneous)", [](library & library)
    {
      #define DEFINE_HOMOGENEOUS_VECTOR(TAG)                                   \
      library.define<predicate>(#TAG "vector?", [](let const& xs)              \
      {                                                                        \
        return xs[0].is<TAG##vector>();                                        \
      });                                                                      \
                                                                               \
      library.define<function>("make-" #TAG "vector", [](let const& xs)        \
      {                                                                        \
        return make<TAG##vector>(xs[0].as<exact_integer>(), 1 < length(xs) ? xs[1] : unspecified); \
      });                                                                      \
                                                                               \
      library.define<function>(#TAG "vector", [](let const& xs)                \
      {                                                                        \
        return make<TAG##vector>(xs);                                          \
      });                                                                      \
                                                                               \
      library.define<function>(#TAG "vector-length", [](let const& xs)         \
      {                                                                        \
        return make<exact_integer>(xs[0].as<TAG##vector>().valarray.size());   \
      });                                                                      \
                                                                               \
      library.define<function>(#TAG "vector-ref", [](let const& xs)            \
      {                                                                        \
        return TAG##vector::output_cast(xs[0].as<TAG##vector>().valarray[xs[1].as<exact_integer>()]); \
      });                                                                      \
                                                                               \
      library.define<command>(#TAG "vector-set!", [](let const& xs)            \
      {                                                                        \
        xs[0].as<TAG##vector>().valarray[xs[1].as<exact_integer>()] = TAG##vector::input_cast(xs[2]); \
      });                                                                      \
                                                                               \
      library.define<function>(#TAG "vector-copy", [](let const& xs)           \
      {                                                                        \
        return make<TAG##vector>(xs[0].as<TAG##vector>(),                      \
                                 1 < length(xs) ? xs[1].as<exact_integer>() : std::size_t(), \
                                 2 < length(xs) ? xs[2].as<exact_integer>() : xs[0].as<TAG##vector>().valarray.size()); \
      });                                                                      \
                                                                               \
      library.define<mutation>(#TAG "vector-copy!", [](let & xs)               \
      {                                                                        \
        auto copy = [](auto&& to, auto&& at, auto&& from, auto&& start, auto&& end) \
        {                                                                      \
          to[std::slice(at, end - start, 1)] = from[std::slice(start, end - start, 1)]; \
        };                                                                     \
                                                                               \
        copy(xs[0].as<TAG##vector>().valarray,                                 \
             xs[1].as<exact_integer>(),                                        \
             xs[2].as<TAG##vector>().valarray,                                 \
             3 < length(xs) ? xs[3].as<exact_integer>() : 0,                   \
             4 < length(xs) ? xs[4].as<exact_integer>() : xs[2].as<TAG##vector>().valarray.size()); \
      });                                                                      \
                                                                               \
      library.define<function>(#TAG "vector-append", [](let const& xs)         \
      {                                                                        \
        return make<TAG##vector>(xs[0].as<TAG##vector>(),                      \
                                 xs[1].as<TAG##vector>());                     \
      });                                                                      \
                                                                               \
      library.define<function>(#TAG "vector->list", [](let const& xs)          \
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
                    1 < length(xs) ? xs[1].as<exact_integer>() : 0,            \
                    2 < length(xs) ? xs[2].as<exact_integer>() : xs[0].as<TAG##vector>().valarray.size()); \
      });                                                                      \
                                                                               \
      library.define<function>("list->" #TAG "vector", [](let const& xs)       \
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

      library.define<function>("u8vector->string", [](let const& xs)
      {
        auto buffer = std::ostringstream();

        std::for_each(std::next(std::begin(xs[0].as<u8vector>().valarray), 1 < length(xs) ? xs[1].as<exact_integer>() : 0),
                      std::next(std::begin(xs[0].as<u8vector>().valarray), 2 < length(xs) ? xs[2].as<exact_integer>() : xs[0].as<u8vector>().valarray.size()),
                      [&](auto const& x)
                      {
                        buffer << x;
                      });

        return input_string_port(buffer.str()).get(std::numeric_limits<std::size_t>::max());
      });

      library.define<function>("string->u8vector", [](let const& xs)
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
      library.define<thunk>("features", []()
      {
        return features();
      });
    });

    define<library>("(meevax write)", [](library & library)
    {
      library.define<command>("put-char", [](let const& xs)
      {
        xs[1].as<textual_output_port>().put(xs[0].as<character>());
      });

      library.define<command>("put-string", [](let const& xs)
      {
        xs[1].as<textual_output_port>().put(xs[0].as<string>());
      });

      library.define<command>("put-u8", [](let const& xs)
      {
        xs[1].as<binary_output_port>().put(xs[0].as<exact_integer>());
      });

      library.define<command>("put-u8vector", [](let const& xs)
      {
        xs[1].as<binary_output_port>().put(xs[0].as<u8vector>());
      });

      library.define<command>("write", [](let const& xs)
      {
        xs[1].as<textual_output_port>().write(xs[0]);
      });

      library.define<command>("write-simple", [](let const& xs)
      {
        xs[1].as<textual_output_port>().write_simple(xs[0]);
      });
    });

    auto boot_loader = environment();

    for (auto each : basis())
    {
      for (let const& x : input_string_port(each))
      {
        boot_loader.evaluate(x);
      }
    }
  }
} // namespace kernel
} // namespace meevax
