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
    define<library>("(meevax boolean)", [](library & library)
    {
      library.define<procedure>("boolean?", [](let const& xs)
      {
        return xs[0].is<bool>();
      });

      library.define<procedure>("not", [](let const& xs)
      {
        return not is_truthy(xs[0]);
      });
    });

    define<library>("(meevax box)", [](library & library)
    {
      library.define<procedure>("box", [](let const& xs)
      {
        return make<box>(car(xs));
      });

      library.define<procedure>("box?", [](let const& xs)
      {
        return car(xs).is<box>();
      });

      library.define<procedure>("box-ref", [](let const& xs) -> auto const&
      {
        return caar(xs);
      });

      library.define<procedure>("box-set!", [](let & xs)
      {
        caar(xs) = cadr(xs);
      });
    });

    define<library>("(meevax character)", [](library & library)
    {
      library.define<procedure>("char?", [](let const& xs)
      {
        return xs[0].is<character>();
      });

      library.define<procedure>("char=?", [](let const& xs)
      {
        auto compare = [](let const& a, let const& b)
        {
          return not (a.as<character>().codepoint == b.as<character>().codepoint);
        };

        return std::adjacent_find(xs.begin(), xs.end(), compare) == xs.end();
      });

      library.define<procedure>("char<?", [](let const& xs)
      {
        auto compare = [](let const& a, let const& b)
        {
          return not (a.as<character>().codepoint < b.as<character>().codepoint);
        };

        return std::adjacent_find(xs.begin(), xs.end(), compare) == xs.end();
      });

      library.define<procedure>("char>?", [](let const& xs)
      {
        auto compare = [](let const& a, let const& b)
        {
          return not (a.as<character>().codepoint > b.as<character>().codepoint);
        };

        return std::adjacent_find(xs.begin(), xs.end(), compare) == xs.end();
      });

      library.define<procedure>("char<=?", [](let const& xs)
      {
        auto compare = [](let const& a, let const& b)
        {
          return not (a.as<character>().codepoint <= b.as<character>().codepoint);
        };

        return std::adjacent_find(xs.begin(), xs.end(), compare) == xs.end();
      });

      library.define<procedure>("char>=?", [](let const& xs)
      {
        auto compare = [](let const& a, let const& b)
        {
          return not (a.as<character>().codepoint >= b.as<character>().codepoint);
        };

        return std::adjacent_find(xs.begin(), xs.end(), compare) == xs.end();
      });

      library.define<procedure>("char-ci=?", [](let const& xs)
      {
        auto compare = [](let const& a, let const& b)
        {
          return not (a.as<character>().downcase() == b.as<character>().downcase());
        };

        return std::adjacent_find(xs.begin(), xs.end(), compare) == xs.end();
      });

      library.define<procedure>("char-ci<?", [](let const& xs)
      {
        auto compare = [](let const& a, let const& b)
        {
          return not (a.as<character>().downcase() < b.as<character>().downcase());
        };

        return std::adjacent_find(xs.begin(), xs.end(), compare) == xs.end();
      });

      library.define<procedure>("char-ci>?", [](let const& xs)
      {
        auto compare = [](let const& a, let const& b)
        {
          return not (a.as<character>().downcase() > b.as<character>().downcase());
        };

        return std::adjacent_find(xs.begin(), xs.end(), compare) == xs.end();
      });

      library.define<procedure>("char-ci<=?", [](let const& xs)
      {
        auto compare = [](let const& a, let const& b)
        {
          return not (a.as<character>().downcase() <= b.as<character>().downcase());
        };

        return std::adjacent_find(xs.begin(), xs.end(), compare) == xs.end();
      });

      library.define<procedure>("char-ci>=?", [](let const& xs)
      {
        auto compare = [](let const& a, let const& b)
        {
          return not (a.as<character>().downcase() >= b.as<character>().downcase());
        };

        return std::adjacent_find(xs.begin(), xs.end(), compare) == xs.end();
      });

      library.define<procedure>("char-alphabetic?", [](let const& xs)
      {
        return xs[0].as<character>().property().is_letter();
      });

      library.define<procedure>("char-numeric?", [](let const& xs)
      {
        return xs[0].as<character>().property().is_numeric();
      });

      library.define<procedure>("char-whitespace?", [](let const& xs)
      {
        return xs[0].as<character>().property().is_whitespace();
      });

      library.define<procedure>("char-upper-case?", [](let const& xs)
      {
        return xs[0].as<character>().property().is_upper_case();
      });

      library.define<procedure>("char-lower-case?", [](let const& xs)
      {
        return xs[0].as<character>().property().is_lower_case();
      });

      library.define<procedure>("digit-value", [](let const& xs)
      {
        auto digit_value = xs[0].as<character>().digit_value();
        return digit_value ? make<exact_integer>(*digit_value) : f;
      });

      library.define<procedure>("char->integer", [](let const& xs)
      {
        return make<exact_integer>(xs[0].as<character>().codepoint);
      });

      library.define<procedure>("integer->char", [](let const& xs)
      {
        return make<character>(xs[0].as<exact_integer>());
      });

      library.define<procedure>("char-upcase", [](let const& xs)
      {
        return make<character>(xs[0].as<character>().upcase());
      });

      library.define<procedure>("char-downcase", [](let const& xs)
      {
        return make<character>(xs[0].as<character>().downcase());
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

      library.define<procedure>("make-polar", [](let const& xs)
      {
        let const& radius = xs[0], angle = xs[1];

        return make<complex>(radius * cos(angle),
                             radius * sin(angle));
      });

      library.define<procedure>("real-part", [](let const& xs) -> auto const&
      {
        return real_part(xs[0]);
      });

      library.define<procedure>("imag-part", [](let const& xs) -> auto const&
      {
        return imag_part(xs[0]);
      });

      library.define<procedure>("magnitude", [](let const& xs)
      {
        return magnitude(xs[0]);
      });

      library.define<procedure>("angle", [](let const& xs)
      {
        return angle(xs[0]);
      });
    });

    define<library>("(meevax context)", [](library & library)
    {
      library.define<procedure>("emergency-exit", [](let const& xs)
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

      library.define<procedure>("command-line", []()
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

      library.define<procedure>("equal?", [](let const& xs)
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
      library.define<procedure>("environment", [](let const& xs)
      {
        auto e = environment();

        for (let const& x : xs)
        {
          e.import(x);
        }

        return make(e);
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
      library.define<procedure>("throw", [](let const& xs)
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
        environment::raise = xs[0];
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

    define<library>("(meevax garbage-collector)", [](library & library)
    {
      library.define<procedure>("gc-collect", [](let const&)
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
        return 1 < length(xs) ? log(xs[0]) / log(xs[1])
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
        return 1 < length(xs) ? atan(xs[0], xs[1])
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

      library.define<procedure>("list?", [](let const& xs)
      {
        return is_list(xs[0]);
      });

      library.define<procedure>("list", [](let const& xs) -> auto const&
      {
        return xs;
      });

      library.define<procedure>("make-list", [](let const& xs)
      {
        switch (length(xs))
        {
        case 1:
          return make_list(xs[0].as<exact_integer>());

        case 2:
          return make_list(xs[0].as<exact_integer>(), xs[1]);

        default:
          throw error(make<string>("procedure make-list takes one or two arugments, but got"), xs);
        }
      });

      library.define<procedure>("iota", [](let const& xs)
      {
        switch (length(xs))
        {
        case 1:
          return iota(xs[0].as<exact_integer>());

        case 2:
          return iota(xs[0].as<exact_integer>(), xs[1]);

        case 3:
          return iota(xs[0].as<exact_integer>(), xs[1], xs[2]);

        default:
          throw error(make<string>("procedure iota takes one to three arugments, but got"), xs);
        }
      });

      library.define<procedure>("circular-list?", [](let const& xs)
      {
        return is_circular_list(xs[0]);
      });

      library.define<procedure>("circular-list", [](let & xs)
      {
        circulate(xs);
        return xs;
      });

      library.define<procedure>("dotted-list?", [](let const& xs)
      {
        return is_dotted_list(xs[0]);
      });

      library.define<procedure>("null-list?", [](let const& xs)
      {
        if (is_list(xs[0]) or is_circular_list(xs[0]))
        {
          return xs[0].is<null>();
        }
        else
        {
          throw error(make<string>("procedure null-list? takes a proper-list or a circular-list, but got"), xs);
        }
      });

      library.define<procedure>("last", [](let const& xs) -> auto const&
      {
        return last(xs[0]);
      });

      library.define<procedure>("last-pair", [](let const& xs) -> auto const&
      {
        return last_pair(xs[0]);
      });

      library.define<procedure>("length", [](let const& xs)
      {
        return make<exact_integer>(length(xs[0]));
      });

      library.define<procedure>("append", [](let const& xs)
      {
        return std::accumulate(std::begin(xs), std::end(xs), unit, append);
      });

      library.define<procedure>("reverse", [](let const& xs)
      {
        return reverse(xs[0]);
      });

      library.define<procedure>("list-copy", [](let const& xs)
      {
        return list_copy(xs[0]);
      });

      library.define<procedure>("list-tail", [](let const& xs) -> auto const&
      {
        return tail(xs[0], xs[1].as<exact_integer>());
      });

      library.define<procedure>("list-ref", [](let const& xs) -> auto const&
      {
        return xs[0][xs[1].as<exact_integer>()];
      });

      library.define<procedure>("first", [](let const& xs) -> decltype(auto)
      {
        return xs[0][0];
      });

      library.define<procedure>("second", [](let const& xs) -> decltype(auto)
      {
        return xs[0][1];
      });

      library.define<procedure>("third", [](let const& xs) -> decltype(auto)
      {
        return xs[0][2];
      });

      library.define<procedure>("fourth", [](let const& xs) -> decltype(auto)
      {
        return xs[0][3];
      });

      library.define<procedure>("fifth", [](let const& xs) -> decltype(auto)
      {
        return xs[0][4];
      });

      library.define<procedure>("sixth", [](let const& xs) -> decltype(auto)
      {
        return xs[0][5];
      });

      library.define<procedure>("seventh", [](let const& xs) -> decltype(auto)
      {
        return xs[0][6];
      });

      library.define<procedure>("eighth", [](let const& xs) -> decltype(auto)
      {
        return xs[0][7];
      });

      library.define<procedure>("ninth", [](let const& xs) -> decltype(auto)
      {
        return xs[0][8];
      });

      library.define<procedure>("tenth", [](let const& xs) -> decltype(auto)
      {
        return xs[0][9];
      });

      library.define<procedure>("take", [](let const& xs)
      {
        return take(xs[0], xs[1].as<exact_integer>());
      });

      library.define<procedure>("drop", [](let const& xs)
      {
        return drop(xs[0], xs[1].as<exact_integer>());
      });

      library.define<procedure>("memq", [](let const& xs) -> auto const&
      {
        return memq(xs[0], xs[1]);
      });

      library.define<procedure>("memv", [](let const& xs) -> auto const&
      {
        return memv(xs[0], xs[1]);
      });

      library.define<procedure>("assq", [](let const& xs) -> auto const&
      {
        return assq(xs[0], xs[1]);
      });

      library.define<procedure>("assv", [](let const& xs) -> auto const&
      {
        return assv(xs[0], xs[1]);
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

      library.define<procedure>("exact?", [](let const& xs)
      {
        return is_exact(xs[0]);
      });

      library.define<procedure>("inexact?", [](let const& xs)
      {
        return is_inexact(xs[0]);
      });

      library.define<procedure>("exact-integer?", [](let const& xs)
      {
        return xs[0].is<exact_integer>();
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

      library.define<procedure>("zero?", [](let const& xs)
      {
        return is_zero(xs[0]);
      });

      library.define<procedure>("positive?", [](let const& xs)
      {
        return is_positive(xs[0]);
      });

      library.define<procedure>("negative?", [](let const& xs)
      {
        return is_negative(xs[0]);
      });

      library.define<procedure>("odd?", [](let const& xs)
      {
        return is_odd(xs[0]);
      });

      library.define<procedure>("even?", [](let const& xs)
      {
        return is_even(xs[0]);
      });

      library.define<procedure>("max", [](let const& xs)
      {
        return max(xs);
      });

      library.define<procedure>("min", [](let const& xs)
      {
        return min(xs);
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

      library.define<procedure>("abs", [](let const& xs)
      {
        return abs(xs[0]);
      });

      library.define<procedure>("quotient", [](let const& xs)
      {
        return quotient(xs[0], xs[1]);
      });

      library.define<procedure>("remainder", [](let const& xs)
      {
        return remainder(xs[0], xs[1]);
      });

      library.define<procedure>("modulo", [](let const& xs)
      {
        return modulo(xs[0], xs[1]);
      });

      library.define<procedure>("gcd", [](let const& xs)
      {
        switch (length(xs))
        {
        case 0:
          return e0;

        case 1:
          return xs[0];

        default:
          return std::accumulate(cdr(xs).begin(), xs.end(), xs[0], gcd);
        }
      });

      library.define<procedure>("lcm", [](let const& xs)
      {
        switch (length(xs))
        {
        case 0:
          return e1;

        case 1:
          return xs[0];

        default:
          return std::accumulate(cdr(xs).begin(), xs.end(), xs[0], lcm);
        }
      });

      library.define<procedure>("numerator", [](let const& xs)
      {
        return numerator(xs[0]);
      });

      library.define<procedure>("denominator", [](let const& xs)
      {
        return denominator(xs[0]);
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
        auto&& [s, r] = xs[0].as<exact_integer>().square_root();

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

      library.define<procedure>("number->string", [](let const& xs)
      {
        switch (length(xs))
        {
        case 1:
          return number_to_string(xs[0], 10);

        case 2:
          return number_to_string(xs[0], xs[1].as<exact_integer>());

        default:
          throw error(make<string>("procedure number->string takes one or two arugments, but got"), xs);
        }
      });

      library.define<procedure>("string->number", [](let const& xs)
      {
        switch (length(xs))
        {
        case 1:
          return make_number(xs[0].as<string>(), 10);

        case 2:
          return make_number(xs[0].as<string>(), xs[1].as<exact_integer>());

        default:
          throw error(make<string>("procedure string->number takes one or two arugments, but got"), xs);
        }
      });
    });

    define<library>("(meevax pair)", [](library & library)
    {
      library.define<procedure>("pair?", [](let const& xs)
      {
        return xs[0].is<pair>();
      });

      library.define<procedure>("not-pair?", [](let const& xs)
      {
        return not xs[0].is<pair>();
      });

      library.define<procedure>("cons", [](let const& xs)
      {
        return cons(xs[0], xs[1]);
      });

      library.define<procedure>("cons*", [](let & xs)
      {
        if (xs.is<null>())
        {
          throw error(make<string>("procedure cons* takes at least one arugments, but got"), xs);
        }
        else if (cdr(xs).is<null>())
        {
          return xs[0];
        }
        else
        {
          auto node = xs.get();

          while (not cddr(*node).is<null>())
          {
            node = cdr(*node).get();
          }

          cdr(*node) = cadr(*node);

          return xs;
        }
      });

      library.define<procedure>("xcons", [](let const& xs)
      {
        return cons(xs[1], xs[0]);
      });

      library.define<procedure>("car", [](let const& xs) -> auto const& { return car(xs[0]); });
      library.define<procedure>("cdr", [](let const& xs) -> auto const& { return cdr(xs[0]); });

      library.define<procedure>("caar", [](let const& xs) -> auto const& { return caar(xs[0]); });
      library.define<procedure>("cadr", [](let const& xs) -> auto const& { return cadr(xs[0]); });
      library.define<procedure>("cdar", [](let const& xs) -> auto const& { return cdar(xs[0]); });
      library.define<procedure>("cddr", [](let const& xs) -> auto const& { return cddr(xs[0]); });

      library.define<procedure>("caaar", [](let const& xs) -> auto const& { return caaar(xs[0]); });
      library.define<procedure>("caadr", [](let const& xs) -> auto const& { return caadr(xs[0]); });
      library.define<procedure>("cadar", [](let const& xs) -> auto const& { return cadar(xs[0]); });
      library.define<procedure>("caddr", [](let const& xs) -> auto const& { return caddr(xs[0]); });
      library.define<procedure>("cdaar", [](let const& xs) -> auto const& { return cdaar(xs[0]); });
      library.define<procedure>("cdadr", [](let const& xs) -> auto const& { return cdadr(xs[0]); });
      library.define<procedure>("cddar", [](let const& xs) -> auto const& { return cddar(xs[0]); });
      library.define<procedure>("cdddr", [](let const& xs) -> auto const& { return cdddr(xs[0]); });

      library.define<procedure>("caaaar", [](let const& xs) -> auto const& { return caaaar(xs[0]); });
      library.define<procedure>("caaadr", [](let const& xs) -> auto const& { return caaadr(xs[0]); });
      library.define<procedure>("caadar", [](let const& xs) -> auto const& { return caadar(xs[0]); });
      library.define<procedure>("caaddr", [](let const& xs) -> auto const& { return caaddr(xs[0]); });
      library.define<procedure>("cadaar", [](let const& xs) -> auto const& { return cadaar(xs[0]); });
      library.define<procedure>("cadadr", [](let const& xs) -> auto const& { return cadadr(xs[0]); });
      library.define<procedure>("caddar", [](let const& xs) -> auto const& { return caddar(xs[0]); });
      library.define<procedure>("cadddr", [](let const& xs) -> auto const& { return cadddr(xs[0]); });
      library.define<procedure>("cdaaar", [](let const& xs) -> auto const& { return cdaaar(xs[0]); });
      library.define<procedure>("cdaadr", [](let const& xs) -> auto const& { return cdaadr(xs[0]); });
      library.define<procedure>("cdadar", [](let const& xs) -> auto const& { return cdadar(xs[0]); });
      library.define<procedure>("cdaddr", [](let const& xs) -> auto const& { return cdaddr(xs[0]); });
      library.define<procedure>("cddaar", [](let const& xs) -> auto const& { return cddaar(xs[0]); });
      library.define<procedure>("cddadr", [](let const& xs) -> auto const& { return cddadr(xs[0]); });
      library.define<procedure>("cdddar", [](let const& xs) -> auto const& { return cdddar(xs[0]); });
      library.define<procedure>("cddddr", [](let const& xs) -> auto const& { return cddddr(xs[0]); });

      library.define<procedure>("set-car!", [](let & xs)
      {
        caar(xs) = cadr(xs);
      });

      library.define<procedure>("set-cdr!", [](let & xs)
      {
        cdar(xs) = cadr(xs);
      });
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

    define<library>("(meevax procedure)", [](library & library)
    {
      library.define<procedure>("closure?", [](let const& xs)
      {
        return xs[0].is<closure>();
      });

      library.define<procedure>("continuation?", [](let const& xs)
      {
        return xs[0].is<continuation>();
      });

      library.define<procedure>("procedure?", [](let const& xs)
      {
        return xs[0].is<closure>() or xs[0].is<continuation>() or xs[0].is_also<callable>();
      });

      library.define<procedure>("procedure", [](let const& xs)
      {
        return make<procedure>(xs[1].as<symbol>(), dlsym(xs[1].as<symbol>(), dlopen(xs[0].as<string>())));
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
        switch (length(xs))
        {
        case 1:
          return make<string>(xs[0].as<exact_integer>(), character());

        case 2:
          return make<string>(xs[0].as<exact_integer>(), xs[1].as<character>());

        default:
          throw error(make<string>("procedure make-string takes one or two arugments, but got"), xs);
        }
      });

      library.define<procedure>("string", [](let const& xs)
      {
        let s = make<string>();

        for (let const& x : xs)
        {
          s.as<string>().vector.push_back(x.as<character>());
        }

        return s;
      });

      library.define<procedure>("string-length", [](let const& xs)
      {
        return make<exact_integer>(xs[0].as<string>().vector.size());
      });

      library.define<procedure>("string-ref", [](let const& xs)
      {
        return make(xs[0].as<string>().vector.at(xs[1].as<exact_integer>()));
      });

      library.define<procedure>("string-set!", [](let & xs)
      {
        xs[0].as<string>().vector.at(xs[1].as<exact_integer>()) = xs[2].as<character>();
      });

      library.define<procedure>("string=?", [](let const& xs)
      {
        auto compare = [](let const& a, let const& b)
        {
          return not (a.as<string>().vector == b.as<string>().vector);
        };

        return std::adjacent_find(xs.begin(), xs.end(), compare) == xs.end();
      });

      library.define<procedure>("string<?", [](let const& xs)
      {
        auto compare = [](let const& a, let const& b)
        {
          return not (a.as<string>().vector < b.as<string>().vector);
        };

        return std::adjacent_find(xs.begin(), xs.end(), compare) == xs.end();
      });

      library.define<procedure>("string>?", [](let const& xs)
      {
        auto compare = [](let const& a, let const& b)
        {
          return not (a.as<string>().vector > b.as<string>().vector);
        };

        return std::adjacent_find(xs.begin(), xs.end(), compare) == xs.end();
      });

      library.define<procedure>("string<=?", [](let const& xs)
      {
        auto compare = [](let const& a, let const& b)
        {
          return not (a.as<string>().vector <= b.as<string>().vector);
        };

        return std::adjacent_find(xs.begin(), xs.end(), compare) == xs.end();
      });

      library.define<procedure>("string>=?", [](let const& xs)
      {
        auto compare = [](let const& a, let const& b)
        {
          return not (a.as<string>().vector >= b.as<string>().vector);
        };

        return std::adjacent_find(xs.begin(), xs.end(), compare) == xs.end();
      });

      library.define<procedure>("string-ci=?", [](let const& xs)
      {
        auto compare = [](let const& s1, let const& s2)
        {
          auto compare = [](auto const& c1, auto const& c2)
          {
            return c1.downcase() == c2.downcase();
          };

          return not std::lexicographical_compare(s1.as<string>().vector.begin(), s1.as<string>().vector.end(),
                                                  s2.as<string>().vector.begin(), s2.as<string>().vector.end(), compare);
        };

        return std::adjacent_find(xs.begin(), xs.end(), compare) == xs.end();
      });

      library.define<procedure>("string-ci<?", [](let const& xs)
      {
        auto compare = [](let const& s1, let const& s2)
        {
          auto compare = [](auto const& c1, auto const& c2)
          {
            return c1.downcase() < c2.downcase();
          };

          return not std::lexicographical_compare(s1.as<string>().vector.begin(), s1.as<string>().vector.end(),
                                                  s2.as<string>().vector.begin(), s2.as<string>().vector.end(), compare);
        };

        return std::adjacent_find(xs.begin(), xs.end(), compare) == xs.end();
      });

      library.define<procedure>("string-ci>?", [](let const& xs)
      {
        auto compare = [](let const& s1, let const& s2)
        {
          auto compare = [](auto const& c1, auto const& c2)
          {
            return c1.downcase() > c2.downcase();
          };

          return not std::lexicographical_compare(s1.as<string>().vector.begin(), s1.as<string>().vector.end(),
                                                  s2.as<string>().vector.begin(), s2.as<string>().vector.end(), compare);
        };

        return std::adjacent_find(xs.begin(), xs.end(), compare) == xs.end();
      });

      library.define<procedure>("string-ci<=?", [](let const& xs)
      {
        auto compare = [](let const& s1, let const& s2)
        {
          auto compare = [](auto const& c1, auto const& c2)
          {
            return c1.downcase() <= c2.downcase();
          };

          return not std::lexicographical_compare(s1.as<string>().vector.begin(), s1.as<string>().vector.end(),
                                                  s2.as<string>().vector.begin(), s2.as<string>().vector.end(), compare);
        };

        return std::adjacent_find(xs.begin(), xs.end(), compare) == xs.end();
      });

      library.define<procedure>("string-ci>=?", [](let const& xs)
      {
        auto compare = [](let const& s1, let const& s2)
        {
          auto compare = [](auto const& c1, auto const& c2)
          {
            return c1.downcase() >= c2.downcase();
          };

          return not std::lexicographical_compare(s1.as<string>().vector.begin(), s1.as<string>().vector.end(),
                                                  s2.as<string>().vector.begin(), s2.as<string>().vector.end(), compare);
        };

        return std::adjacent_find(xs.begin(), xs.end(), compare) == xs.end();
      });

      library.define<procedure>("string-append", [](let const& xs)
      {
        let s = make<string>();

        for (let const& x : xs)
        {
          s.as<string>().vector.insert(s.as<string>().vector.end(),
                                       x.as<string>().vector.begin(),
                                       x.as<string>().vector.end());
        }

        return s;
      });

      library.define<procedure>("string->list", [](let const& xs)
      {
        auto push = [](let const& xs, character const& c)
        {
          return cons(make(c), xs);
        };

        switch (length(xs))
        {
        case 1:
          return std::accumulate(xs[0].as<string>().vector.rbegin(),
                                 xs[0].as<string>().vector.rend(),
                                 unit,
                                 push);

        case 2:
          return std::accumulate(xs[0].as<string>().vector.rbegin(),
                                 std::prev(xs[0].as<string>().vector.rend(),
                                           xs[1].as<exact_integer>()),
                                 unit,
                                 push);

        case 3:
          return std::accumulate(std::prev(xs[0].as<string>().vector.rend(),
                                           xs[2].as<exact_integer>()),
                                 std::prev(xs[0].as<string>().vector.rend(),
                                           xs[1].as<exact_integer>()),
                                 unit,
                                 push);

        default:
          throw error(make<string>("procedure string->list takes one to three arugments, but got"), xs);
        }
      });

      library.define<procedure>("list->string", [](let const& xs)
      {
        let s = make<string>();

        for (let const& x : xs[0])
        {
          s.as<string>().vector.push_back(x.as<character>());
        }

        return s;
      });

      library.define<procedure>("string-copy", [](let const& xs)
      {
        switch (length(xs))
        {
        case 1:
          return make<string>(xs[0].as<string>().vector.begin(),
                              xs[0].as<string>().vector.end());

        case 2:
          return make<string>(std::next(xs[0].as<string>().vector.begin(),
                                        xs[1].as<exact_integer>()),
                              xs[0].as<string>().vector.end());

        case 3:
          return make<string>(std::next(xs[0].as<string>().vector.begin(),
                                        xs[1].as<exact_integer>()),
                              std::next(xs[0].as<string>().vector.begin(),
                                        xs[2].as<exact_integer>()));

        default:
          throw error(make<string>("procedure string-copy takes one to three arugments, but got"), xs);
        }
      });

      library.define<procedure>("string-copy!", [](let const& xs)
      {
        xs[0].as<string>().vector.reserve(xs[0].as<string>().vector.size() +
                                          xs[2].as<string>().vector.size());

        switch (length(xs))
        {
        case 3:
          std::copy(xs[2].as<string>().vector.begin(),
                    xs[2].as<string>().vector.end(),
                    std::next(xs[0].as<string>().vector.begin(),
                              xs[1].as<exact_integer>()));
          break;

        case 4:
          std::copy(std::next(xs[2].as<string>().vector.begin(),
                              xs[3].as<exact_integer>()),
                    xs[2].as<string>().vector.end(),
                    std::next(xs[0].as<string>().vector.begin(),
                              xs[1].as<exact_integer>()));
          break;

        case 5:
          std::copy(std::next(xs[2].as<string>().vector.begin(),
                              xs[3].as<exact_integer>()),
                    std::next(xs[2].as<string>().vector.begin(),
                              xs[4].as<exact_integer>()),
                    std::next(xs[0].as<string>().vector.begin(),
                              xs[1].as<exact_integer>()));
          break;

        default:
          throw error(make<string>("procedure string-copy takes three to five arugments, but got"), xs);
        }
      });

      library.define<procedure>("string-fill!", [](let & xs)
      {
        switch (length(xs))
        {
        case 2:
          std::fill(xs[0].as<string>().vector.begin(),
                    xs[0].as<string>().vector.end(),
                    xs[1].as<character>());
          break;

        case 3:
          std::fill(std::next(xs[0].as<string>().vector.begin(),
                              xs[2].as<exact_integer>()),
                    xs[0].as<string>().vector.end(),
                    xs[1].as<character>());
          break;

        case 4:
          std::fill(std::next(xs[0].as<string>().vector.begin(),
                              xs[2].as<exact_integer>()),
                    std::next(xs[0].as<string>().vector.begin(),
                              xs[3].as<exact_integer>()),
                    xs[1].as<character>());
          break;

        default:
          throw error(make<string>("procedure string-fill! takes one to three arugments, but got"), xs);
        }
      });
    });

    define<library>("(meevax symbol)", [](library & library)
    {
      library.define<procedure>("symbol?", [](let const& xs)
      {
        return xs[0].is<symbol>();
      });

      library.define<procedure>("symbol->string", [](let const& xs)
      {
        return make<string>(xs[0].as<symbol>());
      });

      library.define<procedure>("string->symbol", [](let const& xs)
      {
        return make_symbol(xs[0].as<string>());
      });

      library.define<procedure>("identifier->symbol", [](let const& xs)
      {
        if (let const& x = xs[0]; x.is<environment::syntactic_closure>())
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

      library.define<procedure>("get-environment-variables", []()
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
      library.define<procedure>("current-jiffy", []()
      {
        return make<exact_integer>(std::chrono::high_resolution_clock::now().time_since_epoch().count());
      });

      library.define<procedure>("jiffies-per-second", []()
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
        return make<vector>(xs);
      });

      library.define<procedure>("make-vector", [](let const& xs)
      {
        switch (length(xs))
        {
        case 1:
          return make<vector>(xs[0].as<exact_integer>(), unspecified);

        case 2:
          return make<vector>(xs[0].as<exact_integer>(), xs[1]);

        default:
          throw error(make<string>("procedure make-vector takes one or two arugments, but got"), xs);
        }
      });

      library.define<procedure>("vector-length", [](let const& xs)
      {
        return make<exact_integer>(xs[0].as<vector>().vector.size());
      });

      library.define<procedure>("vector-ref", [](let const& xs) -> auto const&
      {
        return xs[0][xs[1].as<exact_integer>()];
      });

      library.define<procedure>("vector-set!", [](let & xs)
      {
        xs[0].as<vector>().vector[xs[1].as<exact_integer>()] = xs[2];
      });

      library.define<procedure>("vector->list", [](let const& xs)
      {
        switch (length(xs))
        {
        case 1:
          return std::accumulate(xs[0].as<vector>().vector.rbegin(),
                                 xs[0].as<vector>().vector.rend(),
                                 unit,
                                 xcons);

        case 2:
          return std::accumulate(xs[0].as<vector>().vector.rbegin(),
                                 std::prev(xs[0].as<vector>().vector.rend(),
                                           xs[1].as<exact_integer>()),
                                 unit,
                                 xcons);

        case 3:
          return std::accumulate(std::prev(xs[0].as<vector>().vector.rend(),
                                           xs[2].as<exact_integer>()),
                                 std::prev(xs[0].as<vector>().vector.rend(),
                                           xs[1].as<exact_integer>()),
                                 unit,
                                 xcons);

        default:
          throw error(make<string>("procedure vector->list takes one to three arugments, but got"), xs);
        }
      });

      library.define<procedure>("list->vector", [](let const& xs)
      {
        return make<vector>(xs[0]);
      });

      library.define<procedure>("vector->string", [](let const& xs)
      {
        let s = make<string>();

        auto push_back = [&](let const& x)
        {
          s.as<string>().vector.push_back(x.as<character>());
        };

        switch (length(xs))
        {
        case 1:
          std::for_each(xs[0].as<vector>().vector.begin(),
                        xs[0].as<vector>().vector.end(),
                        push_back);
          return s;

        case 2:
          std::for_each(std::next(xs[0].as<vector>().vector.begin(),
                                  xs[1].as<exact_integer>()),
                        xs[0].as<vector>().vector.end(),
                        push_back);
          return s;

        case 3:
          std::for_each(std::next(xs[0].as<vector>().vector.begin(),
                                  xs[1].as<exact_integer>()),
                        std::next(xs[0].as<vector>().vector.begin(),
                                  xs[2].as<exact_integer>()),
                        push_back);
          return s;

        default:
          throw error(make<string>("procedure vector->list takes one to three arugments, but got"), xs);
        }
      });

      library.define<procedure>("string->vector", [](let const& xs)
      {
        let v = make<vector>();

        for (auto character : xs[0].as<string>().vector)
        {
          v.as<vector>().vector.push_back(make(character));
        }

        return v;
      });

      library.define<procedure>("vector-copy", [](let const& xs)
      {
        switch (length(xs))
        {
        case 1:
          return make<vector>(xs[0].as<vector>().vector.begin(),
                              xs[0].as<vector>().vector.end());

        case 2:
          return make<vector>(std::next(xs[0].as<vector>().vector.begin(),
                                        xs[1].as<exact_integer>()),
                              xs[0].as<vector>().vector.end());

        case 3:
          return make<vector>(std::next(xs[0].as<vector>().vector.begin(),
                                        xs[1].as<exact_integer>()),
                              std::next(xs[0].as<vector>().vector.begin(),
                                        xs[2].as<exact_integer>()));

        default:
          throw error(make<string>("procedure vector-copy takes one to three arugments, but got"), xs);
        }
      });

      library.define<procedure>("vector-copy!", [](let const& xs)
      {
        xs[0].as<vector>().vector.reserve(xs[0].as<vector>().vector.size() +
                                          xs[2].as<vector>().vector.size());

        switch (length(xs))
        {
        case 3:
          std::copy(xs[2].as<vector>().vector.begin(),
                    xs[2].as<vector>().vector.end(),
                    std::next(xs[0].as<vector>().vector.begin(),
                              xs[1].as<exact_integer>()));
          break;

        case 4:
          std::copy(std::next(xs[2].as<vector>().vector.begin(),
                              xs[3].as<exact_integer>()),
                    xs[2].as<vector>().vector.end(),
                    std::next(xs[0].as<vector>().vector.begin(),
                              xs[1].as<exact_integer>()));
          break;

        case 5:
          std::copy(std::next(xs[2].as<vector>().vector.begin(),
                              xs[3].as<exact_integer>()),
                    std::next(xs[2].as<vector>().vector.begin(),
                              xs[4].as<exact_integer>()),
                    std::next(xs[0].as<vector>().vector.begin(),
                              xs[1].as<exact_integer>()));
          break;

        default:
          throw error(make<string>("procedure vector-copy takes three to five arugments, but got"), xs);
        }
      });

      library.define<procedure>("vector-append", [](let const& xs)
      {
        let v = make<vector>();

        for (let const& x : xs)
        {
          v.as<vector>().vector.insert(v.as<vector>().vector.end(),
                                       x.as<vector>().vector.begin(),
                                       x.as<vector>().vector.end());
        }

        return v;
      });

      library.define<procedure>("vector-fill!", [](let & xs)
      {
        switch (length(xs))
        {
        case 2:
          std::fill(xs[0].as<vector>().vector.begin(),
                    xs[0].as<vector>().vector.end(),
                    xs[1]);
          break;

        case 3:
          std::fill(std::next(xs[0].as<vector>().vector.begin(),
                              xs[2].as<exact_integer>()),
                    xs[0].as<vector>().vector.end(),
                    xs[1]);
          break;

        case 4:
          std::fill(std::next(xs[0].as<vector>().vector.begin(),
                              xs[2].as<exact_integer>()),
                    std::next(xs[0].as<vector>().vector.begin(),
                              xs[3].as<exact_integer>()),
                    xs[1]);
          break;
        }
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
        return make<TAG##vector>(xs[0].as<exact_integer>(), 1 < length(xs) ? xs[1] : unspecified); \
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
                                 1 < length(xs) ? xs[1].as<exact_integer>() : std::size_t(), \
                                 2 < length(xs) ? xs[2].as<exact_integer>() : xs[0].as<TAG##vector>().valarray.size()); \
      });                                                                      \
                                                                               \
      library.define<procedure>(#TAG "vector-copy!", [](let & xs)              \
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
                    1 < length(xs) ? xs[1].as<exact_integer>() : 0,            \
                    2 < length(xs) ? xs[2].as<exact_integer>() : xs[0].as<TAG##vector>().valarray.size()); \
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

        std::for_each(std::next(std::begin(xs[0].as<u8vector>().valarray), 1 < length(xs) ? xs[1].as<exact_integer>() : 0),
                      std::next(std::begin(xs[0].as<u8vector>().valarray), 2 < length(xs) ? xs[2].as<exact_integer>() : xs[0].as<u8vector>().valarray.size()),
                      [&](auto const& x)
                      {
                        buffer << x;
                      });

        return input_string_port(buffer.str()).get(std::numeric_limits<std::size_t>::max());
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
  }
} // namespace kernel
} // namespace meevax
