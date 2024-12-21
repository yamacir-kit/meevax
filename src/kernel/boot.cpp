/*
   Copyright 2018-2024 Tatsuya Yamasaki.

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
#include <numbers>
#include <numeric>

#include <meevax/kernel/binary_input_file_port.hpp>
#include <meevax/kernel/binary_output_file_port.hpp>
#include <meevax/kernel/boot.hpp>
#include <meevax/kernel/box.hpp>
#include <meevax/kernel/closure.hpp>
#include <meevax/kernel/continuation.hpp>
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

extern char ** environ; // for procedure get-environment-variables

namespace meevax::inline kernel
{
  auto boot() -> void
  {
    #define EXPORT1(IDENTIFIER)                                                \
    library.define<procedure>(#IDENTIFIER, [](let const& xs)                   \
    {                                                                          \
      return IDENTIFIER(car(xs));                                              \
    })

    #define EXPORT2(IDENTIFIER)                                                \
    library.define<procedure>(#IDENTIFIER, [](let const& xs)                   \
    {                                                                          \
      return IDENTIFIER(car(xs), cadr(xs));                                    \
    })

    #define EXPORT1_RENAME(IDENTIFIER1, IDENTIFIER2)                           \
    library.define<procedure>(IDENTIFIER2, [](let const& xs)                   \
    {                                                                          \
      return IDENTIFIER1(car(xs));                                             \
    })

    #define EXPORT2_RENAME(IDENTIFIER1, IDENTIFIER2)                           \
    library.define<procedure>(IDENTIFIER2, [](let const& xs)                   \
    {                                                                          \
      return IDENTIFIER1(car(xs), cadr(xs));                                   \
    })

    define<library>("(meevax boolean)", [](library & library)
    {
      library.define<procedure>("boolean?", [](let const& xs)
      {
        return car(xs).is<bool>();
      });

      library.define<procedure>("not", [](let const& xs)
      {
        return car(xs) == f;
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

      library.define<procedure>("box-ref", [](let const& xs)
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
        return car(xs).is<character>();
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
        return car(xs).as<character>().property().is_letter();
      });

      library.define<procedure>("char-numeric?", [](let const& xs)
      {
        return car(xs).as<character>().property().is_numeric();
      });

      library.define<procedure>("char-whitespace?", [](let const& xs)
      {
        return car(xs).as<character>().property().is_whitespace();
      });

      library.define<procedure>("char-upper-case?", [](let const& xs)
      {
        return car(xs).as<character>().property().is_upper_case();
      });

      library.define<procedure>("char-lower-case?", [](let const& xs)
      {
        return car(xs).as<character>().property().is_lower_case();
      });

      library.define<procedure>("digit-value", [](let const& xs) -> object
      {
        if (auto digit_value = car(xs).as<character>().digit_value(); digit_value)
        {
          return make<exact_integer>(*digit_value);
        }
        else
        {
          return f;
        }
      });

      library.define<procedure>("char->integer", [](let const& xs)
      {
        return make<exact_integer>(car(xs).as<character>().codepoint);
      });

      library.define<procedure>("integer->char", [](let const& xs)
      {
        return make<character>(car(xs).as<exact_integer>());
      });

      library.define<procedure>("char-upcase", [](let const& xs)
      {
        return make<character>(car(xs).as<character>().upcase());
      });

      library.define<procedure>("char-downcase", [](let const& xs)
      {
        return make<character>(car(xs).as<character>().downcase());
      });
    });

    define<library>("(meevax complex)", [](library & library)
    {
      library.define<procedure>("make-rectangular", [](let const& xs)
      {
        assert(is_real(car(xs)));
        assert(is_real(cadr(xs)));

        return make<complex>(car(xs), cadr(xs));
      });

      library.define<procedure>("make-polar", [](let const& xs)
      {
        let const& radius = car(xs), angle = cadr(xs);

        return make<complex>(radius * cos(angle),
                             radius * sin(angle));
      });

      EXPORT1(angle);
      EXPORT1(magnitude);
      EXPORT1_RENAME(imag_part, "imag-part");
      EXPORT1_RENAME(real_part, "real-part");
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
          throw status != f ? EXIT_SUCCESS : EXIT_FAILURE;
        }
        else
        {
          throw static_cast<int>(status.as<exact_integer>());
        }
      });

      library.define<procedure>("command-line", []()
      {
        let xs = list();

        for (auto&& each : interaction_environment().as<environment>().command_line)
        {
          xs = cons(make<string>(each), xs);
        }

        return reverse(xs);
      });
    });

    define<library>("(meevax comparator)", [](library & library)
    {
      EXPORT2_RENAME(eq, "eq?");
      EXPORT2_RENAME(equal, "equal?");
      EXPORT2_RENAME(eqv, "eqv?");
    });

    define<library>("(meevax core)", [](library & library)
    {
      library.second = cdr(environment::core()); // DIRTY HACK!
      library.export_specs = map(car, library.second);
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
        return cadr(xs).as<environment>().evaluate(car(xs));
      });

      library.define<procedure>("expand", [](let const& xs)
      {
        return cadr(xs).as<environment>().expand(car(xs), unit, environment::default_rename);
      });

      library.define<procedure>("interaction-environment", []()
      {
        return interaction_environment();
      });

      library.define<procedure>("load", [](let const& xs)
      {
        return car(xs).as<environment>().load(static_cast<std::filesystem::path>(cadr(xs).as<string>()));
      });
    });

    define<library>("(meevax error)", [](library & library)
    {
      library.define<procedure>("throw", [](let const& xs)
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
        environment::exception_handler = car(xs);
      });
    });

    define<library>("(meevax file)", [](library & library)
    {
      library.define<procedure>("file-exists?", [](let const& xs)
      {
        return std::filesystem::exists(static_cast<std::string>(car(xs).as<string>()));
      });

      library.define<procedure>("delete-file", [](let const& xs)
      {
        try
        {
          if (not std::filesystem::remove(static_cast<std::string>(car(xs).as<string>())))
          {
            throw file_error(make<string>("failed to remove file"), car(xs));
          }
        }
        catch (std::filesystem::filesystem_error const& e)
        {
          throw file_error(make<string>(e.what()), car(xs));
        }
      });
    });

    define<library>("(meevax inexact)", [](library & library)
    {
      library.define<procedure>("log", [](let const& xs)
      {
        switch (length(xs))
        {
        case 1:
          return log(car(xs));

        case 2:
          return log(car(xs)) / log(cadr(xs));

        default:
          throw error(make<string>("procedure log takes one or two arguments, but got"), xs);
        }
      });

      library.define<procedure>("atan", [](let const& xs)
      {
        switch (length(xs))
        {
        case 1:
          return atan(car(xs));

        case 2:
          return atan2(car(xs), cadr(xs));

        default:
          throw error(make<string>("procedure atan takes one or two arguments, but got"), xs);
        }
      });

      EXPORT1(acos);
      EXPORT1(acosh);
      EXPORT1(asin);
      EXPORT1(asinh);
      EXPORT1(atanh);
      EXPORT1(cos);
      EXPORT1(cosh);
      EXPORT1(erf);
      EXPORT1(erfc);
      EXPORT1(exp);
      EXPORT1(expm1);
      EXPORT1(fabs);
      EXPORT1(lgamma);
      EXPORT1(log1p);
      EXPORT1(sin);
      EXPORT1(sinh);
      EXPORT1(sqrt);
      EXPORT1(tan);
      EXPORT1(tanh);
      EXPORT1(tgamma);

      EXPORT1_RENAME(is_finite, "finite?");
      EXPORT1_RENAME(is_infinite, "infinite?");
      EXPORT1_RENAME(is_nan, "nan?");

      EXPORT2(copysign);
      EXPORT2(cyl_bessel_j);
      EXPORT2(cyl_neumann);
      EXPORT2(ldexp);
      EXPORT2(nextafter);

      library.define<double>("e", std::numbers::e);

      library.define<double>("pi", std::numbers::pi);

      library.define<double>("euler", std::numbers::egamma);

      library.define<double>("phi", std::numbers::phi);
    });

    define<library>("(meevax binary32)", [](library & library)
    {
      library.define<procedure>("binary32?", [](let const& xs)
      {
        return std::numeric_limits<float>::is_iec559 and car(xs).is<float>();
      });
    });

    define<library>("(meevax binary64)", [](library & library)
    {
      library.define<procedure>("binary64?", [](let const& xs)
      {
        return std::numeric_limits<double>::is_iec559 and car(xs).is<double>();
      });

      library.define<double>("binary64-least", std::numeric_limits<double>::min());

      library.define<double>("binary64-greatest", std::numeric_limits<double>::max());

      library.define<double>("binary64-epsilon", std::numeric_limits<double>::epsilon());

      library.define<procedure>("binary64-integral-part", [](let const& xs)
      {
        auto integral_part = 0.0;
        std::modf(car(xs).as<double>(), &integral_part);
        return make(integral_part);
      });

      library.define<procedure>("binary64-fractional-part", [](let const& xs)
      {
        auto integral_part = 0.0;
        return make(std::modf(car(xs).as<double>(), &integral_part));
      });

      library.define<procedure>("binary64-log-binary", [](let const& xs)
      {
        return make(std::logb(car(xs).as<double>()));
      });

      library.define<procedure>("binary64-integer-log-binary", [](let const& xs)
      {
        return make<exact_integer>(std::ilogb(car(xs).as<double>()));
      });

      library.define<procedure>("binary64-normalized-fraction", [](let const& xs)
      {
        auto exponent = 0;
        return make(std::frexp(car(xs).as<double>(), &exponent));
      });

      library.define<procedure>("binary64-exponent", [](let const& xs)
      {
        auto exponent = 0;
        std::frexp(car(xs).as<double>(), &exponent);
        return make<exact_integer>(exponent);
      });

      library.define<procedure>("binary64-sign-bit", [](let const& xs)
      {
        return make(std::signbit(car(xs).as<double>()));
      });

      library.define<procedure>("binary64-normalized?", [](let const& xs)
      {
        return std::fpclassify(car(xs).as<double>()) == FP_NORMAL;
      });

      library.define<procedure>("binary64-denormalized?", [](let const& xs)
      {
        return std::fpclassify(car(xs).as<double>()) == FP_SUBNORMAL;
      });

      library.define<procedure>("binary64-max", [](let const& xs)
      {
        auto max = -std::numeric_limits<double>::infinity();

        for (let const& x : xs)
        {
          max = std::fmax(max, x.as<double>());
        }

        return make(max);
      });

      library.define<procedure>("binary64-min", [](let const& xs)
      {
        auto min = std::numeric_limits<double>::infinity();

        for (let const& x : xs)
        {
          min = std::fmin(min, x.as<double>());
        }

        return make(min);
      });

      library.define<procedure>("binary64-fused-multiply-add", [](let const& xs)
      {
        return make(std::fma(car(xs).as<double>(), cadr(xs).as<double>(), caddr(xs).as<double>()));
      });

      library.define<procedure>("binary64-remquo", [](let const& xs)
      {
        auto quotient = 0;
        auto remainder = std::remquo(car(xs).as<double>(), cadr(xs).as<double>(), &quotient);
        return cons(make(remainder), make<exact_integer>(quotient));
      });
    });

    define<library>("(meevax list)", [](library & library)
    {
      library.define<procedure>("null?", [](let const& xs)
      {
        return car(xs).is<null>();
      });

      EXPORT1_RENAME(is_list, "list?");

      library.define<procedure>("list", [](let const& xs)
      {
        return xs;
      });

      library.define<procedure>("make-list", [](let const& xs)
      {
        switch (length(xs))
        {
        case 1:
          return make_list(car(xs).as<exact_integer>());

        case 2:
          return make_list(car(xs).as<exact_integer>(), cadr(xs));

        default:
          throw error(make<string>("procedure make-list takes one or two arugments, but got"), xs);
        }
      });

      library.define<procedure>("iota", [](let const& xs)
      {
        switch (length(xs))
        {
        case 1:
          return iota(car(xs).as<exact_integer>());

        case 2:
          return iota(car(xs).as<exact_integer>(), cadr(xs));

        case 3:
          return iota(car(xs).as<exact_integer>(), cadr(xs), caddr(xs));

        default:
          throw error(make<string>("procedure iota takes one to three arugments, but got"), xs);
        }
      });

      EXPORT1_RENAME(is_circular_list, "circular-list?");

      library.define<procedure>("circular-list", [](let & xs)
      {
        circulate(xs);
        return xs;
      });

      EXPORT1_RENAME(is_dotted_list, "dotted-list?");

      library.define<procedure>("null-list?", [](let const& xs)
      {
        if (is_list(car(xs)) or is_circular_list(car(xs)))
        {
          return car(xs).is<null>();
        }
        else
        {
          throw error(make<string>("procedure null-list? takes a proper-list or a circular-list, but got"), xs);
        }
      });

      EXPORT1(last);

      EXPORT1_RENAME(last_pair, "last-pair");

      library.define<procedure>("length", [](let const& xs)
      {
        return make<exact_integer>(length(car(xs)));
      });

      library.define<procedure>("length+", [](let const& xs) -> object
      {
        if (is_circular_list(car(xs)))
        {
          return f;
        }
        else
        {
          return make<exact_integer>(length(car(xs)));
        }
      });

      library.define<procedure>("append", [](let const& xs)
      {
        return std::accumulate(xs.begin(), xs.end(), unit, [](let const& x, let const& y) { return append(x, y); });
      });

      library.define<procedure>("append!", [](let & xs)
      {
        auto append = [](auto append, let & x, let & xs) -> auto &
        {
          if (xs.is<null>())
          {
            return x;
          }
          else if (x.is<null>())
          {
            return x = append(append, car(xs), cdr(xs));
          }
          else
          {
            return meevax::append(x, append(append, car(xs), cdr(xs)));
          }
        };

        return not xs.is<pair>() ? xs : append(append, car(xs), cdr(xs));
      });

      EXPORT2_RENAME(append_reverse, "append-reverse");

      library.define<procedure>("append-reverse!", [](let & xs)
      {
        return append_reverse(car(xs), cadr(xs));
      });

      EXPORT1(reverse);

      library.define<procedure>("reverse!", [](let & xs)
      {
        return reverse(car(xs));
      });

      library.define<procedure>("concatenate", [](let const& xs)
      {
        return std::accumulate(car(xs).begin(), car(xs).end(), unit, [](let const& x, let const& y) { return append(x, y); });
      });

      library.define<procedure>("concatenate!", [](let & xs)
      {
        auto concatenate = [](auto concatenate, let & x, let & xs) -> auto &
        {
          if (xs.is<null>())
          {
            return x;
          }
          else if (x.is<null>())
          {
            return x = concatenate(concatenate, car(xs), cdr(xs));
          }
          else
          {
            return meevax::append(x, concatenate(concatenate, car(xs), cdr(xs)));
          }
        };

        return not xs.is<pair>() ? xs : concatenate(concatenate, caar(xs), cdar(xs));
      });

      EXPORT1_RENAME(list_copy, "list-copy");

      library.define<procedure>("list-tail", [](let const& xs)
      {
        return tail(car(xs), cadr(xs).as<exact_integer>());
      });

      library.define<procedure>("list-ref", [](let const& xs)
      {
        return head(car(xs), cadr(xs).as<exact_integer>());
      });

      library.define<procedure>("first",   [](let const& xs) { return head(car(xs), 0); });
      library.define<procedure>("second",  [](let const& xs) { return head(car(xs), 1); });
      library.define<procedure>("third",   [](let const& xs) { return head(car(xs), 2); });
      library.define<procedure>("fourth",  [](let const& xs) { return head(car(xs), 3); });
      library.define<procedure>("fifth",   [](let const& xs) { return head(car(xs), 4); });
      library.define<procedure>("sixth",   [](let const& xs) { return head(car(xs), 5); });
      library.define<procedure>("seventh", [](let const& xs) { return head(car(xs), 6); });
      library.define<procedure>("eighth",  [](let const& xs) { return head(car(xs), 7); });
      library.define<procedure>("ninth",   [](let const& xs) { return head(car(xs), 8); });
      library.define<procedure>("tenth",   [](let const& xs) { return head(car(xs), 9); });

      library.define<procedure>("take", [](let const& xs)
      {
        return take(car(xs), cadr(xs).as<exact_integer>());
      });

      library.define<procedure>("take!", [](let & xs)
      {
        return take(car(xs), cadr(xs).as<exact_integer>());
      });

      library.define<procedure>("take-right", [](let const& xs)
      {
        return take_right(car(xs), cadr(xs).as<exact_integer>());
      });

      library.define<procedure>("drop", [](let const& xs)
      {
        return drop(car(xs), cadr(xs).as<exact_integer>());
      });

      library.define<procedure>("drop-right", [](let const& xs)
      {
        return drop_right(car(xs), cadr(xs).as<exact_integer>());
      });

      library.define<procedure>("drop-right!", [](let & xs)
      {
        return drop_right(car(xs), cadr(xs).as<exact_integer>());
      });

      EXPORT2(memq);
      EXPORT2(memv);
      EXPORT2(assq);
      EXPORT2(assv);

      library.define<procedure>("alist-cons", [](let const& xs)
      {
        return alist_cons(car(xs), cadr(xs), caddr(xs));
      });

      EXPORT1_RENAME(alist_copy, "alist-copy");
    });

    define<library>("(meevax number)", [](library & library)
    {
      EXPORT1_RENAME(is_complex, "number?");
      EXPORT1_RENAME(is_complex, "complex?");
      EXPORT1_RENAME(is_real, "real?");
      EXPORT1_RENAME(is_rational, "rational?");
      EXPORT1_RENAME(is_integer, "integer?");
      EXPORT1_RENAME(is_exact, "exact?");
      EXPORT1_RENAME(is_inexact, "inexact?");

      library.define<procedure>("exact-integer?", [](let const& xs)
      {
        return car(xs).is<exact_integer>();
      });

      library.define<procedure>("=", [](let const& xs)
      {
        return std::adjacent_find(xs.begin(), xs.end(), not_equals) == xs.end();
      });

      library.define<procedure>("<", [](let const& xs)
      {
        return std::adjacent_find(xs.begin(), xs.end(), greater_than_or_equals) == xs.end();
      });

      library.define<procedure>("<=", [](let const& xs)
      {
        return std::adjacent_find(xs.begin(), xs.end(), greater_than) == xs.end();
      });

      library.define<procedure>(">", [](let const& xs)
      {
        return std::adjacent_find(xs.begin(), xs.end(), less_than_or_equals) == xs.end();
      });

      library.define<procedure>(">=", [](let const& xs)
      {
        return std::adjacent_find(xs.begin(), xs.end(), less_than) == xs.end();
      });

      EXPORT1_RENAME(is_zero, "zero?");
      EXPORT1_RENAME(is_positive, "positive?");
      EXPORT1_RENAME(is_negative, "negative?");
      EXPORT1_RENAME(is_odd, "odd?");
      EXPORT1_RENAME(is_even, "even?");

      library.define<procedure>("max", [](let const& xs)
      {
        if (auto iter = std::max_element(xs.begin(), xs.end(), less_than); iter != xs.end())
        {
          return std::any_of(xs.begin(), xs.end(), is_inexact) ? inexact(*iter) : *iter;
        }
        else
        {
          return unspecified;
        }
      });

      library.define<procedure>("min", [](let const& xs)
      {
        if (auto iter = std::min_element(xs.begin(), xs.end(), less_than); iter != xs.end())
        {
          return std::any_of(xs.begin(), xs.end(), is_inexact) ? inexact(*iter) : *iter;
        }
        else
        {
          return unspecified;
        }
      });

      library.define<procedure>("+", [](let const& xs)
      {
        return std::accumulate(xs.begin(), xs.end(), e0, std::plus());
      });

      library.define<procedure>("*", [](let const& xs)
      {
        return std::accumulate(xs.begin(), xs.end(), e1, std::multiplies());
      });

      library.define<procedure>("-", [](let const& xs)
      {
        if (cdr(xs).is<pair>())
        {
          return std::accumulate(std::next(xs.begin()), xs.end(), car(xs), std::minus());
        }
        else
        {
          return e0 - car(xs);
        }
      });

      library.define<procedure>("/", [](let const& xs)
      {
        if (cdr(xs).is<pair>())
        {
          return std::accumulate(std::next(xs.begin()), xs.end(), car(xs), std::divides());
        }
        else
        {
          return e1 / car(xs);
        }
      });

      EXPORT1(abs);
      EXPORT2(quotient);
      EXPORT2(remainder);
      EXPORT2(modulo);

      library.define<procedure>("gcd", [](let const& xs)
      {
        switch (length(xs))
        {
        case 0:
          return e0;

        case 1:
          return car(xs);

        default:
          return std::accumulate(cdr(xs).begin(), xs.end(), car(xs), gcd);
        }
      });

      library.define<procedure>("lcm", [](let const& xs)
      {
        switch (length(xs))
        {
        case 0:
          return e1;

        case 1:
          return car(xs);

        default:
          return std::accumulate(cdr(xs).begin(), xs.end(), car(xs), lcm);
        }
      });

      EXPORT1(numerator);
      EXPORT1(denominator);
      EXPORT1(floor);
      EXPORT1(ceiling);
      EXPORT1(truncate);
      EXPORT1(round);

      library.define<procedure>("exact-integer-square-root", [](let const& xs)
      {
        auto&& [s, r] = car(xs).as<exact_integer>().sqrt();

        return cons(make(std::forward<decltype(s)>(s)),
                    make(std::forward<decltype(r)>(r)));
      });

      EXPORT2_RENAME(pow, "expt");
      EXPORT1(exact);
      EXPORT1(inexact);

      library.define<procedure>("number->string", [](let const& xs)
      {
        switch (length(xs))
        {
        case 1:
          return number_to_string(car(xs), 10);

        case 2:
          return number_to_string(car(xs), cadr(xs).as<exact_integer>());

        default:
          throw error(make<string>("procedure number->string takes one or two arugments, but got"), xs);
        }
      });

      library.define<procedure>("string->number", [](let const& xs)
      {
        switch (length(xs))
        {
        case 1:
          return make_number(car(xs).as<string>(), 10);

        case 2:
          return make_number(car(xs).as<string>(), cadr(xs).as<exact_integer>());

        default:
          throw error(make<string>("procedure string->number takes one or two arugments, but got"), xs);
        }
      });
    });

    define<library>("(meevax pair)", [](library & library)
    {
      library.define<procedure>("pair?", [](let const& xs)
      {
        return car(xs).is<pair>();
      });

      library.define<procedure>("not-pair?", [](let const& xs)
      {
        return not car(xs).is<pair>();
      });

      EXPORT2(cons);

      library.define<procedure>("cons*", [](let & xs)
      {
        if (xs.is<null>())
        {
          throw error(make<string>("procedure cons* takes at least one arugments, but got"), xs);
        }
        else if (cdr(xs).is<null>())
        {
          return car(xs);
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
        return cons(cadr(xs), car(xs));
      });

      EXPORT1(car);
      EXPORT1(cdr);
      EXPORT1(caar);
      EXPORT1(cadr);
      EXPORT1(cdar);
      EXPORT1(cddr);
      EXPORT1(caaar);
      EXPORT1(caadr);
      EXPORT1(cadar);
      EXPORT1(caddr);
      EXPORT1(cdaar);
      EXPORT1(cdadr);
      EXPORT1(cddar);
      EXPORT1(cdddr);
      EXPORT1(caaaar);
      EXPORT1(caaadr);
      EXPORT1(caadar);
      EXPORT1(caaddr);
      EXPORT1(cadaar);
      EXPORT1(cadadr);
      EXPORT1(caddar);
      EXPORT1(cadddr);
      EXPORT1(cdaaar);
      EXPORT1(cdaadr);
      EXPORT1(cdadar);
      EXPORT1(cdaddr);
      EXPORT1(cddaar);
      EXPORT1(cddadr);
      EXPORT1(cdddar);
      EXPORT1(cddddr);

      library.define<procedure>("set-car!", [](let & xs) { caar(xs) = cadr(xs); });
      library.define<procedure>("set-cdr!", [](let & xs) { cdar(xs) = cadr(xs); });
    });

    define<library>("(meevax port)", [](library & library)
    {
      library.define<procedure>("input-port?", [](let const& xs)
      {
        return car(xs).is_also<input_port>();
      });

      library.define<procedure>("output-port?", [](let const& xs)
      {
        return car(xs).is_also<output_port>();
      });

      library.define<procedure>("binary-port?", [](let const& xs)
      {
        return car(xs).is_also<binary_port>();
      });

      library.define<procedure>("textual-port?", [](let const& xs)
      {
        return car(xs).is_also<textual_port>();
      });

      library.define<procedure>("port?", [](let const& xs)
      {
        return car(xs).is_also<port>();
      });

      library.define<procedure>("open?", [](let const& xs)
      {
        return car(xs).as<port>().is_open();
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
        return make<input_file_port>(car(xs).as<string>());
      });

      library.define<procedure>("open-output-file", [](let const& xs)
      {
        return make<output_file_port>(car(xs).as<string>());
      });

      library.define<procedure>("open-binary-input-file", [](let const& xs)
      {
        return make<binary_input_file_port>(car(xs).as<string>());
      });

      library.define<procedure>("open-binary-output-file", [](let const& xs)
      {
        return make<binary_output_file_port>(car(xs).as<string>());
      });

      library.define<procedure>("close", [](let const& xs)
      {
        car(xs).as<port>().close();
      });

      library.define<procedure>("open-input-string", [](let const& xs)
      {
        return make<input_string_port>(car(xs).as<string>());
      });

      library.define<procedure>("open-output-string", [](let const&)
      {
        return make<output_string_port>();
      });

      library.define<procedure>("get-output-string", [](let const& xs)
      {
        return make<string>(car(xs).as<output_string_port>().ostringstream.str());
      });

      library.define<procedure>("open-input-u8vector", [](let const& xs)
      {
        return make<input_u8vector_port>(car(xs).as<u8vector>());
      });

      library.define<procedure>("open-output-u8vector", [](let const&)
      {
        return make<output_u8vector_port>();
      });

      library.define<procedure>("get-output-u8vector", [](let const& xs)
      {
        return make<u8vector>(car(xs).as<output_u8vector_port>().vector.data(),
                              car(xs).as<output_u8vector_port>().vector.size());
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
        car(xs).as<output_port>().flush();
      });
    });

    define<library>("(meevax procedure)", [](library & library)
    {
      library.define<procedure>("closure?", [](let const& xs)
      {
        return car(xs).is<closure>();
      });

      library.define<procedure>("continuation?", [](let const& xs)
      {
        return car(xs).is<continuation>();
      });

      library.define<procedure>("procedure?", [](let const& xs)
      {
        return car(xs).is<closure>() or car(xs).is<continuation>() or car(xs).is_also<primitive>();
      });

      library.define<procedure>("procedure", [](let const& xs)
      {
        return make<procedure>(cadr(xs).as<symbol>(),
                               reinterpret_cast<primitive::signature>(
                                 default_collector::dlsym(cadr(xs).as<symbol>(),
                                                          default_collector::dlopen(car(xs).as<string>()))));
      });
    });

    define<library>("(meevax read)", [](library & library)
    {
      library.define<procedure>("get-char", [](let const& xs)
      {
        return car(xs).as<textual_input_port>().get();
      });

      library.define<procedure>("get-char-ready?", [](let const& xs)
      {
        return car(xs).as<textual_input_port>().get_ready();
      });

      library.define<procedure>("get-line", [](let const& xs)
      {
        return car(xs).as<textual_input_port>().get_line();
      });

      library.define<procedure>("get-string", [](let const& xs)
      {
        return cadr(xs).as<textual_input_port>().get(car(xs).as<exact_integer>());
      });

      library.define<procedure>("peek-char", [](let const& xs)
      {
        return car(xs).as<textual_input_port>().peek();
      });

      library.define<procedure>("get-u8", [](let const& xs)
      {
        return car(xs).as<binary_input_port>().get();
      });

      library.define<procedure>("get-u8-ready?", [](let const& xs)
      {
        return car(xs).as<binary_input_port>().get_ready();
      });

      library.define<procedure>("peek-u8", [](let const& xs)
      {
        return car(xs).as<binary_input_port>().peek();
      });

      library.define<procedure>("get-u8vector", [](let const& xs)
      {
        return cadr(xs).as<binary_input_port>().get(car(xs).as<exact_integer>());
      });

      library.define<procedure>("read", [](let const& xs)
      {
        return car(xs).as<textual_input_port>().read();
      });
    });

    define<library>("(meevax string)", [](library & library)
    {
      library.define<procedure>("string?", [](let const& xs)
      {
        return car(xs).is<string>();
      });

      library.define<procedure>("make-string", [](let const& xs)
      {
        switch (length(xs))
        {
        case 1:
          return make<string>(car(xs).as<exact_integer>(), character());

        case 2:
          return make<string>(car(xs).as<exact_integer>(), cadr(xs).as<character>());

        default:
          throw error(make<string>("procedure make-string takes one or two arugments, but got"), xs);
        }
      });

      library.define<procedure>("string", [](let const& xs)
      {
        let s = make<string>();

        for (let const& x : xs)
        {
          s.as<string>().push_back(x.as<character>());
        }

        return s;
      });

      library.define<procedure>("string-length", [](let const& xs)
      {
        return make<exact_integer>(car(xs).as<string>().size());
      });

      library.define<procedure>("string-ref", [](let const& xs)
      {
        return make(car(xs).as<string>().at(cadr(xs).as<exact_integer>()));
      });

      library.define<procedure>("string-set!", [](let & xs)
      {
        car(xs).as<string>().at(cadr(xs).as<exact_integer>()) = caddr(xs).as<character>();
      });

      library.define<procedure>("string=?", [](let const& xs)
      {
        auto compare = [](let const& a, let const& b)
        {
          return not (a.as<string>() == b.as<string>());
        };

        return std::adjacent_find(xs.begin(), xs.end(), compare) == xs.end();
      });

      library.define<procedure>("string<?", [](let const& xs)
      {
        auto compare = [](let const& a, let const& b)
        {
          return not (a.as<string>() < b.as<string>());
        };

        return std::adjacent_find(xs.begin(), xs.end(), compare) == xs.end();
      });

      library.define<procedure>("string>?", [](let const& xs)
      {
        auto compare = [](let const& a, let const& b)
        {
          return not (a.as<string>() > b.as<string>());
        };

        return std::adjacent_find(xs.begin(), xs.end(), compare) == xs.end();
      });

      library.define<procedure>("string<=?", [](let const& xs)
      {
        auto compare = [](let const& a, let const& b)
        {
          return not (a.as<string>() <= b.as<string>());
        };

        return std::adjacent_find(xs.begin(), xs.end(), compare) == xs.end();
      });

      library.define<procedure>("string>=?", [](let const& xs)
      {
        auto compare = [](let const& a, let const& b)
        {
          return not (a.as<string>() >= b.as<string>());
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

          return not std::lexicographical_compare(s1.as<string>().begin(), s1.as<string>().end(),
                                                  s2.as<string>().begin(), s2.as<string>().end(), compare);
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

          return not std::lexicographical_compare(s1.as<string>().begin(), s1.as<string>().end(),
                                                  s2.as<string>().begin(), s2.as<string>().end(), compare);
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

          return not std::lexicographical_compare(s1.as<string>().begin(), s1.as<string>().end(),
                                                  s2.as<string>().begin(), s2.as<string>().end(), compare);
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

          return not std::lexicographical_compare(s1.as<string>().begin(), s1.as<string>().end(),
                                                  s2.as<string>().begin(), s2.as<string>().end(), compare);
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

          return not std::lexicographical_compare(s1.as<string>().begin(), s1.as<string>().end(),
                                                  s2.as<string>().begin(), s2.as<string>().end(), compare);
        };

        return std::adjacent_find(xs.begin(), xs.end(), compare) == xs.end();
      });

      library.define<procedure>("string-append", [](let const& xs)
      {
        let s = make<string>();

        for (let const& x : xs)
        {
          s.as<string>().insert(s.as<string>().end(),
                                x.as<string>().begin(),
                                x.as<string>().end());
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
          return std::accumulate(car(xs).as<string>().rbegin(),
                                 car(xs).as<string>().rend(),
                                 unit,
                                 push);

        case 2:
          return std::accumulate(car(xs).as<string>().rbegin(),
                                 std::prev(car(xs).as<string>().rend(),
                                           cadr(xs).as<exact_integer>()),
                                 unit,
                                 push);

        case 3:
          return std::accumulate(std::prev(car(xs).as<string>().rend(),
                                           caddr(xs).as<exact_integer>()),
                                 std::prev(car(xs).as<string>().rend(),
                                           cadr(xs).as<exact_integer>()),
                                 unit,
                                 push);

        default:
          throw error(make<string>("procedure string->list takes one to three arugments, but got"), xs);
        }
      });

      library.define<procedure>("list->string", [](let const& xs)
      {
        let s = make<string>();

        for (let const& x : car(xs))
        {
          s.as<string>().push_back(x.as<character>());
        }

        return s;
      });

      library.define<procedure>("string-copy", [](let const& xs)
      {
        switch (length(xs))
        {
        case 1:
          return make<string>(car(xs).as<string>().begin(),
                              car(xs).as<string>().end());

        case 2:
          return make<string>(std::next(car(xs).as<string>().begin(),
                                        cadr(xs).as<exact_integer>()),
                              car(xs).as<string>().end());

        case 3:
          return make<string>(std::next(car(xs).as<string>().begin(),
                                        cadr(xs).as<exact_integer>()),
                              std::next(car(xs).as<string>().begin(),
                                        caddr(xs).as<exact_integer>()));

        default:
          throw error(make<string>("procedure string-copy takes one to three arugments, but got"), xs);
        }
      });

      library.define<procedure>("string-copy!", [](let const& xs)
      {
        car(xs).as<string>().reserve(car(xs).as<string>().size() +
                                     caddr(xs).as<string>().size());

        switch (length(xs))
        {
        case 3:
          std::copy(caddr(xs).as<string>().begin(),
                    caddr(xs).as<string>().end(),
                    std::next(car(xs).as<string>().begin(),
                              cadr(xs).as<exact_integer>()));
          break;

        case 4:
          std::copy(std::next(caddr(xs).as<string>().begin(),
                              cadddr(xs).as<exact_integer>()),
                    caddr(xs).as<string>().end(),
                    std::next(car(xs).as<string>().begin(),
                              cadr(xs).as<exact_integer>()));
          break;

        case 5:
          std::copy(std::next(caddr(xs).as<string>().begin(),
                              cadddr(xs).as<exact_integer>()),
                    std::next(caddr(xs).as<string>().begin(),
                              caddddr(xs).as<exact_integer>()),
                    std::next(car(xs).as<string>().begin(),
                              cadr(xs).as<exact_integer>()));
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
          std::fill(car(xs).as<string>().begin(),
                    car(xs).as<string>().end(),
                    cadr(xs).as<character>());
          break;

        case 3:
          std::fill(std::next(car(xs).as<string>().begin(),
                              caddr(xs).as<exact_integer>()),
                    car(xs).as<string>().end(),
                    cadr(xs).as<character>());
          break;

        case 4:
          std::fill(std::next(car(xs).as<string>().begin(),
                              caddr(xs).as<exact_integer>()),
                    std::next(car(xs).as<string>().begin(),
                              cadddr(xs).as<exact_integer>()),
                    cadr(xs).as<character>());
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
        return car(xs).is<symbol>();
      });

      library.define<procedure>("symbol->string", [](let const& xs)
      {
        return make<string>(car(xs).as<symbol>());
      });

      library.define<procedure>("string->symbol", [](let const& xs)
      {
        return make_symbol(car(xs).as<string>());
      });

      library.define<procedure>("identifier->symbol", [](let const& xs)
      {
        if (let const& x = car(xs); x.is<environment::syntactic_closure>())
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
        return car(xs).is_also<identifier>();
      });

      library.define<procedure>("transformer?", [](let const& xs)
      {
        return car(xs).is<transformer>();
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

    define<library>("(meevax system)", [](library & library)
    {
      library.define<procedure>("get-environment-variable", [](let const& xs) -> object
      {
        if (auto s = std::getenv(static_cast<std::string>(car(xs).as<string>()).c_str()))
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
        let alist = nullptr;

        for (auto iter = environ; *iter; ++iter)
        {
          if (auto const position = std::string_view(*iter).find_first_of("="); position != std::string::npos)
          {
            alist = alist_cons(make<string>(std::string(*iter, position)),
                               make<string>(std::string(*iter + position + 1)),
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
        return car(xs).is<vector>();
      });

      library.define<procedure>("vector", [](let const& xs)
      {
        return make_vector(xs);
      });

      library.define<procedure>("make-vector", [](let const& xs)
      {
        switch (length(xs))
        {
        case 1:
          return make<vector>(car(xs).as<exact_integer>(), unspecified);

        case 2:
          return make<vector>(car(xs).as<exact_integer>(), cadr(xs));

        default:
          throw error(make<string>("procedure make-vector takes one or two arugments, but got"), xs);
        }
      });

      library.define<procedure>("vector-length", [](let const& xs)
      {
        return make<exact_integer>(car(xs).as<vector>().size());
      });

      library.define<procedure>("vector-ref", [](let const& xs)
      {
        return car(xs).as<vector>()[cadr(xs).as<exact_integer>()];
      });

      library.define<procedure>("vector-set!", [](let & xs)
      {
        car(xs).as<vector>()[cadr(xs).as<exact_integer>()] = caddr(xs);
      });

      library.define<procedure>("vector->list", [](let const& xs)
      {
        switch (length(xs))
        {
        case 1:
          return std::accumulate(car(xs).as<vector>().rbegin(),
                                 car(xs).as<vector>().rend(),
                                 unit,
                                 xcons);

        case 2:
          return std::accumulate(car(xs).as<vector>().rbegin(),
                                 std::prev(car(xs).as<vector>().rend(),
                                           cadr(xs).as<exact_integer>()),
                                 unit,
                                 xcons);

        case 3:
          return std::accumulate(std::prev(car(xs).as<vector>().rend(),
                                           caddr(xs).as<exact_integer>()),
                                 std::prev(car(xs).as<vector>().rend(),
                                           cadr(xs).as<exact_integer>()),
                                 unit,
                                 xcons);

        default:
          throw error(make<string>("procedure vector->list takes one to three arugments, but got"), xs);
        }
      });

      EXPORT1_RENAME(make_vector, "list->vector");

      library.define<procedure>("vector->string", [](let const& xs)
      {
        let s = make<string>();

        auto push_back = [&](let const& x)
        {
          s.as<string>().push_back(x.as<character>());
        };

        switch (length(xs))
        {
        case 1:
          std::for_each(car(xs).as<vector>().begin(),
                        car(xs).as<vector>().end(),
                        push_back);
          return s;

        case 2:
          std::for_each(std::next(car(xs).as<vector>().begin(),
                                  cadr(xs).as<exact_integer>()),
                        car(xs).as<vector>().end(),
                        push_back);
          return s;

        case 3:
          std::for_each(std::next(car(xs).as<vector>().begin(),
                                  cadr(xs).as<exact_integer>()),
                        std::next(car(xs).as<vector>().begin(),
                                  caddr(xs).as<exact_integer>()),
                        push_back);
          return s;

        default:
          throw error(make<string>("procedure vector->list takes one to three arugments, but got"), xs);
        }
      });

      library.define<procedure>("string->vector", [](let const& xs)
      {
        let v = make<vector>();

        for (auto character : car(xs).as<string>())
        {
          v.as<vector>().push_back(make(character));
        }

        return v;
      });

      library.define<procedure>("vector-copy", [](let const& xs)
      {
        switch (length(xs))
        {
        case 1:
          return make<vector>(car(xs).as<vector>().begin(),
                              car(xs).as<vector>().end());

        case 2:
          return make<vector>(std::next(car(xs).as<vector>().begin(),
                                        cadr(xs).as<exact_integer>()),
                              car(xs).as<vector>().end());

        case 3:
          return make<vector>(std::next(car(xs).as<vector>().begin(),
                                        cadr(xs).as<exact_integer>()),
                              std::next(car(xs).as<vector>().begin(),
                                        caddr(xs).as<exact_integer>()));

        default:
          throw error(make<string>("procedure vector-copy takes one to three arugments, but got"), xs);
        }
      });

      library.define<procedure>("vector-copy!", [](let const& xs)
      {
        car(xs).as<vector>().reserve(car(xs).as<vector>().size() +
                                     caddr(xs).as<vector>().size());

        switch (length(xs))
        {
        case 3:
          std::copy(caddr(xs).as<vector>().begin(),
                    caddr(xs).as<vector>().end(),
                    std::next(car(xs).as<vector>().begin(),
                              cadr(xs).as<exact_integer>()));
          break;

        case 4:
          std::copy(std::next(caddr(xs).as<vector>().begin(),
                              cadddr(xs).as<exact_integer>()),
                    caddr(xs).as<vector>().end(),
                    std::next(car(xs).as<vector>().begin(),
                              cadr(xs).as<exact_integer>()));
          break;

        case 5:
          std::copy(std::next(caddr(xs).as<vector>().begin(),
                              cadddr(xs).as<exact_integer>()),
                    std::next(caddr(xs).as<vector>().begin(),
                              caddddr(xs).as<exact_integer>()),
                    std::next(car(xs).as<vector>().begin(),
                              cadr(xs).as<exact_integer>()));
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
          v.as<vector>().insert(v.as<vector>().end(),
                                x.as<vector>().begin(),
                                x.as<vector>().end());
        }

        return v;
      });

      library.define<procedure>("vector-fill!", [](let & xs)
      {
        switch (length(xs))
        {
        case 2:
          std::fill(car(xs).as<vector>().begin(),
                    car(xs).as<vector>().end(),
                    cadr(xs));
          break;

        case 3:
          std::fill(std::next(car(xs).as<vector>().begin(),
                              caddr(xs).as<exact_integer>()),
                    car(xs).as<vector>().end(),
                    cadr(xs));
          break;

        case 4:
          std::fill(std::next(car(xs).as<vector>().begin(),
                              caddr(xs).as<exact_integer>()),
                    std::next(car(xs).as<vector>().begin(),
                              cadddr(xs).as<exact_integer>()),
                    cadr(xs));
          break;
        }
      });
    });

    define<library>("(meevax vector homogeneous)", [](library & library)
    {
      #define DEFINE_VECTOR_AUX(TAG, VECTOR)                                   \
      library.define<procedure>(#TAG "vector?", [](let const& xs)              \
      {                                                                        \
        return car(xs).is<VECTOR>();                                           \
      });                                                                      \
                                                                               \
      library.define<procedure>("make-" #TAG "vector", [](let const& xs)       \
      {                                                                        \
        switch (length(xs))                                                    \
        {                                                                      \
        case 1:                                                                \
          return make<VECTOR>(direct_initialization, static_cast<VECTOR::value_type>(0), car(xs).as<exact_integer>()); \
                                                                               \
        case 2:                                                                \
          return make<VECTOR>(direct_initialization, VECTOR::input_cast(cadr(xs)), car(xs).as<exact_integer>()); \
                                                                               \
        default:                                                               \
          throw error(make<string>("procedure make-" #TAG "vector takes one or two arguments, but got"), xs); \
        }                                                                      \
      });                                                                      \
                                                                               \
      library.define<procedure>(#TAG "vector", [](let const& xs)               \
      {                                                                        \
        return make<VECTOR>(from_list, xs);                                    \
      });                                                                      \
                                                                               \
      library.define<procedure>(#TAG "vector-length", [](let const& xs)        \
      {                                                                        \
        return make<exact_integer>(car(xs).as<VECTOR>().size());               \
      });                                                                      \
                                                                               \
      library.define<procedure>(#TAG "vector-ref", [](let const& xs)           \
      {                                                                        \
        return VECTOR::output_cast(car(xs).as<VECTOR>()[cadr(xs).as<exact_integer>()]); \
      });                                                                      \
                                                                               \
      library.define<procedure>(#TAG "vector-set!", [](let const& xs)          \
      {                                                                        \
        car(xs).as<VECTOR>()[cadr(xs).as<exact_integer>()] = VECTOR::input_cast(caddr(xs)); \
      });                                                                      \
                                                                               \
      library.define<procedure>(#TAG "vector-copy", [](let const& xs)          \
      {                                                                        \
        switch (length(xs))                                                    \
        {                                                                      \
        case 1:                                                                \
          {                                                                    \
            std::size_t begin = 0;                                             \
            std::size_t end = car(xs).as<VECTOR>().size();                     \
            assert(begin <= end);                                              \
            return make<VECTOR>(car(xs).as<VECTOR>()[std::slice(begin, end - begin, 1)]); \
          }                                                                    \
                                                                               \
        case 2:                                                                \
          {                                                                    \
            std::size_t begin = cadr(xs).as<exact_integer>();                  \
            std::size_t end = car(xs).as<VECTOR>().size();                     \
            assert(begin <= end);                                              \
            return make<VECTOR>(car(xs).as<VECTOR>()[std::slice(begin, end - begin, 1)]); \
          }                                                                    \
                                                                               \
        case 3:                                                                \
          {                                                                    \
            std::size_t begin = cadr(xs).as<exact_integer>();                  \
            std::size_t end = caddr(xs).as<exact_integer>();                   \
            assert(begin <= end);                                              \
            return make<VECTOR>(car(xs).as<VECTOR>()[std::slice(begin, end - begin, 1)]); \
          }                                                                    \
                                                                               \
        default:                                                               \
          throw error(make<string>("procedure " #TAG "vector-copy takes one to three arguments, but got"), xs); \
        }                                                                      \
      });                                                                      \
                                                                               \
      library.define<procedure>(#TAG "vector-copy!", [](let & xs)              \
      {                                                                        \
        switch (length(xs))                                                    \
        {                                                                      \
        case 3:                                                                \
          {                                                                    \
            std::size_t at = cadr(xs).as<exact_integer>();                     \
            std::size_t begin = 0;                                             \
            std::size_t end = caddr(xs).as<VECTOR>().size();                   \
            assert(begin <= end);                                              \
            car(xs).as<VECTOR>()[std::slice(at, end - begin, 1)] = caddr(xs).as<VECTOR>()[std::slice(begin, end - begin, 1)]; \
          }                                                                    \
          break;                                                               \
                                                                               \
        case 4:                                                                \
          {                                                                    \
            std::size_t at = cadr(xs).as<exact_integer>();                     \
            std::size_t begin = cadddr(xs).as<exact_integer>();                \
            std::size_t end = caddr(xs).as<VECTOR>().size();                   \
            assert(begin <= end);                                              \
            car(xs).as<VECTOR>()[std::slice(at, end - begin, 1)] = caddr(xs).as<VECTOR>()[std::slice(begin, end - begin, 1)]; \
          }                                                                    \
          break;                                                               \
                                                                               \
        case 5:                                                                \
          {                                                                    \
            std::size_t at = cadr(xs).as<exact_integer>();                     \
            std::size_t begin = cadddr(xs).as<exact_integer>();                \
            std::size_t end = caddddr(xs).as<exact_integer>();                 \
            assert(begin <= end);                                              \
            car(xs).as<VECTOR>()[std::slice(at, end - begin, 1)] = caddr(xs).as<VECTOR>()[std::slice(begin, end - begin, 1)]; \
          }                                                                    \
          break;                                                               \
                                                                               \
        default:                                                               \
          throw error(make<string>("procedure " #TAG "vector-copy! takes three to five arguments, but got"), xs); \
        }                                                                      \
      });                                                                      \
                                                                               \
      library.define<procedure>(#TAG "vector-append", [](let const& xs)        \
      {                                                                        \
        auto const& a = car(xs).as<VECTOR>();                                  \
        auto const& b = cadr(xs).as<VECTOR>();                                 \
        let const c = make<VECTOR>(direct_initialization, a.size() + b.size()); \
        c.as<VECTOR>()[std::slice(0, a.size(), 1)] = a.valarray();             \
        c.as<VECTOR>()[std::slice(a.size(), b.size(), 1)] = b.valarray();      \
        return c;                                                              \
      });                                                                      \
                                                                               \
      library.define<procedure>(#TAG "vector->list", [](let const& xs)         \
      {                                                                        \
        auto list = [](auto&& v, auto&& a, auto&& b)                           \
        {                                                                      \
          auto xcons = [](auto&& x, auto&& y)                                  \
          {                                                                    \
            return cons(VECTOR::output_cast(y), x);                            \
          };                                                                   \
                                                                               \
          return reverse(std::accumulate(std::next(std::begin(v), a),          \
                                         std::next(std::begin(v), b), unit, xcons)); \
        };                                                                     \
                                                                               \
        switch (length(xs))                                                    \
        {                                                                      \
        case 1:                                                                \
          return list(car(xs).as<VECTOR>().valarray(),                         \
                      0,                                                       \
                      car(xs).as<VECTOR>().size());                            \
                                                                               \
        case 2:                                                                \
          return list(car(xs).as<VECTOR>().valarray(),                         \
                      cadr(xs).as<exact_integer>(),                            \
                      car(xs).as<VECTOR>().size());                            \
                                                                               \
        case 3:                                                                \
          return list(car(xs).as<VECTOR>().valarray(),                         \
                      cadr(xs).as<exact_integer>(),                            \
                      caddr(xs).as<exact_integer>());                          \
                                                                               \
        default:                                                               \
          throw error(make<string>("procedure " #TAG "vector->list takes one to three arguments, but got"), xs); \
        }                                                                      \
      });                                                                      \
                                                                               \
      library.define<procedure>("list->" #TAG "vector", [](let const& xs)      \
      {                                                                        \
        return make<VECTOR>(from_list, car(xs));                               \
      })

      #define DEFINE_VECTOR(TAG) DEFINE_VECTOR_AUX(TAG, TAG##vector)

      DEFINE_VECTOR(s8); DEFINE_VECTOR(s16); DEFINE_VECTOR(s32); DEFINE_VECTOR(s64);
      DEFINE_VECTOR(u8); DEFINE_VECTOR(u16); DEFINE_VECTOR(u32); DEFINE_VECTOR(u64);
                                             DEFINE_VECTOR(f32); DEFINE_VECTOR(f64);

      #undef DEFINE_VECTOR
      #undef DEFINE_VECTOR_AUX

      library.define<procedure>("u8vector->string", [](let const& xs)
      {
        auto buffer = std::ostringstream();

        auto print = [&](auto const& x)
        {
          buffer << x;
        };

        switch (length(xs))
        {
        case 1:
          std::for_each(std::begin(car(xs).as<u8vector>().valarray()),
                        std::end(car(xs).as<u8vector>().valarray()),
                        print);
          break;

        case 2:
          std::for_each(std::next(std::begin(car(xs).as<u8vector>().valarray()),
                                  cadr(xs).as<exact_integer>()),
                        std::end(car(xs).as<u8vector>().valarray()),
                        print);
          break;

        case 3:
          std::for_each(std::next(std::begin(car(xs).as<u8vector>().valarray()),
                                  cadr(xs).as<exact_integer>()),
                        std::next(std::begin(car(xs).as<u8vector>().valarray()),
                                  caddr(xs).as<exact_integer>()),
                        print);
          break;

        default:
          throw error(make<string>("procedure u8vector->string takes one to three arguments, but got"), xs);
        }

        return input_string_port(buffer.str()).get(std::numeric_limits<std::size_t>::max());
      });

      library.define<procedure>("string->u8vector", [](let const& xs)
      {
        auto convert = [](std::string const& s)
        {
          return make<u8vector>(reinterpret_cast<std::uint8_t const*>(s.data()), s.size());
        };

        return convert(car(xs).as<string>());
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
        cadr(xs).as<textual_output_port>().put(car(xs).as<character>());
      });

      library.define<procedure>("put-string", [](let const& xs)
      {
        cadr(xs).as<textual_output_port>().put(car(xs).as<string>());
      });

      library.define<procedure>("put-u8", [](let const& xs)
      {
        cadr(xs).as<binary_output_port>().put(car(xs).as<exact_integer>());
      });

      library.define<procedure>("put-u8vector", [](let const& xs)
      {
        cadr(xs).as<binary_output_port>().put(car(xs).as<u8vector>());
      });

      library.define<procedure>("write", [](let const& xs)
      {
        cadr(xs).as<textual_output_port>().write(car(xs));
      });

      library.define<procedure>("write-simple", [](let const& xs)
      {
        cadr(xs).as<textual_output_port>().write_simple(car(xs));
      });
    });
  }
} // namespace meevax::kernel
