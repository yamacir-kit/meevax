/*
   Copyright 2018-2025 Tatsuya Yamasaki.

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
#include <numbers>
#include <numeric>

#include <meevax/kernel/binary_input_file_port.hpp>
#include <meevax/kernel/binary_output_file_port.hpp>
#include <meevax/kernel/boot.hpp>
#include <meevax/kernel/box.hpp>
#include <meevax/kernel/closure.hpp>
#include <meevax/kernel/configurator.hpp>
#include <meevax/kernel/continuation.hpp>
#include <meevax/kernel/input_file_port.hpp>
#include <meevax/kernel/input_homogeneous_vector_port.hpp>
#include <meevax/kernel/input_string_port.hpp>
#include <meevax/kernel/library.hpp>
#include <meevax/kernel/number/bitwise.hpp>
#include <meevax/kernel/number/error_and_gamma.hpp>
#include <meevax/kernel/number/exponential.hpp>
#include <meevax/kernel/number/floating_point_manipulation.hpp>
#include <meevax/kernel/number/hyperbolic.hpp>
#include <meevax/kernel/number/nearest_integer.hpp>
#include <meevax/kernel/number/power.hpp>
#include <meevax/kernel/number/special.hpp>
#include <meevax/kernel/number/trigonometric.hpp>
#include <meevax/kernel/output_file_port.hpp>
#include <meevax/kernel/output_homogeneous_vector_port.hpp>
#include <meevax/kernel/output_string_port.hpp>
#include <meevax/kernel/procedure.hpp>
#include <meevax/kernel/standard_error_port.hpp>
#include <meevax/kernel/standard_input_port.hpp>
#include <meevax/kernel/standard_output_port.hpp>
#include <meevax/kernel/system.hpp>
#include <meevax/kernel/transformer.hpp>
#include <meevax/kernel/vector.hpp>

extern char ** environ; // for procedure get-environment-variables

namespace meevax::inline kernel
{
  auto boot() -> void
  {
    #define EXPORT1_RENAME(FUNCTION, IDENTIFIER)                               \
    library.define(IDENTIFIER, make<procedure>(IDENTIFIER, [](let const& xs)   \
    {                                                                          \
      return FUNCTION(car(xs));                                                \
    }))

    #define EXPORT2_RENAME(FUNCTION, IDENTIFIER)                               \
    library.define(IDENTIFIER, make<procedure>(IDENTIFIER, [](let const& xs)   \
    {                                                                          \
      return FUNCTION(car(xs), cadr(xs));                                      \
    }))

    #define EXPORT3_RENAME(FUNCTION, IDENTIFIER)                               \
    library.define(IDENTIFIER, make<procedure>(IDENTIFIER, [](let const& xs)   \
    {                                                                          \
      return FUNCTION(car(xs), cadr(xs), caddr(xs));                           \
    }))

    #define EXPORT_PREDICATE(TYPENAME, IDENTIFIER)                             \
    library.define(IDENTIFIER, make<procedure>(IDENTIFIER, [](let const& xs)   \
    {                                                                          \
      return car(xs).is<TYPENAME>();                                           \
    }))

    libraries().emplace("(meevax binary32)", make<library>([](library & library)
    {
      EXPORT_PREDICATE(float, "binary32?");
    }));

    libraries().emplace("(meevax binary64)", make<library>([](library & library)
    {
      EXPORT_PREDICATE(double, "binary64?");

      library.define("binary64-least",    make<double>(std::numeric_limits<double>::min()));
      library.define("binary64-greatest", make<double>(std::numeric_limits<double>::max()));
      library.define("binary64-epsilon",  make<double>(std::numeric_limits<double>::epsilon()));

      library.define("binary64-integral-part", make<procedure>("binary64-integral-part", [](let const& xs)
      {
        auto integral_part = 0.0;
        std::modf(car(xs).as<double>(), &integral_part);
        return make(integral_part);
      }));

      library.define("binary64-fractional-part", make<procedure>("binary64-fractional-part", [](let const& xs)
      {
        auto integral_part = 0.0;
        return make(std::modf(car(xs).as<double>(), &integral_part));
      }));

      library.define("binary64-log-binary", make<procedure>("binary64-log-binary", [](let const& xs)
      {
        return make(std::logb(car(xs).as<double>()));
      }));

      library.define("binary64-integer-log-binary", make<procedure>("binary64-integer-log-binary", [](let const& xs)
      {
        return make<small_integer>(std::ilogb(car(xs).as<double>()));
      }));

      library.define("binary64-normalized-fraction", make<procedure>("binary64-normalized-fraction", [](let const& xs)
      {
        auto exponent = 0;
        return make(std::frexp(car(xs).as<double>(), &exponent));
      }));

      library.define("binary64-exponent", make<procedure>("binary64-exponent", [](let const& xs)
      {
        auto exponent = 0;
        std::frexp(car(xs).as<double>(), &exponent);
        return make<small_integer>(exponent);
      }));

      library.define("binary64-sign-bit", make<procedure>("binary64-sign-bit", [](let const& xs)
      {
        return make(std::signbit(car(xs).as<double>()));
      }));

      library.define("binary64-normalized?", make<procedure>("binary64-normalized?", [](let const& xs)
      {
        return std::fpclassify(car(xs).as<double>()) == FP_NORMAL;
      }));

      library.define("binary64-denormalized?", make<procedure>("binary64-denormalized?", [](let const& xs)
      {
        return std::fpclassify(car(xs).as<double>()) == FP_SUBNORMAL;
      }));

      library.define("binary64-max", make<procedure>("binary64-max", [](let const& xs)
      {
        auto max = -std::numeric_limits<double>::infinity();

        for (let const& x : xs)
        {
          max = std::fmax(max, x.as<double>());
        }

        return make(max);
      }));

      library.define("binary64-min", make<procedure>("binary64-min", [](let const& xs)
      {
        auto min = std::numeric_limits<double>::infinity();

        for (let const& x : xs)
        {
          min = std::fmin(min, x.as<double>());
        }

        return make(min);
      }));

      library.define("binary64-fused-multiply-add", make<procedure>("binary64-fused-multiply-add", [](let const& xs)
      {
        return make(std::fma(car(xs).as<double>(), cadr(xs).as<double>(), caddr(xs).as<double>()));
      }));

      library.define("binary64-remquo", make<procedure>("binary64-remquo", [](let const& xs)
      {
        auto quotient = 0;
        auto remainder = std::remquo(car(xs).as<double>(), cadr(xs).as<double>(), &quotient);
        return cons(make(remainder), make<small_integer>(quotient));
      }));
    }));

    libraries().emplace("(meevax boolean)", make<library>([](library & library)
    {
      EXPORT_PREDICATE(bool, "boolean?");

      library.define("not", make<procedure>("not", [](let const& xs)
      {
        return car(xs) == f;
      }));
    }));

    libraries().emplace("(meevax box)", make<library>([](library & library)
    {
      EXPORT1_RENAME(make<box>, "box");

      EXPORT_PREDICATE(box, "box?");

      library.define("box-ref", make<procedure>("box-ref", [](let const& xs)
      {
        return caar(xs);
      }));

      library.define("box-set!", make<procedure>("box-set!", [](let & xs)
      {
        caar(xs) = cadr(xs);
      }));
    }));

    libraries().emplace("(meevax character)", make<library>([](library & library)
    {
      EXPORT_PREDICATE(character, "char?");

      library.define("char=?",  make<procedure>("char=?",  [](let const& xs) { return std::adjacent_find(xs.begin(), xs.end(), [](let const& a, let const& b) { return not (a.as<character>().codepoint == b.as<character>().codepoint); }) == xs.end(); }));
      library.define("char<?",  make<procedure>("char<?",  [](let const& xs) { return std::adjacent_find(xs.begin(), xs.end(), [](let const& a, let const& b) { return not (a.as<character>().codepoint <  b.as<character>().codepoint); }) == xs.end(); }));
      library.define("char>?",  make<procedure>("char>?",  [](let const& xs) { return std::adjacent_find(xs.begin(), xs.end(), [](let const& a, let const& b) { return not (a.as<character>().codepoint >  b.as<character>().codepoint); }) == xs.end(); }));
      library.define("char<=?", make<procedure>("char<=?", [](let const& xs) { return std::adjacent_find(xs.begin(), xs.end(), [](let const& a, let const& b) { return not (a.as<character>().codepoint <= b.as<character>().codepoint); }) == xs.end(); }));
      library.define("char>=?", make<procedure>("char>=?", [](let const& xs) { return std::adjacent_find(xs.begin(), xs.end(), [](let const& a, let const& b) { return not (a.as<character>().codepoint >= b.as<character>().codepoint); }) == xs.end(); }));

      library.define("char-ci=?",  make<procedure>("char-ci=?",  [](let const& xs) { return std::adjacent_find(xs.begin(), xs.end(), [](let const& a, let const& b) { return not (a.as<character>().downcase() == b.as<character>().downcase()); }) == xs.end(); }));
      library.define("char-ci<?",  make<procedure>("char-ci<?",  [](let const& xs) { return std::adjacent_find(xs.begin(), xs.end(), [](let const& a, let const& b) { return not (a.as<character>().downcase() <  b.as<character>().downcase()); }) == xs.end(); }));
      library.define("char-ci>?",  make<procedure>("char-ci>?",  [](let const& xs) { return std::adjacent_find(xs.begin(), xs.end(), [](let const& a, let const& b) { return not (a.as<character>().downcase() >  b.as<character>().downcase()); }) == xs.end(); }));
      library.define("char-ci<=?", make<procedure>("char-ci<=?", [](let const& xs) { return std::adjacent_find(xs.begin(), xs.end(), [](let const& a, let const& b) { return not (a.as<character>().downcase() <= b.as<character>().downcase()); }) == xs.end(); }));
      library.define("char-ci>=?", make<procedure>("char-ci>=?", [](let const& xs) { return std::adjacent_find(xs.begin(), xs.end(), [](let const& a, let const& b) { return not (a.as<character>().downcase() >= b.as<character>().downcase()); }) == xs.end(); }));

      library.define("char-alphabetic?", make<procedure>("char-alphabetic?", [](let const& xs) { return car(xs).as<character>().property().is_letter    (); }));
      library.define("char-numeric?",    make<procedure>("char-numeric?",    [](let const& xs) { return car(xs).as<character>().property().is_numeric   (); }));
      library.define("char-whitespace?", make<procedure>("char-whitespace?", [](let const& xs) { return car(xs).as<character>().property().is_whitespace(); }));
      library.define("char-upper-case?", make<procedure>("char-upper-case?", [](let const& xs) { return car(xs).as<character>().property().is_upper_case(); }));
      library.define("char-lower-case?", make<procedure>("char-lower-case?", [](let const& xs) { return car(xs).as<character>().property().is_lower_case(); }));

      library.define("digit-value", make<procedure>("digit-value", [](let const& xs) -> object
      {
        if (auto digit_value = car(xs).as<character>().digit_value(); digit_value)
        {
          return make<small_integer>(*digit_value);
        }
        else
        {
          return f;
        }
      }));

      library.define("char->integer", make<procedure>("char->integer", [](let const& xs)
      {
        return make<small_integer>(car(xs).as<character>().codepoint);
      }));

      library.define("integer->char", make<procedure>("integer->char", [](let const& xs)
      {
        return make<character>(exact_integer_cast<character::int_type>(car(xs)));
      }));

      library.define("char-upcase",   make<procedure>("char-upcase",   [](let const& xs) { return make<character>(car(xs).as<character>().upcase  ()); }));
      library.define("char-downcase", make<procedure>("char-downcase", [](let const& xs) { return make<character>(car(xs).as<character>().downcase()); }));
    }));

    libraries().emplace("(meevax context)", make<library>([](library & library)
    {
      library.define("emergency-exit", make<procedure>("emergency-exit", [](let const& xs)
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
          throw exact_integer_cast<int>(status);
        }
      }));

      library.define("command-line", make<procedure>("command-line", []()
      {
        let xs = list();

        for (auto&& each : configurator::command_line())
        {
          xs = cons(make<string>(each), xs);
        }

        return reverse(xs);
      }));
    }));

    libraries().emplace("(meevax comparator)", make<library>([](library & library)
    {
      EXPORT2_RENAME(eq,    "eq?");
      EXPORT2_RENAME(equal, "equal?");
      EXPORT2_RENAME(eqv,   "eqv?");
    }));

    libraries().emplace("(meevax core)", make<library>([](library & library)
    {
      library.evaluator.second = core_syntactic_environment().as<syntactic_environment>().second;
      library.export_specs = map(car, library.evaluator.second);
    }));

    libraries().emplace("(meevax environment)", make<library>([](library & library)
    {
      library.define("environment", make<procedure>("environment", [](let const& xs)
      {
        auto e = environment();

        for (let const& x : xs)
        {
          e.import(x);
        }

        return make(e);
      }));

      library.define("eval", make<procedure>("eval", [](let const& xs)
      {
        return cadr(xs).as<environment>().evaluate(car(xs));
      }));

      library.define("expand", make<procedure>("expand", [](let const& xs)
      {
        return cadr(xs).as<environment>().expand(car(xs), unit);
      }));

      library.define("interaction-environment", make<procedure>("interaction-environment", []()
      {
        return interaction_environment();
      }));

      library.define("load", make<procedure>("load", [](let const& xs)
      {
        return car(xs).as<environment>().load(cadr(xs).as<string>().utf8());
      }));
    }));

    libraries().emplace("(meevax error)", make<library>([](library & library)
    {
      library.define("throw", make<procedure>("throw", [](let const& xs)
      {
        throw car(xs);
      }));

      library.define("error-object", make<procedure>("error-object", [](let const& xs)
      {
        return make<error>(car(xs), cdr(xs));
      }));

      library.define("error-object?", make<procedure>("error-object?", [](let const& xs)
      {
        return car(xs).is_also<error>();
      }));

      EXPORT_PREDICATE(read_error, "read-error?");
      EXPORT_PREDICATE(file_error, "file-error?");

      library.define("kernel-exception-handler-set!", make<procedure>("kernel-exception-handler-set!", [](let const& xs)
      {
        environment::exception_handler = car(xs);
      }));
    }));

    libraries().emplace("(meevax file)", make<library>([](library & library)
    {
      library.define("file-exists?", make<procedure>("file-exists?", [](let const& xs)
      {
        return std::filesystem::exists(car(xs).as<string>().utf8());
      }));

      library.define("delete-file", make<procedure>("delete-file", [](let const& xs)
      {
        try
        {
          if (not std::filesystem::remove(car(xs).as<string>().utf8()))
          {
            throw file_error(make<string>("failed to remove file"), car(xs));
          }
        }
        catch (std::filesystem::filesystem_error const& e)
        {
          throw file_error(make<string>(e.what()), car(xs));
        }
      }));

      library.define("library-directories", make<procedure>("library-directories", [](let const&)
      {
        let directories = unit;

        for (auto iterator = configurator::directories().rbegin(); iterator != configurator::directories().rend(); ++iterator)
        {
          directories = cons(make<string>(iterator->native()), directories);
        }

        return directories;
      }));
    }));

    libraries().emplace("(meevax instruction)", make<library>([](library & library)
    {
      library.define("secd-call",              make<instruction>(instruction::secd_call             ));
      library.define("secd-cons",              make<instruction>(instruction::secd_cons             ));
      library.define("secd-current",           make<instruction>(instruction::secd_current          ));
      library.define("secd-drop",              make<instruction>(instruction::secd_drop             ));
      library.define("secd-dummy",             make<instruction>(instruction::secd_dummy            ));
      library.define("secd-install",           make<instruction>(instruction::secd_install          ));
      library.define("secd-join",              make<instruction>(instruction::secd_join             ));
      library.define("secd-letrec",            make<instruction>(instruction::secd_letrec           ));
      library.define("secd-load-absolute",     make<instruction>(instruction::secd_load_absolute    ));
      library.define("secd-load-closure",      make<instruction>(instruction::secd_load_closure     ));
      library.define("secd-load-constant",     make<instruction>(instruction::secd_load_constant    ));
      library.define("secd-load-continuation", make<instruction>(instruction::secd_load_continuation));
      library.define("secd-load-relative",     make<instruction>(instruction::secd_load_relative    ));
      library.define("secd-load-variadic",     make<instruction>(instruction::secd_load_variadic    ));
      library.define("secd-return",            make<instruction>(instruction::secd_return           ));
      library.define("secd-select",            make<instruction>(instruction::secd_select           ));
      library.define("secd-stop",              make<instruction>(instruction::secd_stop             ));
      library.define("secd-store-absolute",    make<instruction>(instruction::secd_store_absolute   ));
      library.define("secd-store-relative",    make<instruction>(instruction::secd_store_relative   ));
      library.define("secd-store-variadic",    make<instruction>(instruction::secd_store_variadic   ));
      library.define("secd-tail-call",         make<instruction>(instruction::secd_tail_call        ));
      library.define("secd-tail-letrec",       make<instruction>(instruction::secd_tail_letrec      ));
      library.define("secd-tail-select",       make<instruction>(instruction::secd_tail_select      ));
    }));

    libraries().emplace("(meevax integer32)", make<library>([](library & library)
    {
      EXPORT_PREDICATE(small_integer, "integer32?");

      library.define("integer32-width", make<small_integer>(32));

      library.define("integer32-min", make<small_integer>(std::numeric_limits<small_integer>::min()));
      library.define("integer32-max", make<small_integer>(std::numeric_limits<small_integer>::max()));
    }));

    libraries().emplace("(meevax list)", make<library>([](library & library)
    {
      EXPORT_PREDICATE(null, "null?");

      EXPORT1_RENAME(is_list, "list?");

      library.define("list", make<procedure>("list", [](let const& xs)
      {
        return xs;
      }));

      library.define("make-list", make<procedure>("make-list", [](let const& xs)
      {
        switch (length(xs))
        {
        case 1:
          return make_list(exact_integer_cast<std::size_t>(car(xs)));

        case 2:
          return make_list(exact_integer_cast<std::size_t>(car(xs)), cadr(xs));

        default:
          throw error(make<string>("procedure make-list takes one or two arugments, but got"), xs);
        }
      }));

      library.define("iota", make<procedure>("iota", [](let const& xs)
      {
        switch (length(xs))
        {
        case 1:
          return iota(exact_integer_cast<std::size_t>(car(xs)));

        case 2:
          return iota(exact_integer_cast<std::size_t>(car(xs)), cadr(xs));

        case 3:
          return iota(exact_integer_cast<std::size_t>(car(xs)), cadr(xs), caddr(xs));

        default:
          throw error(make<string>("procedure iota takes one to three arugments, but got"), xs);
        }
      }));

      EXPORT1_RENAME(is_circular_list, "circular-list?");

      library.define("circular-list", make<procedure>("circular-list", [](let & xs)
      {
        circulate(xs);
        return xs;
      }));

      EXPORT1_RENAME(is_dotted_list, "dotted-list?");

      library.define("null-list?", make<procedure>("null-list?", [](let const& xs)
      {
        if (is_list(car(xs)) or is_circular_list(car(xs)))
        {
          return car(xs).is<null>();
        }
        else
        {
          throw error(make<string>("procedure null-list? takes a proper-list or a circular-list, but got"), xs);
        }
      }));

      EXPORT1_RENAME(last, "last");

      EXPORT1_RENAME(last_pair, "last-pair");

      library.define("length", make<procedure>("length", [](let const& xs)
      {
        return make(static_cast<small_integer>(length(car(xs))));
      }));

      library.define("length+", make<procedure>("length+", [](let const& xs) -> object
      {
        if (is_circular_list(car(xs)))
        {
          return f;
        }
        else
        {
          return make(static_cast<small_integer>(length(car(xs))));
        }
      }));

      library.define("append", make<procedure>("append", [](let const& xs)
      {
        return std::accumulate(xs.begin(), xs.end(), unit, [](let const& x, let const& y) { return append(x, y); });
      }));

      library.define("append!", make<procedure>("append!", [](let & xs)
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
      }));

      EXPORT2_RENAME(append_reverse, "append-reverse");

      library.define("append-reverse!", make<procedure>("append-reverse!", [](let & xs)
      {
        return append_reverse(car(xs), cadr(xs));
      }));

      EXPORT1_RENAME(reverse, "reverse");

      library.define("reverse!", make<procedure>("reverse!", [](let & xs)
      {
        return reverse(car(xs));
      }));

      library.define("concatenate", make<procedure>("concatenate", [](let const& xs)
      {
        return std::accumulate(car(xs).begin(), car(xs).end(), unit, [](let const& x, let const& y) { return append(x, y); });
      }));

      library.define("concatenate!", make<procedure>("concatenate!", [](let & xs)
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
      }));

      EXPORT1_RENAME(list_copy, "list-copy");

      library.define("list-ref",  make<procedure>("list-ref",  [](let const& xs) { return head(car(xs), exact_integer_cast<std::size_t>(cadr(xs))); }));
      library.define("list-tail", make<procedure>("list-tail", [](let const& xs) { return tail(car(xs), exact_integer_cast<std::size_t>(cadr(xs))); }));

      library.define("first",   make<procedure>("first",   [](let const& xs) { return head(car(xs), 0); }));
      library.define("second",  make<procedure>("second",  [](let const& xs) { return head(car(xs), 1); }));
      library.define("third",   make<procedure>("third",   [](let const& xs) { return head(car(xs), 2); }));
      library.define("fourth",  make<procedure>("fourth",  [](let const& xs) { return head(car(xs), 3); }));
      library.define("fifth",   make<procedure>("fifth",   [](let const& xs) { return head(car(xs), 4); }));
      library.define("sixth",   make<procedure>("sixth",   [](let const& xs) { return head(car(xs), 5); }));
      library.define("seventh", make<procedure>("seventh", [](let const& xs) { return head(car(xs), 6); }));
      library.define("eighth",  make<procedure>("eighth",  [](let const& xs) { return head(car(xs), 7); }));
      library.define("ninth",   make<procedure>("ninth",   [](let const& xs) { return head(car(xs), 8); }));
      library.define("tenth",   make<procedure>("tenth",   [](let const& xs) { return head(car(xs), 9); }));

      library.define("take",        make<procedure>("take",        [](let const& xs) { return take      (car(xs), exact_integer_cast<std::size_t>(cadr(xs))); }));
      library.define("take!",       make<procedure>("take!",       [](let      & xs) { return take      (car(xs), exact_integer_cast<std::size_t>(cadr(xs))); }));
      library.define("take-right",  make<procedure>("take-right",  [](let const& xs) { return take_right(car(xs), exact_integer_cast<std::size_t>(cadr(xs))); }));
      library.define("drop",        make<procedure>("drop",        [](let const& xs) { return drop      (car(xs), exact_integer_cast<std::size_t>(cadr(xs))); }));
      library.define("drop-right",  make<procedure>("drop-right",  [](let const& xs) { return drop_right(car(xs), exact_integer_cast<std::size_t>(cadr(xs))); }));
      library.define("drop-right!", make<procedure>("drop-right!", [](let      & xs) { return drop_right(car(xs), exact_integer_cast<std::size_t>(cadr(xs))); }));

      EXPORT2_RENAME(memq, "memq");
      EXPORT2_RENAME(memv, "memv");
      EXPORT2_RENAME(assq, "assq");
      EXPORT2_RENAME(assv, "assv");

      EXPORT3_RENAME(alist_cons, "alist-cons");
      EXPORT1_RENAME(alist_copy, "alist-copy");
    }));

    libraries().emplace("(meevax number)", make<library>([](library & library)
    {
      using namespace number;

      // R7RS 6.2.6 Numerial operations

      EXPORT1_RENAME(is_complex,       "number?");
      EXPORT1_RENAME(is_complex,       "complex?");
      EXPORT1_RENAME(is_real,          "real?");
      EXPORT1_RENAME(is_rational,      "rational?");
      EXPORT1_RENAME(is_integer,       "integer?");
      EXPORT1_RENAME(is_exact,         "exact?");
      EXPORT1_RENAME(is_inexact,       "inexact?");
      EXPORT1_RENAME(is_exact_integer, "exact-integer?");
      EXPORT1_RENAME(is_finite,        "finite?");
      EXPORT1_RENAME(is_infinite,      "infinite?");
      EXPORT1_RENAME(is_nan,           "nan?");

      library.define("=",  make<procedure>("=",  [](let const& xs) { return std::adjacent_find(xs.begin(), xs.end(), not_equals            ) == xs.end(); }));
      library.define("<",  make<procedure>("<",  [](let const& xs) { return std::adjacent_find(xs.begin(), xs.end(), greater_than_or_equals) == xs.end(); }));
      library.define("<=", make<procedure>("<=", [](let const& xs) { return std::adjacent_find(xs.begin(), xs.end(), greater_than          ) == xs.end(); }));
      library.define(">",  make<procedure>(">",  [](let const& xs) { return std::adjacent_find(xs.begin(), xs.end(), less_than_or_equals   ) == xs.end(); }));
      library.define(">=", make<procedure>(">=", [](let const& xs) { return std::adjacent_find(xs.begin(), xs.end(), less_than             ) == xs.end(); }));

      EXPORT1_RENAME(is_zero,     "zero?");
      EXPORT1_RENAME(is_positive, "positive?");
      EXPORT1_RENAME(is_negative, "negative?");
      EXPORT1_RENAME(is_odd,      "odd?");
      EXPORT1_RENAME(is_even,     "even?");

      library.define("max", make<procedure>("max", [](let const& xs) { if (auto iter = std::max_element(xs.begin(), xs.end(), less_than); iter != xs.end()) { return std::any_of(xs.begin(), xs.end(), is_inexact) ? inexact(*iter) : *iter; } else { return unspecified; } }));
      library.define("min", make<procedure>("min", [](let const& xs) { if (auto iter = std::min_element(xs.begin(), xs.end(), less_than); iter != xs.end()) { return std::any_of(xs.begin(), xs.end(), is_inexact) ? inexact(*iter) : *iter; } else { return unspecified; } }));

      library.define("+", make<procedure>("+", [](let const& xs) { return std::accumulate(xs.begin(), xs.end(), e0, std::plus      ()); }));
      library.define("*", make<procedure>("*", [](let const& xs) { return std::accumulate(xs.begin(), xs.end(), e1, std::multiplies()); }));

      library.define("-", make<procedure>("-", [](let const& xs) { if (cdr(xs).is<pair>()) { return std::accumulate(std::next(xs.begin()), xs.end(), car(xs), std::minus  ()); } else { return e0 - car(xs); } }));
      library.define("/", make<procedure>("/", [](let const& xs) { if (cdr(xs).is<pair>()) { return std::accumulate(std::next(xs.begin()), xs.end(), car(xs), std::divides()); } else { return e1 / car(xs); } }));

      EXPORT1_RENAME(abs,       "abs");
      EXPORT2_RENAME(quotient,  "quotient");
      EXPORT2_RENAME(remainder, "remainder");
      EXPORT2_RENAME(modulo,    "modulo");

      library.define("gcd", make<procedure>("gcd", [](let const& xs) { switch (length(xs)) { case 0: return e0; case 1: return car(xs); default: return std::accumulate(cdr(xs).begin(), xs.end(), car(xs), gcd); } }));
      library.define("lcm", make<procedure>("lcm", [](let const& xs) { switch (length(xs)) { case 0: return e1; case 1: return car(xs); default: return std::accumulate(cdr(xs).begin(), xs.end(), car(xs), lcm); } }));

      EXPORT1_RENAME(numerator,   "numerator");
      EXPORT1_RENAME(denominator, "denominator");
      EXPORT1_RENAME(floor,       "floor");
      EXPORT1_RENAME(ceiling,     "ceiling");
      EXPORT1_RENAME(truncate,    "truncate");
      EXPORT1_RENAME(round,       "round");

      EXPORT1_RENAME(exp, "exp");

      library.define("log", make<procedure>("log", [](let const& xs)
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
      }));

      EXPORT1_RENAME(sin, "sin"); EXPORT1_RENAME(asin, "asin"); EXPORT1_RENAME(sinh, "sinh"); EXPORT1_RENAME(asinh, "asinh");
      EXPORT1_RENAME(cos, "cos"); EXPORT1_RENAME(acos, "acos"); EXPORT1_RENAME(cosh, "cosh"); EXPORT1_RENAME(acosh, "acosh");
      EXPORT1_RENAME(tan, "tan");                               EXPORT1_RENAME(tanh, "tanh"); EXPORT1_RENAME(atanh, "atanh");

      library.define("atan", make<procedure>("atan", [](let const& xs)
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
      }));

      EXPORT1_RENAME(sqrt, "sqrt");

      library.define("exact-integer-square-root", make<procedure>("exact-integer-square-root", [](let const& xs)
      {
        auto sqrt = [](let const& x)
        {
          if (x.is<small_integer>())
          {
            return large_integer(x.as<small_integer>()).sqrt();
          }
          else
          {
            return x.as<large_integer>().sqrt();
          }
        };

        auto&& [s, r] = sqrt(car(xs));

        return cons(make(std::forward<decltype(s)>(s)),
                    make(std::forward<decltype(r)>(r)));
      }));

      EXPORT2_RENAME(pow, "expt");

      EXPORT2_RENAME(make<complex>, "make-rectangular");

      library.define("make-polar", make<procedure>("make-polar", [](let const& xs)
      {
        let const& radius = car(xs), angle = cadr(xs);

        return make<complex>(radius * cos(angle),
                             radius * sin(angle));
      }));

      EXPORT1_RENAME(real,      "real-part");
      EXPORT1_RENAME(imag,      "imag-part");
      EXPORT1_RENAME(magnitude, "magnitude");
      EXPORT1_RENAME(angle,     "angle");

      EXPORT1_RENAME(exact,   "exact");
      EXPORT1_RENAME(inexact, "inexact");

      // Exponential functions (cmath)
      EXPORT1_RENAME(expm1, "expm1");
      EXPORT1_RENAME(log1p, "log1p");

      // Error and gamma functions (cmath)
      EXPORT1_RENAME(erf,    "erf");
      EXPORT1_RENAME(erfc,   "erfc");
      EXPORT1_RENAME(tgamma, "tgamma");
      EXPORT1_RENAME(lgamma, "lgamma");

      // Floating-point manipulation functions (cmath)
      EXPORT2_RENAME(ldexp,     "ldexp");
      EXPORT2_RENAME(nextafter, "nextafter");
      EXPORT2_RENAME(copysign,  "copysign");

      // Mathematical special functions (cmath)
      EXPORT2_RENAME(cyl_bessel_j, "cyl_bessel_j");
      EXPORT2_RENAME(cyl_neumann,  "cyl_neumann");

      library.define("e",     make<double>(std::numbers::e));
      library.define("pi",    make<double>(std::numbers::pi));
      library.define("euler", make<double>(std::numbers::egamma));
      library.define("phi",   make<double>(std::numbers::phi));

      // SRFI 151: Bitwise Operations

      EXPORT1_RENAME(bitwise_not, "bitwise-not");

      library.define("bitwise-and", make<procedure>("bitwise-and", [](let const& xs) { return std::accumulate(xs.begin(), xs.end(), make<small_integer>(-1), bitwise_and); }));
      library.define("bitwise-ior", make<procedure>("bitwise-ior", [](let const& xs) { return std::accumulate(xs.begin(), xs.end(), make<small_integer>( 0), bitwise_ior); }));
      library.define("bitwise-xor", make<procedure>("bitwise-xor", [](let const& xs) { return std::accumulate(xs.begin(), xs.end(), make<small_integer>( 0), bitwise_xor); }));

      library.define("bit-shift", make<procedure>("bit-shift", [](let const& xs)
      {
        return bit_shift(car(xs), exact_integer_cast<small_integer>(cadr(xs)));
      }));

      EXPORT1_RENAME(bit_count, "bit-count");
      EXPORT1_RENAME(bit_width, "bit-width");

      // R7RS 6.2.7 Numerial input and output

      library.define("number->string", make<procedure>("number->string", [](let const& xs)
      {
        switch (length(xs))
        {
        case 1:
          return number_to_string(car(xs), 10);

        case 2:
          return number_to_string(car(xs), exact_integer_cast<std::size_t>(cadr(xs)));

        default:
          throw error(make<string>("procedure number->string takes one or two arugments, but got"), xs);
        }
      }));

      library.define("string->number", make<procedure>("string->number", [](let const& xs)
      {
        switch (length(xs))
        {
        case 1:
          return make_number(car(xs).as<string>().utf8(), 10);

        case 2:
          return make_number(car(xs).as<string>().utf8(), exact_integer_cast<std::size_t>(cadr(xs)));

        default:
          throw error(make<string>("procedure string->number takes one or two arugments, but got"), xs);
        }
      }));
    }));

    libraries().emplace("(meevax pair)", make<library>([](library & library)
    {
      EXPORT_PREDICATE(pair, "pair?");

      library.define("not-pair?", make<procedure>("not-pair?", [](let const& xs)
      {
        return not car(xs).is<pair>();
      }));

      EXPORT2_RENAME(cons,  "cons");
      EXPORT2_RENAME(xcons, "xcons");

      library.define("cons*", make<procedure>("cons*", [](let & xs)
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
      }));

      EXPORT1_RENAME(car, "car");
      EXPORT1_RENAME(cdr, "cdr");

      EXPORT1_RENAME(caar, "caar"); EXPORT1_RENAME(cdar, "cdar");
      EXPORT1_RENAME(cadr, "cadr"); EXPORT1_RENAME(cddr, "cddr");

      EXPORT1_RENAME(caaar, "caaar"); EXPORT1_RENAME(cdaar, "cdaar");
      EXPORT1_RENAME(caadr, "caadr"); EXPORT1_RENAME(cdadr, "cdadr");
      EXPORT1_RENAME(cadar, "cadar"); EXPORT1_RENAME(cddar, "cddar");
      EXPORT1_RENAME(caddr, "caddr"); EXPORT1_RENAME(cdddr, "cdddr");

      EXPORT1_RENAME(caaaar, "caaaar"); EXPORT1_RENAME(cdaaar, "cdaaar");
      EXPORT1_RENAME(caaadr, "caaadr"); EXPORT1_RENAME(cdaadr, "cdaadr");
      EXPORT1_RENAME(caadar, "caadar"); EXPORT1_RENAME(cdadar, "cdadar");
      EXPORT1_RENAME(caaddr, "caaddr"); EXPORT1_RENAME(cdaddr, "cdaddr");
      EXPORT1_RENAME(cadaar, "cadaar"); EXPORT1_RENAME(cddaar, "cddaar");
      EXPORT1_RENAME(cadadr, "cadadr"); EXPORT1_RENAME(cddadr, "cddadr");
      EXPORT1_RENAME(caddar, "caddar"); EXPORT1_RENAME(cdddar, "cdddar");
      EXPORT1_RENAME(cadddr, "cadddr"); EXPORT1_RENAME(cddddr, "cddddr");

      library.define("set-car!", make<procedure>("set-car!", [](let & xs) { caar(xs) = cadr(xs); }));
      library.define("set-cdr!", make<procedure>("set-cdr!", [](let & xs) { cdar(xs) = cadr(xs); }));
    }));

    libraries().emplace("(meevax port)", make<library>([](library & library)
    {
      library.define(  "input-port?", make<procedure>(  "input-port?", [](let const& xs) { return car(xs).is_also<  input_port>(); }));
      library.define( "output-port?", make<procedure>( "output-port?", [](let const& xs) { return car(xs).is_also< output_port>(); }));
      library.define( "binary-port?", make<procedure>( "binary-port?", [](let const& xs) { return car(xs).is_also< binary_port>(); }));
      library.define("textual-port?", make<procedure>("textual-port?", [](let const& xs) { return car(xs).is_also<textual_port>(); }));
      library.define(        "port?", make<procedure>(        "port?", [](let const& xs) { return car(xs).is_also<        port>(); }));

      library.define("open?", make<procedure>("open?", [](let const& xs)
      {
        return car(xs).as<port>().is_open();
      }));

      library.define( "standard-input-port", make<procedure>( "standard-input-port", []() { return make< standard_input_port>(); }));
      library.define("standard-output-port", make<procedure>("standard-output-port", []() { return make<standard_output_port>(); }));
      library.define( "standard-error-port", make<procedure>( "standard-error-port", []() { return make< standard_error_port>(); }));

      library.define(        "open-input-file", make<procedure>(        "open-input-file", [](let const& xs) { return make<        input_file_port>(car(xs).as<string>().utf8()); }));
      library.define(       "open-output-file", make<procedure>(       "open-output-file", [](let const& xs) { return make<       output_file_port>(car(xs).as<string>().utf8()); }));
      library.define( "open-binary-input-file", make<procedure>( "open-binary-input-file", [](let const& xs) { return make< binary_input_file_port>(car(xs).as<string>().utf8()); }));
      library.define("open-binary-output-file", make<procedure>("open-binary-output-file", [](let const& xs) { return make<binary_output_file_port>(car(xs).as<string>().utf8()); }));

      library.define("close", make<procedure>("close", [](let const& xs)
      {
        car(xs).as<port>().close();
      }));

      library.define("open-input-string", make<procedure>("open-input-string", [](let const& xs)
      {
        return make<input_string_port>(car(xs).as<string>().utf8());
      }));

      library.define("open-output-string", make<procedure>("open-output-string", [](let const&)
      {
        return make<output_string_port>();
      }));

      library.define("get-output-string", make<procedure>("get-output-string", [](let const& xs)
      {
        return make<string>(car(xs).as<output_string_port>().ostringstream.str());
      }));

      library.define("open-input-u8vector", make<procedure>("open-input-u8vector", [](let const& xs)
      {
        return make<input_u8vector_port>(car(xs).as<u8vector>());
      }));

      library.define("open-output-u8vector", make<procedure>("open-output-u8vector", [](let const&)
      {
        return make<output_u8vector_port>();
      }));

      library.define("get-output-u8vector", make<procedure>("get-output-u8vector", [](let const& xs)
      {
        return make<u8vector>(car(xs).as<output_u8vector_port>().vector.data(),
                              car(xs).as<output_u8vector_port>().vector.size());
      }));

      library.define("eof-object?", make<procedure>("eof-object?", [](let const& xs)
      {
        return car(xs).is<eof>();
      }));

      library.define("eof-object", make<procedure>("eof-object", []()
      {
        return eof_object;
      }));

      library.define("flush", make<procedure>("flush", [](let const& xs)
      {
        car(xs).as<output_port>().flush();
      }));
    }));

    libraries().emplace("(meevax procedure)", make<library>([](library & library)
    {
      EXPORT_PREDICATE(closure,      "closure?");
      EXPORT_PREDICATE(continuation, "continuation?");

      library.define("procedure?", make<procedure>("procedure?", [](let const& xs)
      {
        return car(xs).is<closure>() or car(xs).is<continuation>() or car(xs).is_also<primitive>();
      }));

      library.define("procedure", make<procedure>("procedure", [](let const& xs)
      {
        return make<procedure>(cadr(xs).as<symbol>(),
                               reinterpret_cast<primitive::signature>(
                                 default_collector::dlsym(cadr(xs).as<symbol>(),
                                                          default_collector::dlopen(car(xs).as<string>().utf8()))));
      }));
    }));

    libraries().emplace("(meevax read)", make<library>([](library & library)
    {
      library.define("get-char",        make<procedure>("get-char",        [](let const& xs) { return car(xs).as<textual_input_port>().get      (); }));
      library.define("get-char-ready?", make<procedure>("get-char-ready?", [](let const& xs) { return car(xs).as<textual_input_port>().get_ready(); }));
      library.define("get-line",        make<procedure>("get-line",        [](let const& xs) { return car(xs).as<textual_input_port>().get_line (); }));
      library.define("peek-char",       make<procedure>("peek-char",       [](let const& xs) { return car(xs).as<textual_input_port>().peek     (); }));
      library.define("read",            make<procedure>("read",            [](let const& xs) { return car(xs).as<textual_input_port>().read     (); }));

      library.define("get-string", make<procedure>("get-string", [](let const& xs)
      {
        return cadr(xs).as<textual_input_port>().get(exact_integer_cast<std::size_t>(car(xs)));
      }));

      library.define("get-u8",        make<procedure>("get-u8",        [](let const& xs) { return car(xs).as<binary_input_port>().get      (); }));
      library.define("get-u8-ready?", make<procedure>("get-u8-ready?", [](let const& xs) { return car(xs).as<binary_input_port>().get_ready(); }));
      library.define("peek-u8",       make<procedure>("peek-u8",       [](let const& xs) { return car(xs).as<binary_input_port>().peek     (); }));

      library.define("get-u8vector", make<procedure>("get-u8vector", [](let const& xs)
      {
        return cadr(xs).as<binary_input_port>().get(exact_integer_cast<std::size_t>(car(xs)));
      }));
    }));

    libraries().emplace("(meevax string)", make<library>([](library & library)
    {
      EXPORT_PREDICATE(string, "string?");

      library.define("make-string", make<procedure>("make-string", [](let const& xs)
      {
        switch (length(xs))
        {
        case 1:
          return make<string>(exact_integer_cast<std::size_t>(car(xs)), character());

        case 2:
          return make<string>(exact_integer_cast<std::size_t>(car(xs)), cadr(xs).as<character>());

        default:
          throw error(make<string>("procedure make-string takes one or two arugments, but got"), xs);
        }
      }));

      library.define("string", make<procedure>("string", [](let const& xs)
      {
        return make_string_from_list_of_character(xs);
      }));

      library.define("string-length", make<procedure>("string-length", [](let const& xs)
      {
        return make(static_cast<small_integer>(car(xs).as<string>().characters.size())); // XXX DIRTY HACK (MAKE large_integer IF THE LENGTH IS GREATER THAN INT_MAX)
      }));

      library.define("string-ref", make<procedure>("string-ref", [](let const& xs)
      {
        return make(car(xs).as<string>().characters.at(exact_integer_cast<std::size_t>(cadr(xs))));
      }));

      library.define("string-set!", make<procedure>("string-set!", [](let & xs)
      {
        car(xs).as<string>().characters.at(exact_integer_cast<std::size_t>(cadr(xs))) = caddr(xs).as<character>();
      }));

      #define EXPORT_STRING_COMPARE(OPERATOR, IDENTIFIER)                      \
      library.define(IDENTIFIER, make<procedure>(IDENTIFIER, [](let const& xs) \
      {                                                                        \
        auto compare = [](let const& a, let const& b)                          \
        {                                                                      \
          return not (a.as<string>() OPERATOR b.as<string>());                 \
        };                                                                     \
                                                                               \
        return std::adjacent_find(xs.begin(), xs.end(), compare) == xs.end();  \
      }))

      EXPORT_STRING_COMPARE(==, "string=?");
      EXPORT_STRING_COMPARE(< , "string<?");
      EXPORT_STRING_COMPARE(> , "string>?");
      EXPORT_STRING_COMPARE(<=, "string<=?");
      EXPORT_STRING_COMPARE(>=, "string>=?");

      #define EXPORT_STRING_CI_COMPARE(OPERATOR, IDENTIFIER)                   \
      library.define(IDENTIFIER, make<procedure>(IDENTIFIER, [](let const& xs) \
      {                                                                        \
        auto compare = [](let const& s1, let const& s2)                        \
        {                                                                      \
          auto compare = [](auto const& c1, auto const& c2)                    \
          {                                                                    \
            return c1.downcase() OPERATOR c2.downcase();                       \
          };                                                                   \
                                                                               \
          return not std::lexicographical_compare(s1.as<string>().characters.begin(), \
                                                  s1.as<string>().characters.end(), \
                                                  s2.as<string>().characters.begin(), \
                                                  s2.as<string>().characters.end(), \
                                                  compare); \
        };                                                                     \
                                                                               \
        return std::adjacent_find(xs.begin(), xs.end(), compare) == xs.end();  \
      }))

      EXPORT_STRING_CI_COMPARE(==, "string-ci=?");
      EXPORT_STRING_CI_COMPARE(< , "string-ci<?");
      EXPORT_STRING_CI_COMPARE(> , "string-ci>?");
      EXPORT_STRING_CI_COMPARE(<=, "string-ci<=?");
      EXPORT_STRING_CI_COMPARE(>=, "string-ci>=?");

      library.define("string-append", make<procedure>("string-append", [](let const& xs)
      {
        let s = make<string>();

        for (let const& x : xs)
        {
          s.as<string>().characters.insert(s.as<string>().characters.end(),
                                           x.as<string>().characters.begin(),
                                           x.as<string>().characters.end());
        }

        return s;
      }));

      library.define("string->list", make<procedure>("string->list", [](let const& xs)
      {
        auto push = [](let const& xs, character const& c)
        {
          return cons(make(c), xs);
        };

        switch (length(xs))
        {
        case 1:
          return std::accumulate(car(xs).as<string>().characters.rbegin(),
                                 car(xs).as<string>().characters.rend(),
                                 unit,
                                 push);
        case 2:
          return std::accumulate(car(xs).as<string>().characters.rbegin(),
                                 std::prev(car(xs).as<string>().characters.rend(), exact_integer_cast<std::size_t>(cadr(xs))),
                                 unit,
                                 push);
        case 3:
          return std::accumulate(std::prev(car(xs).as<string>().characters.rend(), exact_integer_cast<std::size_t>(caddr(xs))),
                                 std::prev(car(xs).as<string>().characters.rend(), exact_integer_cast<std::size_t>(cadr(xs))),
                                 unit,
                                 push);

        default:
          throw error(make<string>("procedure string->list takes one to three arugments, but got"), xs);
        }
      }));

      library.define("list->string", make<procedure>("list->string", [](let const& xs)
      {
        return make_string_from_list_of_character(car(xs));
      }));

      library.define("string-copy", make<procedure>("string-copy", [](let const& xs)
      {
        switch (length(xs))
        {
        case 1:
          return make<string>(car(xs).as<string>().characters.begin(),
                              car(xs).as<string>().characters.end());
        case 2:
          return make<string>(std::next(car(xs).as<string>().characters.begin(), exact_integer_cast<std::size_t>(cadr(xs))),
                              car(xs).as<string>().characters.end());
        case 3:
          return make<string>(std::next(car(xs).as<string>().characters.begin(), exact_integer_cast<std::size_t>(cadr(xs))),
                              std::next(car(xs).as<string>().characters.begin(), exact_integer_cast<std::size_t>(caddr(xs))));
        default:
          throw error(make<string>("procedure string-copy takes one to three arugments, but got"), xs);
        }
      }));

      library.define("string-copy!", make<procedure>("string-copy!", [](let const& xs)
      {
        car(xs).as<string>().characters.reserve(car(xs).as<string>().characters.size() + caddr(xs).as<string>().characters.size());

        switch (length(xs))
        {
        case 3:
          std::copy(caddr(xs).as<string>().characters.begin(),
                    caddr(xs).as<string>().characters.end(),
                    std::next(car(xs).as<string>().characters.begin(), exact_integer_cast<std::size_t>(cadr(xs))));
          break;

        case 4:
          std::copy(std::next(caddr(xs).as<string>().characters.begin(), exact_integer_cast<std::size_t>(cadddr(xs))),
                    caddr(xs).as<string>().characters.end(),
                    std::next(car(xs).as<string>().characters.begin(), exact_integer_cast<std::size_t>(cadr(xs))));
          break;

        case 5:
          std::copy(std::next(caddr(xs).as<string>().characters.begin(), exact_integer_cast<std::size_t>(cadddr(xs))),
                    std::next(caddr(xs).as<string>().characters.begin(), exact_integer_cast<std::size_t>(caddddr(xs))),
                    std::next(car(xs).as<string>().characters.begin(), exact_integer_cast<std::size_t>(cadr(xs))));
          break;

        default:
          throw error(make<string>("procedure string-copy takes three to five arugments, but got"), xs);
        }
      }));

      library.define("string-fill!", make<procedure>("string-fill!", [](let & xs)
      {
        switch (length(xs))
        {
        case 2:
          std::fill(car(xs).as<string>().characters.begin(),
                    car(xs).as<string>().characters.end(),
                    cadr(xs).as<character>());
          break;

        case 3:
          std::fill(std::next(car(xs).as<string>().characters.begin(), exact_integer_cast<std::size_t>(caddr(xs))),
                    car(xs).as<string>().characters.end(),
                    cadr(xs).as<character>());
          break;

        case 4:
          std::fill(std::next(car(xs).as<string>().characters.begin(), exact_integer_cast<std::size_t>(caddr(xs))),
                    std::next(car(xs).as<string>().characters.begin(), exact_integer_cast<std::size_t>(cadddr(xs))),
                    cadr(xs).as<character>());
          break;

        default:
          throw error(make<string>("procedure string-fill! takes one to three arugments, but got"), xs);
        }
      }));
    }));

    libraries().emplace("(meevax symbol)", make<library>([](library & library)
    {
      EXPORT_PREDICATE(symbol, "symbol?");

      library.define("symbol->string", make<procedure>("symbol->string", [](let const& xs)
      {
        return make<string>(car(xs).as<symbol>());
      }));

      library.define("string->symbol", make<procedure>("string->symbol", [](let const& xs)
      {
        return make_symbol(car(xs).as<string>().utf8());
      }));

      library.define("identifier->symbol", make<procedure>("identifier->symbol", [](let const& xs)
      {
        if (let const& x = car(xs); x.is<syntactic_closure>())
        {
          return cddr(x);
        }
        else
        {
          return x;
        }
      }));
    }));

    libraries().emplace("(meevax syntactic-closure)", make<library>([](library & library)
    {
      library.define("identifier?", make<procedure>("identifier?", [](let const& xs)
      {
        return car(xs).is_also<identifier>();
      }));

      EXPORT_PREDICATE(transformer, "transformer?");
      EXPORT_PREDICATE(syntactic_closure, "syntactic-closure?");

      EXPORT3_RENAME(make<syntactic_closure>, "make-syntactic-closure");
    }));

    libraries().emplace("(meevax system)", make<library>([](library & library)
    {
      library.define("features", make<procedure>("features", []()
      {
        return features();
      }));

      library.define("get-environment-variable", make<procedure>("get-environment-variable", [](let const& xs) -> object
      {
        if (auto s = std::getenv(car(xs).as<string>().utf8().c_str()))
        {
          return make<string>(s);
        }
        else
        {
          return f;
        }
      }));

      library.define("get-environment-variables", make<procedure>("get-environment-variables", []()
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
      }));
    }));

    libraries().emplace("(meevax time)", make<library>([](library & library)
    {
      library.define("current-jiffy", make<procedure>("current-jiffy", []()
      {
        return make<large_integer>(std::chrono::high_resolution_clock::now().time_since_epoch().count());
      }));

      library.define("jiffies-per-second", make<procedure>("jiffies-per-second", []()
      {
        return make<large_integer>(std::chrono::high_resolution_clock::period::den);
      }));
    }));

    libraries().emplace("(meevax vector)", make<library>([](library & library)
    {
      EXPORT_PREDICATE(vector, "vector?");

      library.define("vector", make<procedure>("vector", [](let const& xs)
      {
        return make_vector(xs);
      }));

      library.define("make-vector", make<procedure>("make-vector", [](let const& xs)
      {
        switch (length(xs))
        {
        case 1:
          return make<vector>(exact_integer_cast<std::size_t>(car(xs)), unspecified);

        case 2:
          return make<vector>(exact_integer_cast<std::size_t>(car(xs)), cadr(xs));

        default:
          throw error(make<string>("procedure make-vector takes one or two arugments, but got"), xs);
        }
      }));

      library.define("vector-length", make<procedure>("vector-length", [](let const& xs)
      {
        return make(static_cast<small_integer>(car(xs).as<vector>().objects.size()));
      }));

      library.define("vector-ref", make<procedure>("vector-ref", [](let const& xs)
      {
        return car(xs).as<vector>().objects[exact_integer_cast<std::size_t>(cadr(xs))];
      }));

      library.define("vector-set!", make<procedure>("vector-set!", [](let & xs)
      {
        car(xs).as<vector>().objects[exact_integer_cast<std::size_t>(cadr(xs))] = caddr(xs);
      }));

      library.define("vector->list", make<procedure>("vector->list", [](let const& xs)
      {
        switch (length(xs))
        {
        case 1:
          return std::accumulate(car(xs).as<vector>().objects.rbegin(),
                                 car(xs).as<vector>().objects.rend(),
                                 unit,
                                 xcons);
        case 2:
          return std::accumulate(car(xs).as<vector>().objects.rbegin(),
                                 std::prev(car(xs).as<vector>().objects.rend(), exact_integer_cast<std::size_t>(cadr(xs))),
                                 unit,
                                 xcons);
        case 3:
          return std::accumulate(std::prev(car(xs).as<vector>().objects.rend(), exact_integer_cast<std::size_t>(caddr(xs))),
                                 std::prev(car(xs).as<vector>().objects.rend(), exact_integer_cast<std::size_t>(cadr(xs))),
                                 unit,
                                 xcons);
        default:
          throw error(make<string>("procedure vector->list takes one to three arugments, but got"), xs);
        }
      }));

      EXPORT1_RENAME(make_vector, "list->vector");

      library.define("vector->string", make<procedure>("vector->string", [](let const& xs)
      {
        let s = make<string>();

        auto push_back = [&](let const& x)
        {
          s.as<string>().characters.push_back(x.as<character>());
        };

        switch (length(xs))
        {
        case 1:
          std::for_each(car(xs).as<vector>().objects.begin(),
                        car(xs).as<vector>().objects.end(),
                        push_back);
          return s;

        case 2:
          std::for_each(std::next(car(xs).as<vector>().objects.begin(), exact_integer_cast<std::size_t>(cadr(xs))),
                        car(xs).as<vector>().objects.end(),
                        push_back);
          return s;

        case 3:
          std::for_each(std::next(car(xs).as<vector>().objects.begin(), exact_integer_cast<std::size_t>(cadr(xs))),
                        std::next(car(xs).as<vector>().objects.begin(), exact_integer_cast<std::size_t>(caddr(xs))),
                        push_back);
          return s;

        default:
          throw error(make<string>("procedure vector->list takes one to three arugments, but got"), xs);
        }
      }));

      library.define("string->vector", make<procedure>("string->vector", [](let const& xs)
      {
        let v = make<vector>();

        for (auto character : car(xs).as<string>().characters)
        {
          v.as<vector>().objects.push_back(make(character));
        }

        return v;
      }));

      library.define("vector-copy", make<procedure>("vector-copy", [](let const& xs)
      {
        switch (length(xs))
        {
        case 1:
          return make<vector>(car(xs).as<vector>().objects.begin(),
                              car(xs).as<vector>().objects.end());
        case 2:
          return make<vector>(std::next(car(xs).as<vector>().objects.begin(), exact_integer_cast<std::size_t>(cadr(xs))),
                              car(xs).as<vector>().objects.end());
        case 3:
          return make<vector>(std::next(car(xs).as<vector>().objects.begin(), exact_integer_cast<std::size_t>(cadr(xs))),
                              std::next(car(xs).as<vector>().objects.begin(), exact_integer_cast<std::size_t>(caddr(xs))));
        default:
          throw error(make<string>("procedure vector-copy takes one to three arugments, but got"), xs);
        }
      }));

      library.define("vector-copy!", make<procedure>("vector-copy!", [](let const& xs)
      {
        car(xs).as<vector>().objects.reserve(car(xs).as<vector>().objects.size() + caddr(xs).as<vector>().objects.size());

        switch (length(xs))
        {
        case 3:
          std::copy(caddr(xs).as<vector>().objects.begin(),
                    caddr(xs).as<vector>().objects.end(),
                    std::next(car(xs).as<vector>().objects.begin(), exact_integer_cast<std::size_t>(cadr(xs))));
          break;

        case 4:
          std::copy(std::next(caddr(xs).as<vector>().objects.begin(), exact_integer_cast<std::size_t>(cadddr(xs))),
                    caddr(xs).as<vector>().objects.end(),
                    std::next(car(xs).as<vector>().objects.begin(), exact_integer_cast<std::size_t>(cadr(xs))));
          break;

        case 5:
          std::copy(std::next(caddr(xs).as<vector>().objects.begin(), exact_integer_cast<std::size_t>(cadddr(xs))),
                    std::next(caddr(xs).as<vector>().objects.begin(), exact_integer_cast<std::size_t>(caddddr(xs))),
                    std::next(car(xs).as<vector>().objects.begin(), exact_integer_cast<std::size_t>(cadr(xs))));
          break;

        default:
          throw error(make<string>("procedure vector-copy takes three to five arugments, but got"), xs);
        }
      }));

      library.define("vector-append", make<procedure>("vector-append", [](let const& xs)
      {
        let v = make<vector>();

        for (let const& x : xs)
        {
          v.as<vector>().objects.insert(v.as<vector>().objects.end(),
                                        x.as<vector>().objects.begin(),
                                        x.as<vector>().objects.end());
        }

        return v;
      }));

      library.define("vector-fill!", make<procedure>("vector-fill!", [](let & xs)
      {
        switch (length(xs))
        {
        case 2:
          std::fill(car(xs).as<vector>().objects.begin(),
                    car(xs).as<vector>().objects.end(),
                    cadr(xs));
          break;

        case 3:
          std::fill(std::next(car(xs).as<vector>().objects.begin(), exact_integer_cast<std::size_t>(caddr(xs))),
                    car(xs).as<vector>().objects.end(),
                    cadr(xs));
          break;

        case 4:
          std::fill(std::next(car(xs).as<vector>().objects.begin(), exact_integer_cast<std::size_t>(caddr(xs))),
                    std::next(car(xs).as<vector>().objects.begin(), exact_integer_cast<std::size_t>(cadddr(xs))),
                    cadr(xs));
          break;
        }
      }));
    }));

    libraries().emplace("(meevax vector homogeneous)", make<library>([](library & library)
    {
      #define DEFINE_VECTOR(TAG)                                               \
      EXPORT_PREDICATE(TAG##vector, #TAG "vector?");                           \
                                                                               \
      library.define("make-" #TAG "vector", make<procedure>("make-" #TAG "vector", [](let const& xs) \
      {                                                                        \
        switch (length(xs))                                                    \
        {                                                                      \
        case 1:                                                                \
          return make<TAG##vector>(static_cast<TAG##vector::values_type::value_type>(0), exact_integer_cast<std::size_t>(car(xs))); \
                                                                               \
        case 2:                                                                \
          return make<TAG##vector>(TAG##vector::input_cast(cadr(xs)), exact_integer_cast<std::size_t>(car(xs))); \
                                                                               \
        default:                                                               \
          throw error(make<string>("procedure make-" #TAG "vector takes one or two arguments, but got"), xs); \
        }                                                                      \
      }));                                                                     \
                                                                               \
      library.define(#TAG "vector", make<procedure>(#TAG "vector", [](let const& xs) \
      {                                                                        \
        return make_homogeneous_vector_from_list_of<TAG>(xs);                  \
      }));                                                                     \
                                                                               \
      library.define(#TAG "vector-length", make<procedure>(#TAG "vector-length", [](let const& xs) \
      {                                                                        \
        return make(static_cast<small_integer>(car(xs).as<TAG##vector>().values.size())); \
      }));                                                                     \
                                                                               \
      library.define(#TAG "vector-ref", make<procedure>(#TAG "vector-ref", [](let const& xs) \
      {                                                                        \
        return TAG##vector::output_cast(car(xs).as<TAG##vector>().values[exact_integer_cast<std::size_t>(cadr(xs))]); \
      }));                                                                     \
                                                                               \
      library.define(#TAG "vector-set!", make<procedure>(#TAG "vector-set!", [](let const& xs) \
      {                                                                        \
        car(xs).as<TAG##vector>().values[exact_integer_cast<std::size_t>(cadr(xs))] = TAG##vector::input_cast(caddr(xs)); \
      }));                                                                     \
                                                                               \
      library.define(#TAG "vector-copy", make<procedure>(#TAG "vector-copy", [](let const& xs) \
      {                                                                        \
        auto copy = [&](std::size_t begin, std::size_t end)                    \
        {                                                                      \
          assert(begin <= end);                                                \
          return make<TAG##vector>(car(xs).as<TAG##vector>().values[std::slice(begin, end - begin, 1)]); \
        };                                                                     \
                                                                               \
        switch (length(xs))                                                    \
        {                                                                      \
        case 1:                                                                \
          return copy(0, car(xs).as<TAG##vector>().values.size());             \
                                                                               \
        case 2:                                                                \
          return copy(exact_integer_cast<std::size_t>(cadr(xs)),               \
                      car(xs).as<TAG##vector>().values.size());                \
                                                                               \
        case 3:                                                                \
          return copy(exact_integer_cast<std::size_t>(cadr(xs)),               \
                      exact_integer_cast<std::size_t>(caddr(xs)));             \
                                                                               \
        default:                                                               \
          throw error(make<string>("procedure " #TAG "vector-copy takes one to three arguments, but got"), xs); \
        }                                                                      \
      }));                                                                     \
                                                                               \
      library.define(#TAG "vector-copy!", make<procedure>(#TAG "vector-copy!", [](let & xs) \
      {                                                                        \
        auto copy = [&](std::size_t at, std::size_t begin, std::size_t end)    \
        {                                                                      \
          assert(begin <= end);                                                \
          auto i = std::slice(at, end - begin, 1);                             \
          auto j = std::slice(begin, end - begin, 1);                          \
          car(xs).as<TAG##vector>().values[i] = caddr(xs).as<TAG##vector>().values[j];   \
        };                                                                     \
                                                                               \
        switch (length(xs))                                                    \
        {                                                                      \
        case 3:                                                                \
          copy(exact_integer_cast<std::size_t>(cadr(xs)),                      \
               0,                                                              \
               caddr(xs).as<TAG##vector>().values.size());                     \
          break;                                                               \
                                                                               \
        case 4:                                                                \
          copy(exact_integer_cast<std::size_t>(cadr(xs)),                      \
               exact_integer_cast<std::size_t>(cadddr(xs)),                    \
               caddr(xs).as<TAG##vector>().values.size());                     \
          break;                                                               \
                                                                               \
        case 5:                                                                \
          copy(exact_integer_cast<std::size_t>(cadr(xs)),                      \
               exact_integer_cast<std::size_t>(cadddr(xs)),                    \
               exact_integer_cast<std::size_t>(caddddr(xs)));                  \
          break;                                                               \
                                                                               \
        default:                                                               \
          throw error(make<string>("procedure " #TAG "vector-copy! takes three to five arguments, but got"), xs); \
        }                                                                      \
      }));                                                                     \
                                                                               \
      library.define(#TAG "vector-append", make<procedure>(#TAG "vector-append", [](let const& xs) \
      {                                                                        \
        auto const& a = car(xs).as<TAG##vector>();                             \
        auto const& b = cadr(xs).as<TAG##vector>();                            \
        let const c = make<TAG##vector>(a.values.size() + b.values.size()); \
        c.as<TAG##vector>().values[std::slice(0, a.values.size(), 1)] = a.values; \
        c.as<TAG##vector>().values[std::slice(a.values.size(), b.values.size(), 1)] = b.values; \
        return c;                                                              \
      }));                                                                     \
                                                                               \
      library.define(#TAG "vector->list", make<procedure>(#TAG "vector->list", [](let const& xs) \
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
        switch (length(xs))                                                    \
        {                                                                      \
        case 1:                                                                \
          return list(car(xs).as<TAG##vector>().values,                        \
                      0,                                                       \
                      car(xs).as<TAG##vector>().values.size());                \
                                                                               \
        case 2:                                                                \
          return list(car(xs).as<TAG##vector>().values,                        \
                      exact_integer_cast<std::size_t>(cadr(xs)),               \
                      car(xs).as<TAG##vector>().values.size());                \
                                                                               \
        case 3:                                                                \
          return list(car(xs).as<TAG##vector>().values,                        \
                      exact_integer_cast<std::size_t>(cadr(xs)),               \
                      exact_integer_cast<std::size_t>(caddr(xs)));             \
                                                                               \
        default:                                                               \
          throw error(make<string>("procedure " #TAG "vector->list takes one to three arguments, but got"), xs); \
        }                                                                      \
      }));                                                                     \
                                                                               \
      library.define("list->" #TAG "vector", make<procedure>("list->" #TAG "vector", [](let const& xs) \
      {                                                                        \
        return make_homogeneous_vector_from_list_of<TAG>(car(xs));             \
      }))

      DEFINE_VECTOR(s8); DEFINE_VECTOR(s16); DEFINE_VECTOR(s32); DEFINE_VECTOR(s64);
      DEFINE_VECTOR(u8); DEFINE_VECTOR(u16); DEFINE_VECTOR(u32); DEFINE_VECTOR(u64);
                                             DEFINE_VECTOR(f32); DEFINE_VECTOR(f64);

      #undef DEFINE_VECTOR

      library.define("u8vector->string", make<procedure>("u8vector->string", [](let const& xs)
      {
        auto buffer = std::ostringstream();

        auto print = [&](auto const& x)
        {
          buffer << x;
        };

        switch (length(xs))
        {
        case 1:
          std::for_each(std::begin(car(xs).as<u8vector>().values),
                        std::end(car(xs).as<u8vector>().values),
                        print);
          break;

        case 2:
          std::for_each(std::next(std::begin(car(xs).as<u8vector>().values), exact_integer_cast<std::size_t>(cadr(xs))),
                        std::end(car(xs).as<u8vector>().values),
                        print);
          break;

        case 3:
          std::for_each(std::next(std::begin(car(xs).as<u8vector>().values), exact_integer_cast<std::size_t>(cadr(xs))),
                        std::next(std::begin(car(xs).as<u8vector>().values), exact_integer_cast<std::size_t>(caddr(xs))),
                        print);
          break;

        default:
          throw error(make<string>("procedure u8vector->string takes one to three arguments, but got"), xs);
        }

        return input_string_port(buffer.str()).get(std::numeric_limits<std::size_t>::max());
      }));

      library.define("string->u8vector", make<procedure>("string->u8vector", [](let const& xs)
      {
        auto convert = [](std::string const& s)
        {
          return make<u8vector>(reinterpret_cast<std::uint8_t const*>(s.data()), s.size());
        };

        return convert(car(xs).as<string>().utf8());
      }));
    }));

    libraries().emplace("(meevax write)", make<library>([](library & library)
    {
      library.define("put-char", make<procedure>("put-char", [](let const& xs)
      {
        cadr(xs).as<textual_output_port>().put(car(xs).as<character>());
      }));

      library.define("put-string", make<procedure>("put-string", [](let const& xs)
      {
        cadr(xs).as<textual_output_port>().put(car(xs).as<string>());
      }));

      library.define("put-u8", make<procedure>("put-u8", [](let const& xs)
      {
        cadr(xs).as<binary_output_port>().put(exact_integer_cast<std::uint8_t>(car(xs)));
      }));

      library.define("put-u8vector", make<procedure>("put-u8vector", [](let const& xs)
      {
        cadr(xs).as<binary_output_port>().put(car(xs).as<u8vector>());
      }));

      library.define("write", make<procedure>("write", [](let const& xs)
      {
        cadr(xs).as<textual_output_port>().write(car(xs));
      }));

      library.define("write-simple", make<procedure>("write-simple", [](let const& xs)
      {
        cadr(xs).as<textual_output_port>().write_simple(car(xs));
      }));
    }));
  }
} // namespace meevax::kernel
