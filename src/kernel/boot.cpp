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

#include <dlfcn.h> // dlopen, dlclose, dlerror

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
    libraries().emplace("(meevax binary32)", make<library>([](auto define)
    {
      define(make_symbol("binary32?"), make<procedure>("binary32?", [](let const& xs)
      {
        return car(xs).is<float>();
      }));

      return list(make_symbol("binary32?"));
    }));

    libraries().emplace("(meevax binary64)", make<library>([](auto define)
    {
      define(make_symbol("binary64?"), make<procedure>("binary64?", [](let const& xs)
      {
        return car(xs).is<double>();
      }));

      define(make_symbol("binary64-least"), make<double>(std::numeric_limits<double>::min()));

      define(make_symbol("binary64-greatest"), make<double>(std::numeric_limits<double>::max()));

      define(make_symbol("binary64-epsilon"),  make<double>(std::numeric_limits<double>::epsilon()));

      define(make_symbol("binary64-integral-part"), make<procedure>("binary64-integral-part", [](let const& xs)
      {
        auto integral_part = 0.0;
        std::modf(car(xs).as<double>(), &integral_part);
        return make(integral_part);
      }));

      define(make_symbol("binary64-fractional-part"), make<procedure>("binary64-fractional-part", [](let const& xs)
      {
        auto integral_part = 0.0;
        return make(std::modf(car(xs).as<double>(), &integral_part));
      }));

      define(make_symbol("binary64-log-binary"), make<procedure>("binary64-log-binary", [](let const& xs)
      {
        return make(std::logb(car(xs).as<double>()));
      }));

      define(make_symbol("binary64-integer-log-binary"), make<procedure>("binary64-integer-log-binary", [](let const& xs)
      {
        return make<small_integer>(std::ilogb(car(xs).as<double>()));
      }));

      define(make_symbol("binary64-normalized-fraction"), make<procedure>("binary64-normalized-fraction", [](let const& xs)
      {
        auto exponent = 0;
        return make(std::frexp(car(xs).as<double>(), &exponent));
      }));

      define(make_symbol("binary64-exponent"), make<procedure>("binary64-exponent", [](let const& xs)
      {
        auto exponent = 0;
        std::frexp(car(xs).as<double>(), &exponent);
        return make<small_integer>(exponent);
      }));

      define(make_symbol("binary64-sign-bit"), make<procedure>("binary64-sign-bit", [](let const& xs)
      {
        return make(std::signbit(car(xs).as<double>()));
      }));

      define(make_symbol("binary64-normalized?"), make<procedure>("binary64-normalized?", [](let const& xs)
      {
        return std::fpclassify(car(xs).as<double>()) == FP_NORMAL;
      }));

      define(make_symbol("binary64-denormalized?"), make<procedure>("binary64-denormalized?", [](let const& xs)
      {
        return std::fpclassify(car(xs).as<double>()) == FP_SUBNORMAL;
      }));

      define(make_symbol("binary64-max"), make<procedure>("binary64-max", [](let const& xs)
      {
        auto max = -std::numeric_limits<double>::infinity();

        for (let const& x : xs)
        {
          max = std::fmax(max, x.as<double>());
        }

        return make(max);
      }));

      define(make_symbol("binary64-min"), make<procedure>("binary64-min", [](let const& xs)
      {
        auto min = std::numeric_limits<double>::infinity();

        for (let const& x : xs)
        {
          min = std::fmin(min, x.as<double>());
        }

        return make(min);
      }));

      define(make_symbol("binary64-fused-multiply-add"), make<procedure>("binary64-fused-multiply-add", [](let const& xs)
      {
        return make(std::fma(car(xs).as<double>(), cadr(xs).as<double>(), caddr(xs).as<double>()));
      }));

      define(make_symbol("binary64-remquo"), make<procedure>("binary64-remquo", [](let const& xs)
      {
        auto quotient = 0;
        auto remainder = std::remquo(car(xs).as<double>(), cadr(xs).as<double>(), &quotient);
        return cons(make(remainder), make<small_integer>(quotient));
      }));

      return list(make_symbol("binary64?"),
                  make_symbol("binary64-least"),
                  make_symbol("binary64-greatest"),
                  make_symbol("binary64-epsilon"),
                  make_symbol("binary64-integral-part"),
                  make_symbol("binary64-fractional-part"),
                  make_symbol("binary64-log-binary"),
                  make_symbol("binary64-integer-log-binary"),
                  make_symbol("binary64-normalized-fraction"),
                  make_symbol("binary64-exponent"),
                  make_symbol("binary64-sign-bit"),
                  make_symbol("binary64-normalized?"),
                  make_symbol("binary64-denormalized?"),
                  make_symbol("binary64-max"),
                  make_symbol("binary64-min"),
                  make_symbol("binary64-fused-multiply-add"),
                  make_symbol("binary64-remquo"));
    }));

    libraries().emplace("(meevax boolean)", make<library>([](auto define)
    {
      define(make_symbol("boolean?"), make<procedure>("boolean?", [](let const& xs)
      {
        return car(xs).is<bool>();
      }));

      define(make_symbol("not"), make<procedure>("not", [](let const& xs)
      {
        return car(xs) == f;
      }));

      return list(make_symbol("boolean?"),
                  make_symbol("not"));
    }));

    libraries().emplace("(meevax box)", make<library>([](auto define)
    {
      define(make_symbol("box"), make<procedure>("box", [](let const& xs)
      {
        return make<box>(car(xs));
      }));

      define(make_symbol("box?"), make<procedure>("box?", [](let const& xs)
      {
        return car(xs).is<box>();
      }));

      define(make_symbol("box-ref"), make<procedure>("box-ref", [](let const& xs)
      {
        return caar(xs);
      }));

      define(make_symbol("box-set!"), make<procedure>("box-set!", [](let & xs)
      {
        caar(xs) = cadr(xs);
      }));

      return list(make_symbol("box"),
                  make_symbol("box?"),
                  make_symbol("box-ref"),
                  make_symbol("box-set!"));
    }));

    libraries().emplace("(meevax character)", make<library>([](auto define)
    {
      define(make_symbol("char?"), make<procedure>("char?", [](let const& xs)
      {
        return car(xs).is<character>();
      }));

      define(make_symbol("char=?"), make<procedure>("char=?", [](let const& xs)
      {
        return std::adjacent_find(xs.begin(), xs.end(), [](let const& a, let const& b) { return not (a.as<character>().codepoint == b.as<character>().codepoint); }) == xs.end();
      }));

      define(make_symbol("char<?"), make<procedure>("char<?", [](let const& xs)
      {
        return std::adjacent_find(xs.begin(), xs.end(), [](let const& a, let const& b) { return not (a.as<character>().codepoint < b.as<character>().codepoint); }) == xs.end();
      }));

      define(make_symbol("char>?"), make<procedure>("char>?", [](let const& xs)
      {
        return std::adjacent_find(xs.begin(), xs.end(), [](let const& a, let const& b) { return not (a.as<character>().codepoint > b.as<character>().codepoint); }) == xs.end();
      }));

      define(make_symbol("char<=?"), make<procedure>("char<=?", [](let const& xs)
      {
        return std::adjacent_find(xs.begin(), xs.end(), [](let const& a, let const& b) { return not (a.as<character>().codepoint <= b.as<character>().codepoint); }) == xs.end();
      }));

      define(make_symbol("char>=?"), make<procedure>("char>=?", [](let const& xs)
      {
        return std::adjacent_find(xs.begin(), xs.end(), [](let const& a, let const& b) { return not (a.as<character>().codepoint >= b.as<character>().codepoint); }) == xs.end();
      }));

      define(make_symbol("char-ci=?"), make<procedure>("char-ci=?", [](let const& xs)
      {
        return std::adjacent_find(xs.begin(), xs.end(), [](let const& a, let const& b) { return not (a.as<character>().downcase() == b.as<character>().downcase()); }) == xs.end();
      }));

      define(make_symbol("char-ci<?"), make<procedure>("char-ci<?", [](let const& xs)
      {
        return std::adjacent_find(xs.begin(), xs.end(), [](let const& a, let const& b) { return not (a.as<character>().downcase() < b.as<character>().downcase()); }) == xs.end();
      }));

      define(make_symbol("char-ci>?"), make<procedure>("char-ci>?", [](let const& xs)
      {
        return std::adjacent_find(xs.begin(), xs.end(), [](let const& a, let const& b) { return not (a.as<character>().downcase() > b.as<character>().downcase()); }) == xs.end();
      }));

      define(make_symbol("char-ci<=?"), make<procedure>("char-ci<=?", [](let const& xs)
      {
        return std::adjacent_find(xs.begin(), xs.end(), [](let const& a, let const& b) { return not (a.as<character>().downcase() <= b.as<character>().downcase()); }) == xs.end();
      }));

      define(make_symbol("char-ci>=?"), make<procedure>("char-ci>=?", [](let const& xs)
      {
        return std::adjacent_find(xs.begin(), xs.end(), [](let const& a, let const& b) { return not (a.as<character>().downcase() >= b.as<character>().downcase()); }) == xs.end();
      }));

      define(make_symbol("char-alphabetic?"), make<procedure>("char-alphabetic?", [](let const& xs)
      {
        return car(xs).as<character>().property().is_letter();
      }));

      define(make_symbol("char-numeric?"), make<procedure>("char-numeric?", [](let const& xs)
      {
        return car(xs).as<character>().property().is_numeric();
      }));

      define(make_symbol("char-whitespace?"), make<procedure>("char-whitespace?", [](let const& xs)
      {
        return car(xs).as<character>().property().is_whitespace();
      }));

      define(make_symbol("char-upper-case?"), make<procedure>("char-upper-case?", [](let const& xs)
      {
        return car(xs).as<character>().property().is_upper_case();
      }));

      define(make_symbol("char-lower-case?"), make<procedure>("char-lower-case?", [](let const& xs)
      {
        return car(xs).as<character>().property().is_lower_case();
      }));

      define(make_symbol("digit-value"), make<procedure>("digit-value", [](let const& xs) -> object
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

      define(make_symbol("char->integer"), make<procedure>("char->integer", [](let const& xs)
      {
        return make<small_integer>(car(xs).as<character>().codepoint);
      }));

      define(make_symbol("integer->char"), make<procedure>("integer->char", [](let const& xs)
      {
        return make<character>(exact_integer_cast<character::int_type>(car(xs)));
      }));

      define(make_symbol("char-upcase"), make<procedure>("char-upcase", [](let const& xs)
      {
        return make<character>(car(xs).as<character>().upcase());
      }));

      define(make_symbol("char-downcase"), make<procedure>("char-downcase", [](let const& xs)
      {
        return make<character>(car(xs).as<character>().downcase());
      }));

      return list(make_symbol("char?"),
                  make_symbol("char=?"),
                  make_symbol("char<?"),
                  make_symbol("char>?"),
                  make_symbol("char<=?"),
                  make_symbol("char>=?"),
                  make_symbol("char-ci=?"),
                  make_symbol("char-ci<?"),
                  make_symbol("char-ci>?"),
                  make_symbol("char-ci<=?"),
                  make_symbol("char-ci>=?"),
                  make_symbol("char-alphabetic?"),
                  make_symbol("char-numeric?"),
                  make_symbol("char-whitespace?"),
                  make_symbol("char-upper-case?"),
                  make_symbol("char-lower-case?"),
                  make_symbol("digit-value"),
                  make_symbol("char->integer"),
                  make_symbol("integer->char"),
                  make_symbol("char-upcase"),
                  make_symbol("char-downcase"));
    }));

    libraries().emplace("(meevax context)", make<library>([](auto define)
    {
      define(make_symbol("emergency-exit"), make<procedure>("emergency-exit", [](let const& xs)
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

      define(make_symbol("command-line"), make<procedure>("command-line", []()
      {
        let xs = list();

        for (auto&& each : configurator::command_line())
        {
          xs = cons(make<string>(each), xs);
        }

        return reverse(xs);
      }));

      return list(make_symbol("emergency-exit"),
                  make_symbol("command-line"));
    }));

    libraries().emplace("(meevax comparator)", make<library>([](auto define)
    {
      define(make_symbol("eq?"), make<procedure>("eq?", [](let const& xs)
      {
        return eq(car(xs), cadr(xs));
      }));

      define(make_symbol("eqv?"), make<procedure>("eqv?", [](let const& xs)
      {
        return eqv(car(xs), cadr(xs));
      }));

      define(make_symbol("equal?"), make<procedure>("equal?", [](let const& xs)
      {
        return equal(car(xs), cadr(xs));
      }));

      return list(make_symbol("eq?"),
                  make_symbol("eqv?"),
                  make_symbol("equal?"));
    }));

    libraries().emplace("(meevax core)", make<library>([](auto define)
    {
      for (let const& binding : core_syntactic_environment().as<syntactic_environment>().second)
      {
        define(car(binding), cdr(binding));
      }

      return map(car, core_syntactic_environment().as<syntactic_environment>().second);
    }));

    libraries().emplace("(meevax environment)", make<library>([](auto define)
    {
      define(make_symbol("environment"), make<procedure>("environment", [](let const& xs)
      {
        auto e = environment();

        for (let const& x : xs)
        {
          e.import(x);
        }

        return make(e);
      }));

      define(make_symbol("eval"), make<procedure>("eval", [](let const& xs)
      {
        return cadr(xs).as<environment>().evaluate(car(xs));
      }));

      define(make_symbol("expand"), make<procedure>("expand", [](let const& xs)
      {
        return cadr(xs).as<environment>().expand(car(xs), unit);
      }));

      define(make_symbol("interaction-environment"), make<procedure>("interaction-environment", []()
      {
        return interaction_environment();
      }));

      define(make_symbol("load"), make<procedure>("load", [](let const& xs)
      {
        return car(xs).as<environment>().load(cadr(xs).as<string>().utf8());
      }));

      return list(make_symbol("environment"),
                  make_symbol("eval"),
                  make_symbol("expand"),
                  make_symbol("interaction-environment"),
                  make_symbol("load"));
    }));

    libraries().emplace("(meevax error)", make<library>([](auto define)
    {
      define(make_symbol("throw"), make<procedure>("throw", [](let const& xs)
      {
        throw car(xs);
      }));

      define(make_symbol("error-object"), make<procedure>("error-object", [](let const& xs)
      {
        return make<error>(car(xs), cdr(xs));
      }));

      define(make_symbol("error-object?"), make<procedure>("error-object?", [](let const& xs)
      {
        return car(xs).is_also<error>();
      }));

      define(make_symbol("read-error?"), make<procedure>("read-error?", [](let const& xs)
      {
        return car(xs).is<read_error>();
      }));

      define(make_symbol("file-error?"), make<procedure>("file-error?", [](let const& xs)
      {
        return car(xs).is<file_error>();
      }));

      define(make_symbol("kernel-exception-handler-set!"), make<procedure>("kernel-exception-handler-set!", [](let const& xs)
      {
        environment::exception_handler = car(xs);
      }));

      return list(make_symbol("throw"),
                  make_symbol("error-object"),
                  make_symbol("error-object?"),
                  make_symbol("read-error?"),
                  make_symbol("file-error?"),
                  make_symbol("kernel-exception-handler-set!"));
    }));

    libraries().emplace("(meevax file)", make<library>([](auto define)
    {
      define(make_symbol("file-exists?"), make<procedure>("file-exists?", [](let const& xs)
      {
        return std::filesystem::exists(car(xs).as<string>().utf8());
      }));

      define(make_symbol("delete-file"), make<procedure>("delete-file", [](let const& xs)
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

      define(make_symbol("library-directories"), make<procedure>("library-directories", [](let const&)
      {
        let directories = unit;

        for (auto iterator = configurator::directories().rbegin(); iterator != configurator::directories().rend(); ++iterator)
        {
          directories = cons(make<string>(iterator->native()), directories);
        }

        return directories;
      }));

      return list(make_symbol("file-exists?"),
                  make_symbol("delete-file"),
                  make_symbol("library-directories"));
    }));

    libraries().emplace("(meevax instruction)", make<library>([](auto define)
    {
      define(make_symbol("secd-call"),              make<instruction>(instruction::secd_call             ));
      define(make_symbol("secd-cons"),              make<instruction>(instruction::secd_cons             ));
      define(make_symbol("secd-current"),           make<instruction>(instruction::secd_current          ));
      define(make_symbol("secd-drop"),              make<instruction>(instruction::secd_drop             ));
      define(make_symbol("secd-dummy"),             make<instruction>(instruction::secd_dummy            ));
      define(make_symbol("secd-install"),           make<instruction>(instruction::secd_install          ));
      define(make_symbol("secd-join"),              make<instruction>(instruction::secd_join             ));
      define(make_symbol("secd-letrec"),            make<instruction>(instruction::secd_letrec           ));
      define(make_symbol("secd-load-absolute"),     make<instruction>(instruction::secd_load_absolute    ));
      define(make_symbol("secd-load-closure"),      make<instruction>(instruction::secd_load_closure     ));
      define(make_symbol("secd-load-constant"),     make<instruction>(instruction::secd_load_constant    ));
      define(make_symbol("secd-load-continuation"), make<instruction>(instruction::secd_load_continuation));
      define(make_symbol("secd-load-relative"),     make<instruction>(instruction::secd_load_relative    ));
      define(make_symbol("secd-load-variadic"),     make<instruction>(instruction::secd_load_variadic    ));
      define(make_symbol("secd-return"),            make<instruction>(instruction::secd_return           ));
      define(make_symbol("secd-select"),            make<instruction>(instruction::secd_select           ));
      define(make_symbol("secd-stop"),              make<instruction>(instruction::secd_stop             ));
      define(make_symbol("secd-store-absolute"),    make<instruction>(instruction::secd_store_absolute   ));
      define(make_symbol("secd-store-relative"),    make<instruction>(instruction::secd_store_relative   ));
      define(make_symbol("secd-store-variadic"),    make<instruction>(instruction::secd_store_variadic   ));
      define(make_symbol("secd-tail-call"),         make<instruction>(instruction::secd_tail_call        ));
      define(make_symbol("secd-tail-letrec"),       make<instruction>(instruction::secd_tail_letrec      ));
      define(make_symbol("secd-tail-select"),       make<instruction>(instruction::secd_tail_select      ));

      return list(make_symbol("secd-call"),
                  make_symbol("secd-cons"),
                  make_symbol("secd-current"),
                  make_symbol("secd-drop"),
                  make_symbol("secd-dummy"),
                  make_symbol("secd-install"),
                  make_symbol("secd-join"),
                  make_symbol("secd-letrec"),
                  make_symbol("secd-load-absolute"),
                  make_symbol("secd-load-closure"),
                  make_symbol("secd-load-constant"),
                  make_symbol("secd-load-continuation"),
                  make_symbol("secd-load-relative"),
                  make_symbol("secd-load-variadic"),
                  make_symbol("secd-return"),
                  make_symbol("secd-select"),
                  make_symbol("secd-stop"),
                  make_symbol("secd-store-absolute"),
                  make_symbol("secd-store-relative"),
                  make_symbol("secd-store-variadic"),
                  make_symbol("secd-tail-call"),
                  make_symbol("secd-tail-letrec"),
                  make_symbol("secd-tail-select"));
    }));

    libraries().emplace("(meevax integer32)", make<library>([](auto define)
    {
      define(make_symbol("integer32?"), make<procedure>("integer32?", [](let const& xs)
      {
        return car(xs).is<small_integer>();
      }));

      define(make_symbol("integer32-width"), make<small_integer>(32));

      define(make_symbol("integer32-min"), make<small_integer>(std::numeric_limits<small_integer>::min()));

      define(make_symbol("integer32-max"), make<small_integer>(std::numeric_limits<small_integer>::max()));

      return list(make_symbol("integer32?"),
                  make_symbol("integer32-width"),
                  make_symbol("integer32-min"),
                  make_symbol("integer32-max"));
    }));

    libraries().emplace("(meevax list)", make<library>([](auto define)
    {
      define(make_symbol("null?"), make<procedure>("null?", [](let const& xs)
      {
        return car(xs).is<null>();
      }));

      define(make_symbol("list?"), make<procedure>("list?", [](let const& xs)
      {
        return is_list(car(xs));
      }));

      define(make_symbol("list"), make<procedure>("list", [](let const& xs)
      {
        return xs;
      }));

      define(make_symbol("make-list"), make<procedure>("make-list", [](let const& xs)
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

      define(make_symbol("iota"), make<procedure>("iota", [](let const& xs)
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

      define(make_symbol("circular-list?"), make<procedure>("circular-list?", [](let const& xs)
      {
        return is_circular_list(car(xs));
      }));

      define(make_symbol("circular-list"), make<procedure>("circular-list", [](let & xs)
      {
        circulate(xs);
        return xs;
      }));

      define(make_symbol("dotted-list?"), make<procedure>("dotted-list?", [](let const& xs)
      {
        return is_dotted_list(car(xs));
      }));

      define(make_symbol("null-list?"), make<procedure>("null-list?", [](let const& xs)
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

      define(make_symbol("last"), make<procedure>("last", [](let const& xs)
      {
        return last(car(xs));
      }));

      define(make_symbol("last-pair"), make<procedure>("last-pair", [](let const& xs)
      {
        return last_pair(car(xs));
      }));

      define(make_symbol("length"), make<procedure>("length", [](let const& xs)
      {
        return make(static_cast<small_integer>(length(car(xs))));
      }));

      define(make_symbol("length+"), make<procedure>("length+", [](let const& xs) -> object
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

      define(make_symbol("append"), make<procedure>("append", [](let const& xs)
      {
        return std::accumulate(xs.begin(), xs.end(), unit, [](let const& x, let const& y) { return append(x, y); });
      }));

      define(make_symbol("append!"), make<procedure>("append!", [](let & xs)
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

      define(make_symbol("append-reverse"), make<procedure>("append-reverse", [](let const& xs)
      {
        return append_reverse(car(xs), cadr(xs));
      }));

      define(make_symbol("append-reverse!"), make<procedure>("append-reverse!", [](let & xs)
      {
        return append_reverse(car(xs), cadr(xs));
      }));

      define(make_symbol("reverse"), make<procedure>("reverse", [](let const& xs)
      {
        return reverse(car(xs));
      }));

      define(make_symbol("reverse!"), make<procedure>("reverse!", [](let & xs)
      {
        return reverse(car(xs));
      }));

      define(make_symbol("concatenate"), make<procedure>("concatenate", [](let const& xs)
      {
        return std::accumulate(car(xs).begin(), car(xs).end(), unit, [](let const& x, let const& y) { return append(x, y); });
      }));

      define(make_symbol("concatenate!"), make<procedure>("concatenate!", [](let & xs)
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

      define(make_symbol("list-copy"), make<procedure>("list-copy", [](let const& xs)
      {
        return list_copy(car(xs));
      }));

      define(make_symbol("list-ref"), make<procedure>("list-ref", [](let const& xs)
      {
        return head(car(xs), exact_integer_cast<std::size_t>(cadr(xs)));
      }));

      define(make_symbol("list-tail"), make<procedure>("list-tail", [](let const& xs)
      {
        return tail(car(xs), exact_integer_cast<std::size_t>(cadr(xs)));
      }));

      define(make_symbol("first"), make<procedure>("first", [](let const& xs)
      {
        return head(car(xs), 0);
      }));

      define(make_symbol("second"), make<procedure>("second", [](let const& xs)
      {
        return head(car(xs), 1);
      }));

      define(make_symbol("third"), make<procedure>("third", [](let const& xs)
      {
        return head(car(xs), 2);
      }));

      define(make_symbol("fourth"), make<procedure>("fourth", [](let const& xs)
      {
        return head(car(xs), 3);
      }));

      define(make_symbol("fifth"), make<procedure>("fifth", [](let const& xs)
      {
        return head(car(xs), 4);
      }));

      define(make_symbol("sixth"), make<procedure>("sixth", [](let const& xs)
      {
        return head(car(xs), 5);
      }));

      define(make_symbol("seventh"), make<procedure>("seventh", [](let const& xs)
      {
        return head(car(xs), 6);
      }));

      define(make_symbol("eighth"), make<procedure>("eighth", [](let const& xs)
      {
        return head(car(xs), 7);
      }));

      define(make_symbol("ninth"), make<procedure>("ninth", [](let const& xs)
      {
        return head(car(xs), 8);
      }));

      define(make_symbol("tenth"), make<procedure>("tenth", [](let const& xs)
      {
        return head(car(xs), 9);
      }));

      define(make_symbol("take"), make<procedure>("take", [](let const& xs)
      {
        return take(car(xs), exact_integer_cast<std::size_t>(cadr(xs)));
      }));

      define(make_symbol("take!"), make<procedure>("take!", [](let & xs)
      {
        return take(car(xs), exact_integer_cast<std::size_t>(cadr(xs)));
      }));

      define(make_symbol("take-right"), make<procedure>("take-right", [](let const& xs)
      {
        return take_right(car(xs), exact_integer_cast<std::size_t>(cadr(xs)));
      }));

      define(make_symbol("drop"), make<procedure>("drop", [](let const& xs)
      {
        return drop(car(xs), exact_integer_cast<std::size_t>(cadr(xs)));
      }));

      define(make_symbol("drop-right"), make<procedure>("drop-right", [](let const& xs)
      {
        return drop_right(car(xs), exact_integer_cast<std::size_t>(cadr(xs)));
      }));

      define(make_symbol("drop-right!"), make<procedure>("drop-right!", [](let & xs)
      {
        return drop_right(car(xs), exact_integer_cast<std::size_t>(cadr(xs)));
      }));

      define(make_symbol("memq"), make<procedure>("memq", [](let const& xs)
      {
        return memq(car(xs), cadr(xs));
      }));

      define(make_symbol("memv"), make<procedure>("memv", [](let const& xs)
      {
        return memv(car(xs), cadr(xs));
      }));

      define(make_symbol("assq"), make<procedure>("assq", [](let const& xs)
      {
        return assq(car(xs), cadr(xs));
      }));

      define(make_symbol("assv"), make<procedure>("assv", [](let const& xs)
      {
        return assv(car(xs), cadr(xs));
      }));

      define(make_symbol("alist-cons"), make<procedure>("alist-cons", [](let const& xs)
      {
        return alist_cons(car(xs), cadr(xs), caddr(xs));
      }));

      define(make_symbol("alist-copy"), make<procedure>("alist-copy", [](let const& xs)
      {
        return alist_copy(car(xs));
      }));

      return list(make_symbol("null?"),
                  make_symbol("list?"),
                  make_symbol("list"),
                  make_symbol("make-list"),
                  make_symbol("iota"),
                  make_symbol("circular-list?"),
                  make_symbol("circular-list"),
                  make_symbol("dotted-list?"),
                  make_symbol("null-list?"),
                  make_symbol("last"),
                  make_symbol("last-pair"),
                  make_symbol("length"),
                  make_symbol("length+"),
                  make_symbol("append"),
                  make_symbol("append!"),
                  make_symbol("append-reverse"),
                  make_symbol("append-reverse!"),
                  make_symbol("reverse"),
                  make_symbol("reverse!"),
                  make_symbol("concatenate"),
                  make_symbol("concatenate!"),
                  make_symbol("list-copy"),
                  make_symbol("list-ref"),
                  make_symbol("list-tail"),
                  make_symbol("first"),
                  make_symbol("second"),
                  make_symbol("third"),
                  make_symbol("fourth"),
                  make_symbol("fifth"),
                  make_symbol("sixth"),
                  make_symbol("seventh"),
                  make_symbol("eighth"),
                  make_symbol("ninth"),
                  make_symbol("tenth"),
                  make_symbol("take"),
                  make_symbol("take!"),
                  make_symbol("take-right"),
                  make_symbol("drop"),
                  make_symbol("drop-right"),
                  make_symbol("drop-right!"),
                  make_symbol("memq"),
                  make_symbol("memv"),
                  make_symbol("assq"),
                  make_symbol("assv"),
                  make_symbol("alist-cons"),
                  make_symbol("alist-copy"));
    }));

    libraries().emplace("(meevax number)", make<library>([](auto define)
    {
      using namespace number;

      define(make_symbol("number?"), make<procedure>("number?", [](let const& xs)
      {
        return is_complex(car(xs));
      }));

      define(make_symbol("complex?"), make<procedure>("complex?", [](let const& xs)
      {
        return is_complex(car(xs));
      }));

      define(make_symbol("real?"), make<procedure>("real?", [](let const& xs)
      {
        return is_real(car(xs));
      }));

      define(make_symbol("rational?"), make<procedure>("rational?", [](let const& xs)
      {
        return is_rational(car(xs));
      }));

      define(make_symbol("integer?"), make<procedure>("integer?", [](let const& xs)
      {
        return is_integer(car(xs));
      }));

      define(make_symbol("exact?"), make<procedure>("exact?", [](let const& xs)
      {
        return is_exact(car(xs));
      }));

      define(make_symbol("inexact?"), make<procedure>("inexact?", [](let const& xs)
      {
        return is_inexact(car(xs));
      }));

      define(make_symbol("exact-integer?"), make<procedure>("exact-integer?", [](let const& xs)
      {
        return is_exact_integer(car(xs));
      }));

      define(make_symbol("finite?"), make<procedure>("finite?", [](let const& xs)
      {
        return is_finite(car(xs));
      }));

      define(make_symbol("infinite?"), make<procedure>("infinite?", [](let const& xs)
      {
        return is_infinite(car(xs));
      }));

      define(make_symbol("nan?"), make<procedure>("nan?", [](let const& xs)
      {
        return is_nan(car(xs));
      }));

      define(make_symbol("="), make<procedure>("=", [](let const& xs)
      {
        return std::adjacent_find(xs.begin(), xs.end(), not_equals) == xs.end();
      }));

      define(make_symbol("<"), make<procedure>("<", [](let const& xs)
      {
        return std::adjacent_find(xs.begin(), xs.end(), greater_than_or_equals) == xs.end();
      }));

      define(make_symbol("<="), make<procedure>("<=", [](let const& xs)
      {
        return std::adjacent_find(xs.begin(), xs.end(), greater_than) == xs.end();
      }));

      define(make_symbol(">"), make<procedure>(">", [](let const& xs)
      {
        return std::adjacent_find(xs.begin(), xs.end(), less_than_or_equals) == xs.end();
      }));

      define(make_symbol(">="), make<procedure>(">=", [](let const& xs)
      {
        return std::adjacent_find(xs.begin(), xs.end(), less_than) == xs.end();
      }));

      define(make_symbol("zero?"), make<procedure>("zero?", [](let const& xs)
      {
        return is_zero(car(xs));
      }));

      define(make_symbol("positive?"), make<procedure>("positive?", [](let const& xs)
      {
        return is_positive(car(xs));
      }));

      define(make_symbol("negative?"), make<procedure>("negative?", [](let const& xs)
      {
        return is_negative(car(xs));
      }));

      define(make_symbol("odd?"), make<procedure>("odd?", [](let const& xs)
      {
        return is_odd(car(xs));
      }));

      define(make_symbol("even?"), make<procedure>("even?", [](let const& xs)
      {
        return is_even(car(xs));
      }));

      define(make_symbol("max"), make<procedure>("max", [](let const& xs)
      {
        if (auto iter = std::max_element(xs.begin(), xs.end(), less_than); iter != xs.end())
        {
          return std::any_of(xs.begin(), xs.end(), is_inexact) ? inexact(*iter) : *iter;
        }
        else
        {
          return unspecified;
        }
      }));

      define(make_symbol("min"), make<procedure>("min", [](let const& xs)
      {
        if (auto iter = std::min_element(xs.begin(), xs.end(), less_than); iter != xs.end())
        {
          return std::any_of(xs.begin(), xs.end(), is_inexact) ? inexact(*iter) : *iter;
        }
        else
        {
          return unspecified;
        }
      }));

      define(make_symbol("+"), make<procedure>("+", [](let const& xs)
      {
        return std::accumulate(xs.begin(), xs.end(), e0, std::plus());
      }));

      define(make_symbol("*"), make<procedure>("*", [](let const& xs)
      {
        return std::accumulate(xs.begin(), xs.end(), e1, std::multiplies());
      }));

      define(make_symbol("-"), make<procedure>("-", [](let const& xs)
      {
        if (cdr(xs).is<pair>())
        {
          return std::accumulate(std::next(xs.begin()), xs.end(), car(xs), std::minus());
        }
        else
        {
          return e0 - car(xs);
        }
      }));

      define(make_symbol("/"), make<procedure>("/", [](let const& xs)
      {
        if (cdr(xs).is<pair>())
        {
          return std::accumulate(std::next(xs.begin()), xs.end(), car(xs), std::divides());
        }
        else
        {
          return e1 / car(xs);
        }
      }));

      define(make_symbol("abs"), make<procedure>("abs", [](let const& xs)
      {
        return abs(car(xs));
      }));

      define(make_symbol("quotient"), make<procedure>("quotient", [](let const& xs)
      {
        return quotient(car(xs), cadr(xs));
      }));

      define(make_symbol("remainder"), make<procedure>("remainder", [](let const& xs)
      {
        return remainder(car(xs), cadr(xs));
      }));

      define(make_symbol("modulo"), make<procedure>("modulo", [](let const& xs)
      {
        return modulo(car(xs), cadr(xs));
      }));

      define(make_symbol("gcd"), make<procedure>("gcd", [](let const& xs)
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
      }));

      define(make_symbol("lcm"), make<procedure>("lcm", [](let const& xs)
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
      }));

      define(make_symbol("numerator"), make<procedure>("numerator", [](let const& xs)
      {
        return numerator(car(xs));
      }));

      define(make_symbol("denominator"), make<procedure>("denominator", [](let const& xs)
      {
        return denominator(car(xs));
      }));

      define(make_symbol("floor"), make<procedure>("floor", [](let const& xs)
      {
        return floor(car(xs));
      }));

      define(make_symbol("ceiling"), make<procedure>("ceiling", [](let const& xs)
      {
        return ceiling(car(xs));
      }));

      define(make_symbol("truncate"), make<procedure>("truncate", [](let const& xs)
      {
        return truncate(car(xs));
      }));

      define(make_symbol("round"), make<procedure>("round", [](let const& xs)
      {
        return round(car(xs));
      }));

      define(make_symbol("exp"), make<procedure>("exp", [](let const& xs)
      {
        return exp(car(xs));
      }));

      define(make_symbol("log"), make<procedure>("log", [](let const& xs)
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

      define(make_symbol("sin"), make<procedure>("sin", [](let const& xs)
      {
        return sin(car(xs));
      }));

      define(make_symbol("cos"), make<procedure>("cos", [](let const& xs)
      {
        return cos(car(xs));
      }));

      define(make_symbol("tan"), make<procedure>("tan", [](let const& xs)
      {
        return tan(car(xs));
      }));

      define(make_symbol("asin"), make<procedure>("asin", [](let const& xs)
      {
        return asin(car(xs));
      }));

      define(make_symbol("acos"), make<procedure>("acos", [](let const& xs)
      {
        return acos(car(xs));
      }));

      define(make_symbol("atan"), make<procedure>("atan", [](let const& xs)
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

      define(make_symbol("sinh"), make<procedure>("sinh", [](let const& xs)
      {
        return sinh(car(xs));
      }));

      define(make_symbol("cosh"), make<procedure>("cosh", [](let const& xs)
      {
        return cosh(car(xs));
      }));

      define(make_symbol("tanh"), make<procedure>("tanh", [](let const& xs)
      {
        return tanh(car(xs));
      }));

      define(make_symbol("asinh"), make<procedure>("asinh", [](let const& xs)
      {
        return asinh(car(xs));
      }));

      define(make_symbol("acosh"), make<procedure>("acosh", [](let const& xs)
      {
        return acosh(car(xs));
      }));

      define(make_symbol("atanh"), make<procedure>("atanh", [](let const& xs)
      {
        return atanh(car(xs));
      }));

      define(make_symbol("sqrt"), make<procedure>("sqrt", [](let const& xs)
      {
        return sqrt(car(xs));
      }));

      define(make_symbol("exact-integer-square-root"), make<procedure>("exact-integer-square-root", [](let const& xs)
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

      define(make_symbol("expt"), make<procedure>("expt", [](let const& xs)
      {
        return pow(car(xs), cadr(xs));
      }));

      define(make_symbol("make-rectangular"), make<procedure>("make-rectangular", [](let const& xs)
      {
        return make<complex>(car(xs), cadr(xs));
      }));

      define(make_symbol("make-polar"), make<procedure>("make-polar", [](let const& xs)
      {
        let const& radius = car(xs), angle = cadr(xs);

        return make<complex>(radius * cos(angle),
                             radius * sin(angle));
      }));

      define(make_symbol("real-part"), make<procedure>("real-part", [](let const& xs)
      {
        return real(car(xs));
      }));

      define(make_symbol("imag-part"), make<procedure>("imag-part", [](let const& xs)
      {
        return imag(car(xs));
      }));

      define(make_symbol("magnitude"), make<procedure>("magnitude", [](let const& xs)
      {
        return magnitude(car(xs));
      }));

      define(make_symbol("angle"), make<procedure>("angle", [](let const& xs)
      {
        return angle(car(xs));
      }));

      define(make_symbol("exact"), make<procedure>("exact", [](let const& xs)
      {
        return exact(car(xs));
      }));

      define(make_symbol("inexact"), make<procedure>("inexact", [](let const& xs)
      {
        return inexact(car(xs));
      }));

      define(make_symbol("expm1"), make<procedure>("expm1", [](let const& xs)
      {
        return expm1(car(xs));
      }));

      define(make_symbol("log1p"), make<procedure>("log1p", [](let const& xs)
      {
        return log1p(car(xs));
      }));

      define(make_symbol("erf"), make<procedure>("erf", [](let const& xs)
      {
        return erf(car(xs));
      }));

      define(make_symbol("erfc"), make<procedure>("erfc", [](let const& xs)
      {
        return erfc(car(xs));
      }));

      define(make_symbol("tgamma"), make<procedure>("tgamma", [](let const& xs)
      {
        return tgamma(car(xs));
      }));

      define(make_symbol("lgamma"), make<procedure>("lgamma", [](let const& xs)
      {
        return lgamma(car(xs));
      }));

      define(make_symbol("ldexp"), make<procedure>("ldexp", [](let const& xs)
      {
        return ldexp(car(xs), cadr(xs));
      }));

      define(make_symbol("nextafter"), make<procedure>("nextafter", [](let const& xs)
      {
        return nextafter(car(xs), cadr(xs));
      }));

      define(make_symbol("copysign"), make<procedure>("copysign", [](let const& xs)
      {
        return copysign(car(xs), cadr(xs));
      }));

      define(make_symbol("cyl_bessel_j"), make<procedure>("cyl_bessel_j", [](let const& xs)
      {
        return cyl_bessel_j(car(xs), cadr(xs));
      }));

      define(make_symbol("cyl_neumann"), make<procedure>("cyl_neumann", [](let const& xs)
      {
        return cyl_neumann(car(xs), cadr(xs));
      }));

      define(make_symbol("e"), make<double>(std::numbers::e));

      define(make_symbol("pi"), make<double>(std::numbers::pi));

      define(make_symbol("euler"), make<double>(std::numbers::egamma));

      define(make_symbol("phi"), make<double>(std::numbers::phi));

      define(make_symbol("bitwise-not"), make<procedure>("bitwise-not", [](let const& xs)
      {
        return bitwise_not(car(xs));
      }));

      define(make_symbol("bitwise-and"), make<procedure>("bitwise-and", [](let const& xs)
      {
        return std::accumulate(xs.begin(), xs.end(), make<small_integer>(-1), bitwise_and);
      }));

      define(make_symbol("bitwise-ior"), make<procedure>("bitwise-ior", [](let const& xs)
      {
        return std::accumulate(xs.begin(), xs.end(), make<small_integer>(0), bitwise_ior);
      }));

      define(make_symbol("bitwise-xor"), make<procedure>("bitwise-xor", [](let const& xs)
      {
        return std::accumulate(xs.begin(), xs.end(), make<small_integer>(0), bitwise_xor);
      }));

      define(make_symbol("bit-shift"), make<procedure>("bit-shift", [](let const& xs)
      {
        return bit_shift(car(xs), exact_integer_cast<small_integer>(cadr(xs)));
      }));

      define(make_symbol("bit-count"), make<procedure>("bit-count", [](let const& xs)
      {
        return bit_count(car(xs));
      }));

      define(make_symbol("bit-width"), make<procedure>("bit-width", [](let const& xs)
      {
        return bit_width(car(xs));
      }));

      define(make_symbol("number->string"), make<procedure>("number->string", [](let const& xs)
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

      define(make_symbol("string->number"), make<procedure>("string->number", [](let const& xs)
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

      return list(make_symbol("number?"),
                  make_symbol("complex?"),
                  make_symbol("real?"),
                  make_symbol("rational?"),
                  make_symbol("integer?"),
                  make_symbol("exact?"),
                  make_symbol("inexact?"),
                  make_symbol("exact-integer?"),
                  make_symbol("finite?"),
                  make_symbol("infinite?"),
                  make_symbol("nan?"),
                  make_symbol("="),
                  make_symbol("<"),
                  make_symbol("<="),
                  make_symbol(">"),
                  make_symbol(">="),
                  make_symbol("zero?"),
                  make_symbol("positive?"),
                  make_symbol("negative?"),
                  make_symbol("odd?"),
                  make_symbol("even?"),
                  make_symbol("max"),
                  make_symbol("min"),
                  make_symbol("+"),
                  make_symbol("*"),
                  make_symbol("-"),
                  make_symbol("/"),
                  make_symbol("abs"),
                  make_symbol("quotient"),
                  make_symbol("remainder"),
                  make_symbol("modulo"),
                  make_symbol("gcd"),
                  make_symbol("lcm"),
                  make_symbol("numerator"),
                  make_symbol("denominator"),
                  make_symbol("floor"),
                  make_symbol("ceiling"),
                  make_symbol("truncate"),
                  make_symbol("round"),
                  make_symbol("exp"),
                  make_symbol("log"),
                  make_symbol("sin"),
                  make_symbol("cos"),
                  make_symbol("tan"),
                  make_symbol("asin"),
                  make_symbol("acos"),
                  make_symbol("atan"),
                  make_symbol("sinh"),
                  make_symbol("cosh"),
                  make_symbol("tanh"),
                  make_symbol("asinh"),
                  make_symbol("acosh"),
                  make_symbol("atanh"),
                  make_symbol("sqrt"),
                  make_symbol("exact-integer-square-root"),
                  make_symbol("expt"),
                  make_symbol("make-rectangular"),
                  make_symbol("make-polar"),
                  make_symbol("real-part"),
                  make_symbol("imag-part"),
                  make_symbol("magnitude"),
                  make_symbol("angle"),
                  make_symbol("exact"),
                  make_symbol("inexact"),
                  make_symbol("expm1"),
                  make_symbol("log1p"),
                  make_symbol("erf"),
                  make_symbol("erfc"),
                  make_symbol("tgamma"),
                  make_symbol("lgamma"),
                  make_symbol("ldexp"),
                  make_symbol("nextafter"),
                  make_symbol("copysign"),
                  make_symbol("cyl_bessel_j"),
                  make_symbol("cyl_neumann"),
                  make_symbol("e"),
                  make_symbol("pi"),
                  make_symbol("euler"),
                  make_symbol("phi"),
                  make_symbol("bitwise-not"),
                  make_symbol("bitwise-and"),
                  make_symbol("bitwise-ior"),
                  make_symbol("bitwise-xor"),
                  make_symbol("bit-shift"),
                  make_symbol("bit-count"),
                  make_symbol("bit-width"),
                  make_symbol("number->string"),
                  make_symbol("string->number"));
    }));

    libraries().emplace("(meevax pair)", make<library>([](auto define)
    {
      define(make_symbol("pair?"), make<procedure>("pair?", [](let const& xs)
      {
        return car(xs).is<pair>();
      }));

      define(make_symbol("not-pair?"), make<procedure>("not-pair?", [](let const& xs)
      {
        return not car(xs).is<pair>();
      }));

      define(make_symbol("cons"), make<procedure>("cons", [](let const& xs)
      {
        return cons(car(xs), cadr(xs));
      }));

      define(make_symbol("xcons"), make<procedure>("xcons", [](let const& xs)
      {
        return cons(cadr(xs), car(xs));
      }));

      define(make_symbol("cons*"), make<procedure>("cons*", [](let & xs)
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

      define(make_symbol("car"), make<procedure>("car", [](let const& xs)
      {
        return car(car(xs));
      }));

      define(make_symbol("cdr"), make<procedure>("cdr", [](let const& xs)
      {
        return cdr(car(xs));
      }));

      define(make_symbol("caar"), make<procedure>("caar", [](let const& xs)
      {
        return caar(car(xs));
      }));

      define(make_symbol("cadr"), make<procedure>("cadr", [](let const& xs)
      {
        return cadr(car(xs));
      }));

      define(make_symbol("cdar"), make<procedure>("cdar", [](let const& xs)
      {
        return cdar(car(xs));
      }));

      define(make_symbol("cddr"), make<procedure>("cddr", [](let const& xs)
      {
        return cddr(car(xs));
      }));

      define(make_symbol("caaar"), make<procedure>("caaar", [](let const& xs)
      {
        return caaar(car(xs));
      }));

      define(make_symbol("cdaar"), make<procedure>("cdaar", [](let const& xs)
      {
        return cdaar(car(xs));
      }));

      define(make_symbol("caadr"), make<procedure>("caadr", [](let const& xs)
      {
        return caadr(car(xs));
      }));

      define(make_symbol("cdadr"), make<procedure>("cdadr", [](let const& xs)
      {
        return cdadr(car(xs));
      }));

      define(make_symbol("cadar"), make<procedure>("cadar", [](let const& xs)
      {
        return cadar(car(xs));
      }));

      define(make_symbol("cddar"), make<procedure>("cddar", [](let const& xs)
      {
        return cddar(car(xs));
      }));

      define(make_symbol("caddr"), make<procedure>("caddr", [](let const& xs)
      {
        return caddr(car(xs));
      }));

      define(make_symbol("cdddr"), make<procedure>("cdddr", [](let const& xs)
      {
        return cdddr(car(xs));
      }));

      define(make_symbol("caaaar"), make<procedure>("caaaar", [](let const& xs)
      {
        return caaaar(car(xs));
      }));

      define(make_symbol("cdaaar"), make<procedure>("cdaaar", [](let const& xs)
      {
        return cdaaar(car(xs));
      }));

      define(make_symbol("caaadr"), make<procedure>("caaadr", [](let const& xs)
      {
        return caaadr(car(xs));
      }));

      define(make_symbol("cdaadr"), make<procedure>("cdaadr", [](let const& xs)
      {
        return cdaadr(car(xs));
      }));

      define(make_symbol("caadar"), make<procedure>("caadar", [](let const& xs)
      {
        return caadar(car(xs));
      }));

      define(make_symbol("cdadar"), make<procedure>("cdadar", [](let const& xs)
      {
        return cdadar(car(xs));
      }));

      define(make_symbol("caaddr"), make<procedure>("caaddr", [](let const& xs)
      {
        return caaddr(car(xs));
      }));

      define(make_symbol("cdaddr"), make<procedure>("cdaddr", [](let const& xs)
      {
        return cdaddr(car(xs));
      }));

      define(make_symbol("cadaar"), make<procedure>("cadaar", [](let const& xs)
      {
        return cadaar(car(xs));
      }));

      define(make_symbol("cddaar"), make<procedure>("cddaar", [](let const& xs)
      {
        return cddaar(car(xs));
      }));

      define(make_symbol("cadadr"), make<procedure>("cadadr", [](let const& xs)
      {
        return cadadr(car(xs));
      }));

      define(make_symbol("cddadr"), make<procedure>("cddadr", [](let const& xs)
      {
        return cddadr(car(xs));
      }));

      define(make_symbol("caddar"), make<procedure>("caddar", [](let const& xs)
      {
        return caddar(car(xs));
      }));

      define(make_symbol("cdddar"), make<procedure>("cdddar", [](let const& xs)
      {
        return cdddar(car(xs));
      }));

      define(make_symbol("cadddr"), make<procedure>("cadddr", [](let const& xs)
      {
        return cadddr(car(xs));
      }));

      define(make_symbol("cddddr"), make<procedure>("cddddr", [](let const& xs)
      {
        return cddddr(car(xs));
      }));

      define(make_symbol("set-car!"), make<procedure>("set-car!", [](let & xs)
      {
        caar(xs) = cadr(xs);
      }));

      define(make_symbol("set-cdr!"), make<procedure>("set-cdr!", [](let & xs)
      {
        cdar(xs) = cadr(xs);
      }));

      return list(make_symbol("pair?"),
                  make_symbol("not-pair?"),
                  make_symbol("cons"),
                  make_symbol("xcons"),
                  make_symbol("cons*"),
                  make_symbol("car"),
                  make_symbol("cdr"),
                  make_symbol("caar"),
                  make_symbol("cadr"),
                  make_symbol("cdar"),
                  make_symbol("cddr"),
                  make_symbol("caaar"),
                  make_symbol("cdaar"),
                  make_symbol("caadr"),
                  make_symbol("cdadr"),
                  make_symbol("cadar"),
                  make_symbol("cddar"),
                  make_symbol("caddr"),
                  make_symbol("cdddr"),
                  make_symbol("caaaar"),
                  make_symbol("cdaaar"),
                  make_symbol("caaadr"),
                  make_symbol("cdaadr"),
                  make_symbol("caadar"),
                  make_symbol("cdadar"),
                  make_symbol("caaddr"),
                  make_symbol("cdaddr"),
                  make_symbol("cadaar"),
                  make_symbol("cddaar"),
                  make_symbol("cadadr"),
                  make_symbol("cddadr"),
                  make_symbol("caddar"),
                  make_symbol("cdddar"),
                  make_symbol("cadddr"),
                  make_symbol("cddddr"),
                  make_symbol("set-car!"),
                  make_symbol("set-cdr!"));
    }));

    libraries().emplace("(meevax port)", make<library>([](auto define)
    {
      define(make_symbol("input-port?"), make<procedure>("input-port?", [](let const& xs)
      {
        return car(xs).is_also<input_port>();
      }));

      define(make_symbol("output-port?"), make<procedure>("output-port?", [](let const& xs)
      {
        return car(xs).is_also<output_port>();
      }));

      define(make_symbol("binary-port?"), make<procedure>("binary-port?", [](let const& xs)
      {
        return car(xs).is_also<binary_port>();
      }));

      define(make_symbol("textual-port?"), make<procedure>("textual-port?", [](let const& xs)
      {
        return car(xs).is_also<textual_port>();
      }));

      define(make_symbol("port?"), make<procedure>("port?", [](let const& xs)
      {
        return car(xs).is_also<port>();
      }));

      define(make_symbol("open?"), make<procedure>("open?", [](let const& xs)
      {
        return car(xs).as<port>().is_open();
      }));

      define(make_symbol("standard-input-port"), make<procedure>("standard-input-port", []()
      {
        return make<standard_input_port>();
      }));

      define(make_symbol("standard-output-port"), make<procedure>("standard-output-port", []()
      {
        return make<standard_output_port>();
      }));

      define(make_symbol("standard-error-port"), make<procedure>("standard-error-port", []()
      {
        return make<standard_error_port>();
      }));

      define(make_symbol("open-input-file"), make<procedure>("open-input-file", [](let const& xs)
      {
        return make<input_file_port>(car(xs).as<string>().utf8());
      }));

      define(make_symbol("open-output-file"), make<procedure>("open-output-file", [](let const& xs)
      {
        return make<output_file_port>(car(xs).as<string>().utf8());
      }));

      define(make_symbol("open-binary-input-file"), make<procedure>("open-binary-input-file", [](let const& xs)
      {
        return make<binary_input_file_port>(car(xs).as<string>().utf8());
      }));

      define(make_symbol("open-binary-output-file"), make<procedure>("open-binary-output-file", [](let const& xs)
      {
        return make<binary_output_file_port>(car(xs).as<string>().utf8());
      }));

      define(make_symbol("close"), make<procedure>("close", [](let const& xs)
      {
        car(xs).as<port>().close();
      }));

      define(make_symbol("open-input-string"), make<procedure>("open-input-string", [](let const& xs)
      {
        return make<input_string_port>(car(xs).as<string>().utf8());
      }));

      define(make_symbol("open-output-string"), make<procedure>("open-output-string", [](let const&)
      {
        return make<output_string_port>();
      }));

      define(make_symbol("get-output-string"), make<procedure>("get-output-string", [](let const& xs)
      {
        return make<string>(car(xs).as<output_string_port>().ostringstream.str());
      }));

      define(make_symbol("open-input-u8vector"), make<procedure>("open-input-u8vector", [](let const& xs)
      {
        return make<input_u8vector_port>(car(xs).as<u8vector>());
      }));

      define(make_symbol("open-output-u8vector"), make<procedure>("open-output-u8vector", [](let const&)
      {
        return make<output_u8vector_port>();
      }));

      define(make_symbol("get-output-u8vector"), make<procedure>("get-output-u8vector", [](let const& xs)
      {
        return make<u8vector>(car(xs).as<output_u8vector_port>().vector.data(),
                              car(xs).as<output_u8vector_port>().vector.size());
      }));

      define(make_symbol("eof-object?"), make<procedure>("eof-object?", [](let const& xs)
      {
        return car(xs).is<eof>();
      }));

      define(make_symbol("eof-object"), make<procedure>("eof-object", []()
      {
        return eof_object;
      }));

      define(make_symbol("flush"), make<procedure>("flush", [](let const& xs)
      {
        car(xs).as<output_port>().flush();
      }));

      return list(make_symbol("input-port?"),
                  make_symbol("output-port?"),
                  make_symbol("binary-port?"),
                  make_symbol("textual-port?"),
                  make_symbol("port?"),
                  make_symbol("open?"),
                  make_symbol("standard-input-port"),
                  make_symbol("standard-output-port"),
                  make_symbol("standard-error-port"),
                  make_symbol("open-input-file"),
                  make_symbol("open-output-file"),
                  make_symbol("open-binary-input-file"),
                  make_symbol("open-binary-output-file"),
                  make_symbol("close"),
                  make_symbol("open-input-string"),
                  make_symbol("open-output-string"),
                  make_symbol("get-output-string"),
                  make_symbol("open-input-u8vector"),
                  make_symbol("open-output-u8vector"),
                  make_symbol("get-output-u8vector"),
                  make_symbol("eof-object?"),
                  make_symbol("eof-object"),
                  make_symbol("flush"));
    }));

    libraries().emplace("(meevax procedure)", make<library>([](auto define)
    {
      define(make_symbol("closure?"), make<procedure>("closure?", [](let const& xs)
      {
        return car(xs).is<closure>();
      }));

      define(make_symbol("continuation?"), make<procedure>("continuation?", [](let const& xs)
      {
        return car(xs).is<continuation>();
      }));

      define(make_symbol("procedure?"), make<procedure>("procedure?", [](let const& xs)
      {
        return car(xs).is<closure>() or car(xs).is<continuation>() or car(xs).is_also<primitive>();
      }));

      define(make_symbol("procedure"), make<procedure>("procedure", [](let const& xs)
      {
        auto lookup = [](std::string const& pathname, char const* symbol)
        {
          auto lookup = [](std::string const& pathname)
          {
            auto dlopen = [](std::string const& pathname)
            {
              auto dlclose = [](void * const handle) -> void
              {
                if (handle and ::dlclose(handle))
                {
                  std::cerr << ::dlerror() << std::endl;
                }
              };

              auto static libraries = std::unordered_map<std::string, std::unique_ptr<void, decltype(dlclose)>>();

              if (auto found = libraries.find(pathname); found != libraries.end())
              {
                return found->second.get();
              }
              else
              {
                ::dlerror(); // clear

                if (auto handle = ::dlopen(pathname.c_str(), RTLD_LAZY | RTLD_GLOBAL); handle)
                {
                  auto [emplaced, success] = libraries.emplace(std::piecewise_construct,
                                                               std::forward_as_tuple(pathname),
                                                               std::forward_as_tuple(handle, dlclose));
                  return emplaced->second.get();
                }
                else
                {
                  throw std::runtime_error(::dlerror());
                }
              }
            };

            auto dlsym = [](std::string const& symbol, auto const handle)
            {
              ::dlerror(); // clear

              if (auto address = ::dlsym(handle, symbol.c_str()); address)
              {
                return address;
              }
              else
              {
                throw std::runtime_error(::dlerror());
              }
            };

            using interface = auto (*)(char const*) -> void *;

            auto static interfaces = std::unordered_map<std::string, interface>();

            if (auto found = interfaces.find(pathname); found != interfaces.end())
            {
              return found->second;
            }
            else
            {
              auto [emplaced, success] = interfaces.emplace(pathname, reinterpret_cast<interface>(dlsym("lookup", dlopen(pathname))));

              return emplaced->second;
            }
          };

          return reinterpret_cast<primitive::signature>(lookup(pathname)(symbol));
        };

        return make<procedure>(cadr(xs).as<symbol>(), lookup(car(xs).as<string>().utf8(), cadr(xs).as<symbol>().name.c_str()));
      }));

      return list(make_symbol("closure?"),
                  make_symbol("continuation?"),
                  make_symbol("procedure?"),
                  make_symbol("procedure"));
    }));

    libraries().emplace("(meevax read)", make<library>([](auto define)
    {
      define(make_symbol("get-char"), make<procedure>("get-char", [](let const& xs)
      {
        return car(xs).as<textual_input_port>().get();
      }));

      define(make_symbol("get-char-ready?"), make<procedure>("get-char-ready?", [](let const& xs)
      {
        return car(xs).as<textual_input_port>().get_ready();
      }));

      define(make_symbol("get-line"), make<procedure>("get-line", [](let const& xs)
      {
        return car(xs).as<textual_input_port>().get_line();
      }));

      define(make_symbol("peek-char"), make<procedure>("peek-char", [](let const& xs)
      {
        return car(xs).as<textual_input_port>().peek();
      }));

      define(make_symbol("read"), make<procedure>("read", [](let const& xs)
      {
        return car(xs).as<textual_input_port>().read();
      }));

      define(make_symbol("get-string"), make<procedure>("get-string", [](let const& xs)
      {
        return cadr(xs).as<textual_input_port>().get(exact_integer_cast<std::size_t>(car(xs)));
      }));

      define(make_symbol("get-u8"), make<procedure>("get-u8", [](let const& xs)
      {
        return car(xs).as<binary_input_port>().get();
      }));

      define(make_symbol("get-u8-ready?"), make<procedure>("get-u8-ready?", [](let const& xs)
      {
        return car(xs).as<binary_input_port>().get_ready();
      }));

      define(make_symbol("peek-u8"), make<procedure>("peek-u8", [](let const& xs)
      {
        return car(xs).as<binary_input_port>().peek();
      }));

      define(make_symbol("get-u8vector"), make<procedure>("get-u8vector", [](let const& xs)
      {
        return cadr(xs).as<binary_input_port>().get(exact_integer_cast<std::size_t>(car(xs)));
      }));

      return list(make_symbol("get-char"),
                  make_symbol("get-char-ready?"),
                  make_symbol("get-line"),
                  make_symbol("peek-char"),
                  make_symbol("read"),
                  make_symbol("get-string"),
                  make_symbol("get-u8"),
                  make_symbol("get-u8-ready?"),
                  make_symbol("peek-u8"),
                  make_symbol("get-u8vector"));
    }));

    libraries().emplace("(meevax string)", make<library>([](auto define)
    {
      define(make_symbol("string?"), make<procedure>("string?", [](let const& xs)
      {
        return car(xs).is<string>();
      }));

      define(make_symbol("make-string"), make<procedure>("make-string", [](let const& xs)
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

      define(make_symbol("string"), make<procedure>("string", [](let const& xs)
      {
        return make_string_from_list_of_character(xs);
      }));

      define(make_symbol("string-length"), make<procedure>("string-length", [](let const& xs)
      {
        return make(static_cast<small_integer>(car(xs).as<string>().characters.size())); // XXX DIRTY HACK (MAKE large_integer IF THE LENGTH IS GREATER THAN INT_MAX)
      }));

      define(make_symbol("string-ref"), make<procedure>("string-ref", [](let const& xs)
      {
        return make(car(xs).as<string>().characters.at(exact_integer_cast<std::size_t>(cadr(xs))));
      }));

      define(make_symbol("string-set!"), make<procedure>("string-set!", [](let & xs)
      {
        car(xs).as<string>().characters.at(exact_integer_cast<std::size_t>(cadr(xs))) = caddr(xs).as<character>();
      }));

      define(make_symbol("string=?"), make<procedure>("string=?", [](let const& xs)
      {
        auto compare = [](let const& a, let const& b)
        {
          return not (a.as<string>() == b.as<string>());
        };

        return std::adjacent_find(xs.begin(), xs.end(), compare) == xs.end();
      }));

      define(make_symbol("string<?"), make<procedure>("string<?", [](let const& xs)
      {
        auto compare = [](let const& a, let const& b)
        {
          return not (a.as<string>() < b.as<string>());
        };

        return std::adjacent_find(xs.begin(), xs.end(), compare) == xs.end();
      }));

      define(make_symbol("string>?"), make<procedure>("string>?", [](let const& xs)
      {
        auto compare = [](let const& a, let const& b)
        {
          return not (a.as<string>() > b.as<string>());
        };

        return std::adjacent_find(xs.begin(), xs.end(), compare) == xs.end();
      }));

      define(make_symbol("string<=?"), make<procedure>("string<=?", [](let const& xs)
      {
        auto compare = [](let const& a, let const& b)
        {
          return not (a.as<string>() <= b.as<string>());
        };

        return std::adjacent_find(xs.begin(), xs.end(), compare) == xs.end();
      }));

      define(make_symbol("string>=?"), make<procedure>("string>=?", [](let const& xs)
      {
        auto compare = [](let const& a, let const& b)
        {
          return not (a.as<string>() >= b.as<string>());
        };

        return std::adjacent_find(xs.begin(), xs.end(), compare) == xs.end();
      }));

      define(make_symbol("string-ci=?"), make<procedure>("string-ci=?", [](let const& xs)
      {
        auto compare = [](let const& s1, let const& s2)
        {
          auto compare = [](auto const& c1, auto const& c2)
          {
            return c1.downcase() == c2.downcase();
          };

          return not std::lexicographical_compare(s1.as<string>().characters.begin(),
                                                  s1.as<string>().characters.end(),
                                                  s2.as<string>().characters.begin(),
                                                  s2.as<string>().characters.end(),
                                                  compare);
        };

        return std::adjacent_find(xs.begin(), xs.end(), compare) == xs.end();
      }));

      define(make_symbol("string-ci<?"), make<procedure>("string-ci<?", [](let const& xs)
      {
        auto compare = [](let const& s1, let const& s2)
        {
          auto compare = [](auto const& c1, auto const& c2)
          {
            return c1.downcase() < c2.downcase();
          };

          return not std::lexicographical_compare(s1.as<string>().characters.begin(),
                                                  s1.as<string>().characters.end(),
                                                  s2.as<string>().characters.begin(),
                                                  s2.as<string>().characters.end(),
                                                  compare);
        };

        return std::adjacent_find(xs.begin(), xs.end(), compare) == xs.end();
      }));

      define(make_symbol("string-ci>?"), make<procedure>("string-ci>?", [](let const& xs)
      {
        auto compare = [](let const& s1, let const& s2)
        {
          auto compare = [](auto const& c1, auto const& c2)
          {
            return c1.downcase() > c2.downcase();
          };

          return not std::lexicographical_compare(s1.as<string>().characters.begin(),
                                                  s1.as<string>().characters.end(),
                                                  s2.as<string>().characters.begin(),
                                                  s2.as<string>().characters.end(),
                                                  compare);
        };

        return std::adjacent_find(xs.begin(), xs.end(), compare) == xs.end();
      }));

      define(make_symbol("string-ci<=?"), make<procedure>("string-ci<=?", [](let const& xs)
      {
        auto compare = [](let const& s1, let const& s2)
        {
          auto compare = [](auto const& c1, auto const& c2)
          {
            return c1.downcase() <= c2.downcase();
          };

          return not std::lexicographical_compare(s1.as<string>().characters.begin(),
                                                  s1.as<string>().characters.end(),
                                                  s2.as<string>().characters.begin(),
                                                  s2.as<string>().characters.end(),
                                                  compare);
        };

        return std::adjacent_find(xs.begin(), xs.end(), compare) == xs.end();
      }));

      define(make_symbol("string-ci>=?"), make<procedure>("string-ci>=?", [](let const& xs)
      {
        auto compare = [](let const& s1, let const& s2)
        {
          auto compare = [](auto const& c1, auto const& c2)
          {
            return c1.downcase() >= c2.downcase();
          };

          return not std::lexicographical_compare(s1.as<string>().characters.begin(),
                                                  s1.as<string>().characters.end(),
                                                  s2.as<string>().characters.begin(),
                                                  s2.as<string>().characters.end(),
                                                  compare);
        };

        return std::adjacent_find(xs.begin(), xs.end(), compare) == xs.end();
      }));

      define(make_symbol("string-append"), make<procedure>("string-append", [](let const& xs)
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

      define(make_symbol("string->list"), make<procedure>("string->list", [](let const& xs)
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

      define(make_symbol("list->string"), make<procedure>("list->string", [](let const& xs)
      {
        return make_string_from_list_of_character(car(xs));
      }));

      define(make_symbol("string-copy"), make<procedure>("string-copy", [](let const& xs)
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

      define(make_symbol("string-copy!"), make<procedure>("string-copy!", [](let const& xs)
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

      define(make_symbol("string-fill!"), make<procedure>("string-fill!", [](let & xs)
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

      return list(make_symbol("string?"),
                  make_symbol("make-string"),
                  make_symbol("string"),
                  make_symbol("string-length"),
                  make_symbol("string-ref"),
                  make_symbol("string-set!"),
                  make_symbol("string=?"),
                  make_symbol("string<?"),
                  make_symbol("string>?"),
                  make_symbol("string<=?"),
                  make_symbol("string>=?"),
                  make_symbol("string-ci=?"),
                  make_symbol("string-ci<?"),
                  make_symbol("string-ci>?"),
                  make_symbol("string-ci<=?"),
                  make_symbol("string-ci>=?"),
                  make_symbol("string-append"),
                  make_symbol("string->list"),
                  make_symbol("list->string"),
                  make_symbol("string-copy"),
                  make_symbol("string-copy!"),
                  make_symbol("string-fill!"));
    }));

    libraries().emplace("(meevax symbol)", make<library>([](auto define)
    {
      define(make_symbol("symbol?"), make<procedure>("symbol?", [](let const& xs)
      {
        return car(xs).is<symbol>();
      }));

      define(make_symbol("symbol->string"), make<procedure>("symbol->string", [](let const& xs)
      {
        return make<string>(car(xs).as<symbol>());
      }));

      define(make_symbol("string->symbol"), make<procedure>("string->symbol", [](let const& xs)
      {
        return make_symbol(car(xs).as<string>().utf8());
      }));

      define(make_symbol("identifier->symbol"), make<procedure>("identifier->symbol", [](let const& xs)
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

      return list(make_symbol("symbol?"),
                  make_symbol("symbol->string"),
                  make_symbol("string->symbol"),
                  make_symbol("identifier->symbol"));
    }));

    libraries().emplace("(meevax syntactic-closure)", make<library>([](auto define)
    {
      define(make_symbol("identifier?"), make<procedure>("identifier?", [](let const& xs)
      {
        return car(xs).is_also<identifier>();
      }));

      define(make_symbol("transformer?"), make<procedure>("transformer?", [](let const& xs)
      {
        return car(xs).is_also<transformer>();
      }));

      define(make_symbol("syntactic-closure?"), make<procedure>("syntactic-closure?", [](let const& xs)
      {
        return car(xs).is_also<syntactic_closure>();
      }));

      define(make_symbol("make-syntactic-closure"), make<procedure>("make-syntactic-closure", [](let const& xs)
      {
        return make<syntactic_closure>(car(xs), cadr(xs), caddr(xs));
      }));

      return list(make_symbol("identifier?"),
                  make_symbol("transformer?"),
                  make_symbol("syntactic-closure?"),
                  make_symbol("make-syntactic-closure"));
    }));

    libraries().emplace("(meevax system)", make<library>([](auto define)
    {
      define(make_symbol("features"), make<procedure>("features", []()
      {
        return features();
      }));

      define(make_symbol("get-environment-variable"), make<procedure>("get-environment-variable", [](let const& xs) -> object
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

      define(make_symbol("get-environment-variables"), make<procedure>("get-environment-variables", []()
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

      return list(make_symbol("features"),
                  make_symbol("get-environment-variable"),
                  make_symbol("get-environment-variables"));
    }));

    libraries().emplace("(meevax time)", make<library>([](auto define)
    {
      define(make_symbol("current-jiffy"), make<procedure>("current-jiffy", []()
      {
        return make<large_integer>(std::chrono::high_resolution_clock::now().time_since_epoch().count());
      }));

      define(make_symbol("jiffies-per-second"), make<procedure>("jiffies-per-second", []()
      {
        return make<large_integer>(std::chrono::high_resolution_clock::period::den);
      }));

      return list(make_symbol("current-jiffy"),
                  make_symbol("jiffies-per-second"));
    }));

    libraries().emplace("(meevax vector)", make<library>([](auto define)
    {
      define(make_symbol("vector?"), make<procedure>("vector?", [](let const& xs)
      {
        return car(xs).is<vector>();
      }));

      define(make_symbol("vector"), make<procedure>("vector", [](let const& xs)
      {
        return make_vector(xs);
      }));

      define(make_symbol("make-vector"), make<procedure>("make-vector", [](let const& xs)
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

      define(make_symbol("vector-length"), make<procedure>("vector-length", [](let const& xs)
      {
        return make(static_cast<small_integer>(car(xs).as<vector>().objects.size()));
      }));

      define(make_symbol("vector-ref"), make<procedure>("vector-ref", [](let const& xs)
      {
        return car(xs).as<vector>().objects[exact_integer_cast<std::size_t>(cadr(xs))];
      }));

      define(make_symbol("vector-set!"), make<procedure>("vector-set!", [](let & xs)
      {
        car(xs).as<vector>().objects[exact_integer_cast<std::size_t>(cadr(xs))] = caddr(xs);
      }));

      define(make_symbol("vector->list"), make<procedure>("vector->list", [](let const& xs)
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

      define(make_symbol("list->vector"), make<procedure>("list->vector", [](let const& xs)
      {
        return make_vector(car(xs));
      }));

      define(make_symbol("vector->string"), make<procedure>("vector->string", [](let const& xs)
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

      define(make_symbol("string->vector"), make<procedure>("string->vector", [](let const& xs)
      {
        let v = make<vector>();

        for (auto character : car(xs).as<string>().characters)
        {
          v.as<vector>().objects.push_back(make(character));
        }

        return v;
      }));

      define(make_symbol("vector-copy"), make<procedure>("vector-copy", [](let const& xs)
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

      define(make_symbol("vector-copy!"), make<procedure>("vector-copy!", [](let const& xs)
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

      define(make_symbol("vector-append"), make<procedure>("vector-append", [](let const& xs)
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

      define(make_symbol("vector-fill!"), make<procedure>("vector-fill!", [](let & xs)
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

      return list(make_symbol("vector?"),
                  make_symbol("vector"),
                  make_symbol("make-vector"),
                  make_symbol("vector-length"),
                  make_symbol("vector-ref"),
                  make_symbol("vector-set!"),
                  make_symbol("vector->list"),
                  make_symbol("list->vector"),
                  make_symbol("vector->string"),
                  make_symbol("string->vector"),
                  make_symbol("vector-copy"),
                  make_symbol("vector-copy!"),
                  make_symbol("vector-append"),
                  make_symbol("vector-fill!"));
    }));

    libraries().emplace("(meevax vector homogeneous)", make<library>([](auto define)
    {
      #define DEFINE_VECTOR(TAG)                                               \
      define(make_symbol(#TAG "vector?"), make<procedure>(#TAG "vector?", [](let const& xs) \
      {                                                                        \
        return car(xs).is<TAG##vector>();                                      \
      }));                                                                     \
                                                                               \
      define(make_symbol("make-" #TAG "vector"), make<procedure>("make-" #TAG "vector", [](let const& xs) \
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
      define(make_symbol(#TAG "vector"), make<procedure>(#TAG "vector", [](let const& xs) \
      {                                                                        \
        return make_homogeneous_vector_from_list_of<TAG>(xs);                  \
      }));                                                                     \
                                                                               \
      define(make_symbol(#TAG "vector-length"), make<procedure>(#TAG "vector-length", [](let const& xs) \
      {                                                                        \
        return make(static_cast<small_integer>(car(xs).as<TAG##vector>().values.size())); \
      }));                                                                     \
                                                                               \
      define(make_symbol(#TAG "vector-ref"), make<procedure>(#TAG "vector-ref", [](let const& xs) \
      {                                                                        \
        return TAG##vector::output_cast(car(xs).as<TAG##vector>().values[exact_integer_cast<std::size_t>(cadr(xs))]); \
      }));                                                                     \
                                                                               \
      define(make_symbol(#TAG "vector-set!"), make<procedure>(#TAG "vector-set!", [](let const& xs) \
      {                                                                        \
        car(xs).as<TAG##vector>().values[exact_integer_cast<std::size_t>(cadr(xs))] = TAG##vector::input_cast(caddr(xs)); \
      }));                                                                     \
                                                                               \
      define(make_symbol(#TAG "vector-copy"), make<procedure>(#TAG "vector-copy", [](let const& xs) \
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
      define(make_symbol(#TAG "vector-copy!"), make<procedure>(#TAG "vector-copy!", [](let & xs) \
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
      define(make_symbol(#TAG "vector-append"), make<procedure>(#TAG "vector-append", [](let const& xs) \
      {                                                                        \
        auto const& a = car(xs).as<TAG##vector>();                             \
        auto const& b = cadr(xs).as<TAG##vector>();                            \
        let const c = make<TAG##vector>(a.values.size() + b.values.size()); \
        c.as<TAG##vector>().values[std::slice(0, a.values.size(), 1)] = a.values; \
        c.as<TAG##vector>().values[std::slice(a.values.size(), b.values.size(), 1)] = b.values; \
        return c;                                                              \
      }));                                                                     \
                                                                               \
      define(make_symbol(#TAG "vector->list"), make<procedure>(#TAG "vector->list", [](let const& xs) \
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
      define(make_symbol("list->" #TAG "vector"), make<procedure>("list->" #TAG "vector", [](let const& xs) \
      {                                                                        \
        return make_homogeneous_vector_from_list_of<TAG>(car(xs));             \
      }))

      DEFINE_VECTOR(s8); DEFINE_VECTOR(s16); DEFINE_VECTOR(s32); DEFINE_VECTOR(s64);
      DEFINE_VECTOR(u8); DEFINE_VECTOR(u16); DEFINE_VECTOR(u32); DEFINE_VECTOR(u64);
                                             DEFINE_VECTOR(f32); DEFINE_VECTOR(f64);

      #undef DEFINE_VECTOR

      define(make_symbol("u8vector->string"), make<procedure>("u8vector->string", [](let const& xs)
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

      define(make_symbol("string->u8vector"), make<procedure>("string->u8vector", [](let const& xs)
      {
        auto convert = [](std::string const& s)
        {
          return make<u8vector>(reinterpret_cast<std::uint8_t const*>(s.data()), s.size());
        };

        return convert(car(xs).as<string>().utf8());
      }));

      #define EXPORT_VECTOR(TAG)                                               \
      make_symbol(#TAG "vector?"),                                             \
      make_symbol("make-" #TAG "vector"),                                      \
      make_symbol(#TAG "vector"),                                              \
      make_symbol(#TAG "vector-length"),                                       \
      make_symbol(#TAG "vector-ref"),                                          \
      make_symbol(#TAG "vector-set!"),                                         \
      make_symbol(#TAG "vector-copy"),                                         \
      make_symbol(#TAG "vector-copy!"),                                        \
      make_symbol(#TAG "vector-append"),                                       \
      make_symbol(#TAG "vector->list"),                                        \
      make_symbol("list->" #TAG "vector")

      return list(EXPORT_VECTOR(s8), EXPORT_VECTOR(s16), EXPORT_VECTOR(s32), EXPORT_VECTOR(s64),
                  EXPORT_VECTOR(u8), EXPORT_VECTOR(u16), EXPORT_VECTOR(u32), EXPORT_VECTOR(u64),
                                                         EXPORT_VECTOR(f32), EXPORT_VECTOR(f64),
                  make_symbol("u8vector->string"),
                  make_symbol("string->u8vector"));
    }));

    libraries().emplace("(meevax write)", make<library>([](auto define)
    {
      define(make_symbol("put-char"), make<procedure>("put-char", [](let const& xs)
      {
        cadr(xs).as<textual_output_port>().put(car(xs).as<character>());
      }));

      define(make_symbol("put-string"), make<procedure>("put-string", [](let const& xs)
      {
        cadr(xs).as<textual_output_port>().put(car(xs).as<string>());
      }));

      define(make_symbol("put-u8"), make<procedure>("put-u8", [](let const& xs)
      {
        cadr(xs).as<binary_output_port>().put(exact_integer_cast<std::uint8_t>(car(xs)));
      }));

      define(make_symbol("put-u8vector"), make<procedure>("put-u8vector", [](let const& xs)
      {
        cadr(xs).as<binary_output_port>().put(car(xs).as<u8vector>());
      }));

      define(make_symbol("write"), make<procedure>("write", [](let const& xs)
      {
        cadr(xs).as<textual_output_port>().write(car(xs));
      }));

      define(make_symbol("write-simple"), make<procedure>("write-simple", [](let const& xs)
      {
        cadr(xs).as<textual_output_port>().write_simple(car(xs));
      }));

      return list(make_symbol("put-char"),
                  make_symbol("put-string"),
                  make_symbol("put-u8"),
                  make_symbol("put-u8vector"),
                  make_symbol("write"),
                  make_symbol("write-simple"));
    }));
  }
} // namespace meevax::kernel
