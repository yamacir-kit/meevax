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
#include <meevax/kernel/boolean.hpp>
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
#include <meevax/kernel/proper_list.hpp>
#include <meevax/kernel/standard_error_port.hpp>
#include <meevax/kernel/standard_input_port.hpp>
#include <meevax/kernel/standard_output_port.hpp>
#include <meevax/kernel/system.hpp>
#include <meevax/kernel/transformer.hpp>
#include <meevax/kernel/vector.hpp>
#include <meevax/ranges/fold_left.hpp>

extern char ** environ; // for procedure get-environment-variables

namespace meevax::inline kernel
{
  auto resolve(char const* name) -> void *
  {
    using namespace number;

    auto static const registry = std::unordered_map<std::string, procedure::signature>
    {
      { "binary32?",                     [](let const& xs) { return make<bool>(car(xs).is<float>()); } },

      { "binary64?",                     [](let const& xs) { return make<bool>(car(xs).is<double>()); } },
      { "binary64-integral-part",        [](let const& xs) { auto integral_part = 0.0; std::modf(car(xs).as<double>(), &integral_part); return make<double>(integral_part); } },
      { "binary64-fractional-part",      [](let const& xs) { auto integral_part = 0.0; return make<double>(std::modf(car(xs).as<double>(), &integral_part)); } },
      { "binary64-log-binary",           [](let const& xs) { return make<double>(std::logb(car(xs).as<double>())); } },
      { "binary64-integer-log-binary",   [](let const& xs) { return make<small_integer>(std::ilogb(car(xs).as<double>())); } },
      { "binary64-normalized-fraction",  [](let const& xs) { auto exponent = 0; return make<double>(std::frexp(car(xs).as<double>(), &exponent)); } },
      { "binary64-exponent",             [](let const& xs) { auto exponent = 0; std::frexp(car(xs).as<double>(), &exponent); return make<small_integer>(exponent); } },
      { "binary64-sign-bit",             [](let const& xs) { return make<bool>(std::signbit(car(xs).as<double>())); } },
      { "binary64-normalized?",          [](let const& xs) { return make<bool>(std::fpclassify(car(xs).as<double>()) == FP_NORMAL); } },
      { "binary64-denormalized?",        [](let const& xs) { return make<bool>(std::fpclassify(car(xs).as<double>()) == FP_SUBNORMAL); } },
      { "binary64-max",                  [](let const& xs) { auto max = -std::numeric_limits<double>::infinity(); for (let const& x : xs | as_proper_list) { max = std::fmax(max, x.as<double>()); } return make<double>(max); } },
      { "binary64-min",                  [](let const& xs) { auto min =  std::numeric_limits<double>::infinity(); for (let const& x : xs | as_proper_list) { min = std::fmin(min, x.as<double>()); } return make<double>(min); } },
      { "binary64-fused-multiply-add",   [](let const& xs) { return make<double>(std::fma(car(xs).as<double>(), cadr(xs).as<double>(), caddr(xs).as<double>())); } },
      { "binary64-remquo",               [](let const& xs) { auto quotient = 0; auto remainder = std::remquo(car(xs).as<double>(), cadr(xs).as<double>(), &quotient); return cons(make<double>(remainder), make<small_integer>(quotient)); } },

      { "boolean?",                      [](let const& xs) { return make<bool>(car(xs).is<bool>()); } },
      { "not",                           [](let const& xs) { return make<bool>(car(xs) == f); } },

      { "box?",                          [](let const& xs) { return make<bool>(car(xs).is<box>()); } },
      { "box",                           [](let const& xs) { return make<box>(car(xs), unit); } },
      { "box-ref",                       [](let const& xs) { return caar(xs); } },
      { "box-set!",                      [](let const& xs) { car(xs).as_mutable<box>().first = cadr(xs); return unspecified; } },

      { "char?",                         [](let const& xs) { return make<bool>(car(xs).is<character>()); } },
      { "char=?",                        [](let const& xs) { return make<bool>(std::ranges::adjacent_find(xs | as_proper_list, [](let const& a, let const& b) { return not (a.as<character>().codepoint == b.as<character>().codepoint); }) == std::ranges::end(xs | as_proper_list)); } },
      { "char<?",                        [](let const& xs) { return make<bool>(std::ranges::adjacent_find(xs | as_proper_list, [](let const& a, let const& b) { return not (a.as<character>().codepoint <  b.as<character>().codepoint); }) == std::ranges::end(xs | as_proper_list)); } },
      { "char<=?",                       [](let const& xs) { return make<bool>(std::ranges::adjacent_find(xs | as_proper_list, [](let const& a, let const& b) { return not (a.as<character>().codepoint <= b.as<character>().codepoint); }) == std::ranges::end(xs | as_proper_list)); } },
      { "char>?",                        [](let const& xs) { return make<bool>(std::ranges::adjacent_find(xs | as_proper_list, [](let const& a, let const& b) { return not (a.as<character>().codepoint >  b.as<character>().codepoint); }) == std::ranges::end(xs | as_proper_list)); } },
      { "char>=?",                       [](let const& xs) { return make<bool>(std::ranges::adjacent_find(xs | as_proper_list, [](let const& a, let const& b) { return not (a.as<character>().codepoint >= b.as<character>().codepoint); }) == std::ranges::end(xs | as_proper_list)); } },
      { "char-ci=?",                     [](let const& xs) { return make<bool>(std::ranges::adjacent_find(xs | as_proper_list, [](let const& a, let const& b) { return not (a.as<character>().downcase() == b.as<character>().downcase()); }) == std::ranges::end(xs | as_proper_list)); } },
      { "char-ci<?",                     [](let const& xs) { return make<bool>(std::ranges::adjacent_find(xs | as_proper_list, [](let const& a, let const& b) { return not (a.as<character>().downcase() <  b.as<character>().downcase()); }) == std::ranges::end(xs | as_proper_list)); } },
      { "char-ci<=?",                    [](let const& xs) { return make<bool>(std::ranges::adjacent_find(xs | as_proper_list, [](let const& a, let const& b) { return not (a.as<character>().downcase() <= b.as<character>().downcase()); }) == std::ranges::end(xs | as_proper_list)); } },
      { "char-ci>?",                     [](let const& xs) { return make<bool>(std::ranges::adjacent_find(xs | as_proper_list, [](let const& a, let const& b) { return not (a.as<character>().downcase() >  b.as<character>().downcase()); }) == std::ranges::end(xs | as_proper_list)); } },
      { "char-ci>=?",                    [](let const& xs) { return make<bool>(std::ranges::adjacent_find(xs | as_proper_list, [](let const& a, let const& b) { return not (a.as<character>().downcase() >= b.as<character>().downcase()); }) == std::ranges::end(xs | as_proper_list)); } },
      { "char-alphabetic?",              [](let const& xs) { return make<bool>(car(xs).as<character>().property().is_letter()); } },
      { "char-numeric?",                 [](let const& xs) { return make<bool>(car(xs).as<character>().property().is_numeric()); } },
      { "char-whitespace?",              [](let const& xs) { return make<bool>(car(xs).as<character>().property().is_whitespace()); } },
      { "char-upper-case?",              [](let const& xs) { return make<bool>(car(xs).as<character>().property().is_upper_case()); } },
      { "char-lower-case?",              [](let const& xs) { return make<bool>(car(xs).as<character>().property().is_lower_case()); } },
      { "digit-value",                   [](let const& xs) { if (auto digit_value = car(xs).as<character>().digit_value(); digit_value) { return make<small_integer>(*digit_value); } else { return f; } } },
      { "char->integer",                 [](let const& xs) { return make<small_integer>(car(xs).as<character>().codepoint); } },
      { "integer->char",                 [](let const& xs) { return make<character>(exact_integer_cast<character::int_type>(car(xs))); } },
      { "char-upcase",                   [](let const& xs) { return make<character>(car(xs).as<character>().upcase()); } },
      { "char-downcase",                 [](let const& xs) { return make<character>(car(xs).as<character>().downcase()); } },

      { "emergency-exit",                [](let const& xs) -> object { if (xs.is<null>()) { throw EXIT_SUCCESS; } else if (let const& status = car(xs); status.is<bool>()) { throw status != f ? EXIT_SUCCESS : EXIT_FAILURE; } else { throw exact_integer_cast<int>(status); } } },
      { "command-line",                  [](let const&   ) { let xs = unit; for (auto&& each : configurator::command_line()) { xs = cons(make<string>(each), xs); } return reverse(xs); } },

      { "eq?",                           [](let const& xs) { return make<bool>(eq(car(xs), cadr(xs))); } },
      { "eqv?",                          [](let const& xs) { return make<bool>(eqv(car(xs), cadr(xs))); } },
      { "equal?",                        [](let const& xs) { return make<bool>(equal(car(xs), cadr(xs))); } },

      { "environment",                   [](let const& xs) { return make<environment>(xs | as_proper_list); } },
      { "eval",                          [](let const& xs) { return cadr(xs).as<environment>().evaluate(car(xs)); } },
      { "expand",                        [](let const& xs) { return cadr(xs).as<environment>().expand(car(xs), unit); } },
      { "interaction-environment",       [](let const&   ) { return interaction_environment(); } },
      { "load",                          [](let const& xs) { car(xs).as<environment>().load(cadr(xs).as<string>().utf8()); return unspecified; } },

      { "throw",                         [](let const& xs) -> object { throw car(xs); } },
      { "error-object",                  [](let const& xs) { return make<error>(car(xs), cdr(xs)); } },
      { "error-object?",                 [](let const& xs) { return make<bool>(car(xs).is_also<error>()); } },
      { "read-error?",                   [](let const& xs) { return make<bool>(car(xs).is<read_error>()); } },
      { "file-error?",                   [](let const& xs) { return make<bool>(car(xs).is<file_error>()); } },
      { "kernel-exception-handler-set!", [](let const& xs) { return environment::exception_handler = car(xs); } },

      { "file-exists?",                  [](let const& xs) { return make<bool>(std::filesystem::exists(car(xs).as<string>().utf8())); } },
      { "delete-file",                   [](let const& xs) { try { if (not std::filesystem::remove(car(xs).as<string>().utf8())) { throw file_error(make<string>("failed to remove file"), car(xs)); } else { return unspecified; } } catch (std::filesystem::filesystem_error const& e) { throw file_error(make<string>(e.what()), car(xs)); } } },
      { "library-directories",           [](let const&   ) { let directories = unit; for (auto iterator = configurator::directories().rbegin(); iterator != configurator::directories().rend(); ++iterator) { directories = cons(make<string>(iterator->native()), directories); } return directories; } },

      { "integer32?",                    [](let const& xs) { return make<bool>(car(xs).is<small_integer>()); } },

      { "null?",                         [](let const& xs) { return make<bool>(car(xs).is<null>()); } },
      { "list?",                         [](let const& xs) { return make<bool>(is_proper_list(car(xs))); } },
      { "list",                          [](let const& xs) { return xs; } },
      { "make-list",                     [](let const& xs) { switch (length(xs)) { case 1: return make_list(exact_integer_cast<std::size_t>(car(xs))); case 2: return make_list(exact_integer_cast<std::size_t>(car(xs)), cadr(xs)); default: throw error(make<string>("procedure make-list takes one or two arugments, but got"), xs); } } },
      { "iota",                          [](let const& xs) { switch (length(xs)) { case 1: return iota(exact_integer_cast<std::size_t>(car(xs))); case 2: return iota(exact_integer_cast<std::size_t>(car(xs)), cadr(xs)); case 3: return iota(exact_integer_cast<std::size_t>(car(xs)), cadr(xs), caddr(xs)); default: throw error(make<string>("procedure iota takes one to three arugments, but got"), xs); } } },
      { "circular-list?",                [](let const& xs) { return make<bool>(is_circular_list(car(xs))); } },
      { "circular-list",                 [](let const& xs) { let copy = list_copy(xs); circulate(copy); return copy; } },
      { "dotted-list?",                  [](let const& xs) { return make<bool>(is_dotted_list(car(xs))); } },
      { "null-list?",                    [](let const& xs) { if (is_proper_list(car(xs)) or is_circular_list(car(xs))) { return make<bool>(car(xs).is<null>()); } else { throw error(make<string>("procedure null-list? takes a proper-list or a circular-list, but got"), xs); } } },
      { "last",                          [](let const& xs) { return last(car(xs)); } },
      { "last-pair",                     [](let const& xs) { return last_pair(car(xs)); } },
      { "length",                        [](let const& xs) { return                                 make<small_integer>(static_cast<small_integer>(length(car(xs)))); } },
      { "length+",                       [](let const& xs) { return is_circular_list(car(xs)) ? f : make<small_integer>(static_cast<small_integer>(length(car(xs)))); } },
      { "append",                        [](let const& xs) { return fold_left(xs | as_proper_list, unit, [](let const& x, let const& y) { return append(x, y); }); } },
      { "append!",                       [](let const& xs) { auto append = [](auto append, let & x, let & xs) -> auto & { if (xs.is<null>()) { return x; } else if (x.is<null>()) { return x = append(append, car(xs), cdr(xs)); } else { return meevax::append(x, append(append, car(xs), cdr(xs))); } }; if (not xs.is<pair>()) { return xs; } else { let head = car(xs); let tail = cdr(xs); return append(append, head, tail); } } },
      { "append-reverse",                [](let const& xs) { return append_reverse(car(xs), cadr(xs)); } },
      { "append-reverse!",               [](let const& xs) { let x = car(xs); let y = cadr(xs); return append_reverse(x, y); } },
      { "reverse",                       [](let const& xs) { return reverse(car(xs)); } },
      { "reverse!",                      [](let const& xs) { let x = car(xs); return reverse(x); } },
      { "concatenate",                   [](let const& xs) { return fold_left(car(xs) | as_proper_list, unit, [](let const& x, let const& y) { return append(x, y); }); } },
      { "concatenate!",                  [](let const& xs) { auto concatenate = [](auto concatenate, let & x, let & xs) -> auto & { if (xs.is<null>()) { return x; } else if (x.is<null>()) { return x = concatenate(concatenate, car(xs), cdr(xs)); } else { return meevax::append(x, concatenate(concatenate, car(xs), cdr(xs))); } }; if (not xs.is<pair>()) { return xs; } else { let x = car(xs); return concatenate(concatenate, car(x), cdr(x)); } } },
      { "list-copy",                     [](let const& xs) { return list_copy(car(xs)); } },
      { "list-ref",                      [](let const& xs) { return head(car(xs), exact_integer_cast<std::size_t>(cadr(xs))); } },
      { "list-tail",                     [](let const& xs) { return tail(car(xs), exact_integer_cast<std::size_t>(cadr(xs))); } },
      { "first",                         [](let const& xs) { return head(car(xs), 0); } },
      { "second",                        [](let const& xs) { return head(car(xs), 1); } },
      { "third",                         [](let const& xs) { return head(car(xs), 2); } },
      { "fourth",                        [](let const& xs) { return head(car(xs), 3); } },
      { "fifth",                         [](let const& xs) { return head(car(xs), 4); } },
      { "sixth",                         [](let const& xs) { return head(car(xs), 5); } },
      { "seventh",                       [](let const& xs) { return head(car(xs), 6); } },
      { "eighth",                        [](let const& xs) { return head(car(xs), 7); } },
      { "ninth",                         [](let const& xs) { return head(car(xs), 8); } },
      { "tenth",                         [](let const& xs) { return head(car(xs), 9); } },
      { "take",                          [](let const& xs) { return take(car(xs), exact_integer_cast<std::size_t>(cadr(xs))); } },
      { "take!",                         [](let const& xs) { let x = car(xs); return take(x, exact_integer_cast<std::size_t>(cadr(xs))); } },
      { "take-right",                    [](let const& xs) { return take_right(car(xs), exact_integer_cast<std::size_t>(cadr(xs))); } },
      { "drop",                          [](let const& xs) { return drop(car(xs), exact_integer_cast<std::size_t>(cadr(xs))); } },
      { "drop-right",                    [](let const& xs) { return drop_right(car(xs), exact_integer_cast<std::size_t>(cadr(xs))); } },
      { "drop-right!",                   [](let const& xs) { let x = car(xs); return drop_right(x, exact_integer_cast<std::size_t>(cadr(xs))); } },
      { "memq",                          [](let const& xs) { return memq(car(xs), cadr(xs)); } },
      { "memv",                          [](let const& xs) { return memv(car(xs), cadr(xs)); } },
      { "assq",                          [](let const& xs) { return assq(car(xs), cadr(xs)); } },
      { "assv",                          [](let const& xs) { return assv(car(xs), cadr(xs)); } },
      { "alist-cons",                    [](let const& xs) { return alist_cons(car(xs), cadr(xs), caddr(xs)); } },
      { "alist-copy",                    [](let const& xs) { return alist_copy(car(xs)); } },

      { "number?",                       [](let const& xs) { return make<bool>(is_complex(car(xs))); } },
      { "complex?",                      [](let const& xs) { return make<bool>(is_complex(car(xs))); } },
      { "real?",                         [](let const& xs) { return make<bool>(is_real(car(xs))); } },
      { "rational?",                     [](let const& xs) { return make<bool>(is_rational(car(xs))); } },
      { "integer?",                      [](let const& xs) { return make<bool>(is_integer(car(xs))); } },
      { "exact?",                        [](let const& xs) { return make<bool>(is_exact(car(xs))); } },
      { "inexact?",                      [](let const& xs) { return make<bool>(is_inexact(car(xs))); } },
      { "exact-integer?",                [](let const& xs) { return make<bool>(is_exact_integer(car(xs))); } },
      { "finite?",                       [](let const& xs) { return make<bool>(is_finite(car(xs))); } },
      { "infinite?",                     [](let const& xs) { return make<bool>(is_infinite(car(xs))); } },
      { "nan?",                          [](let const& xs) { return make<bool>(is_nan(car(xs))); } },
      { "=",                             [](let const& xs) { return make<bool>(std::ranges::adjacent_find(xs | as_proper_list,             not_equals) == std::ranges::end(xs | as_proper_list)); } },
      { "<",                             [](let const& xs) { return make<bool>(std::ranges::adjacent_find(xs | as_proper_list, greater_than_or_equals) == std::ranges::end(xs | as_proper_list)); } },
      { "<=",                            [](let const& xs) { return make<bool>(std::ranges::adjacent_find(xs | as_proper_list, greater_than          ) == std::ranges::end(xs | as_proper_list)); } },
      { ">",                             [](let const& xs) { return make<bool>(std::ranges::adjacent_find(xs | as_proper_list, less_than_or_equals   ) == std::ranges::end(xs | as_proper_list)); } },
      { ">=",                            [](let const& xs) { return make<bool>(std::ranges::adjacent_find(xs | as_proper_list, less_than             ) == std::ranges::end(xs | as_proper_list)); } },
      { "zero?",                         [](let const& xs) { return make<bool>(is_zero(car(xs))); } },
      { "positive?",                     [](let const& xs) { return make<bool>(is_positive(car(xs))); } },
      { "negative?",                     [](let const& xs) { return make<bool>(is_negative(car(xs))); } },
      { "odd?",                          [](let const& xs) { return make<bool>(is_odd(car(xs))); } },
      { "even?",                         [](let const& xs) { return make<bool>(is_even(car(xs))); } },
      { "max",                           [](let const& xs) { if (auto iter = std::ranges::max_element(xs | as_proper_list, less_than); iter != std::ranges::end(xs | as_proper_list)) { return std::ranges::any_of(xs | as_proper_list, is_inexact) ? inexact(*iter) : *iter; } else { return unspecified; } } },
      { "min",                           [](let const& xs) { if (auto iter = std::ranges::min_element(xs | as_proper_list, less_than); iter != std::ranges::end(xs | as_proper_list)) { return std::ranges::any_of(xs | as_proper_list, is_inexact) ? inexact(*iter) : *iter; } else { return unspecified; } } },
      { "+",                             [](let const& xs) { return fold_left(xs | as_proper_list, e0, std::plus()); } },
      { "*",                             [](let const& xs) { return fold_left(xs | as_proper_list, e1, std::multiplies()); } },
      { "-",                             [](let const& xs) { return cdr(xs).is<pair>() ? fold_left(cdr(xs) | as_proper_list, car(xs), std::minus  ()) : e0 - car(xs); } },
      { "/",                             [](let const& xs) { return cdr(xs).is<pair>() ? fold_left(cdr(xs) | as_proper_list, car(xs), std::divides()) : e1 / car(xs); } },
      { "abs",                           [](let const& xs) { return abs(car(xs)); } },
      { "quotient",                      [](let const& xs) { return quotient(car(xs), cadr(xs)); } },
      { "remainder",                     [](let const& xs) { return remainder(car(xs), cadr(xs)); } },
      { "modulo",                        [](let const& xs) { return modulo(car(xs), cadr(xs)); } },
      { "gcd",                           [](let const& xs) { switch (length(xs)) { case 0: return e0; case 1: return car(xs); default: return fold_left(cdr(xs) | as_proper_list, car(xs), gcd); } } },
      { "lcm",                           [](let const& xs) { switch (length(xs)) { case 0: return e1; case 1: return car(xs); default: return fold_left(cdr(xs) | as_proper_list, car(xs), lcm); } } },
      { "numerator",                     [](let const& xs) { return numerator(car(xs)); } },
      { "denominator",                   [](let const& xs) { return denominator(car(xs)); } },
      { "floor",                         [](let const& xs) { return floor(car(xs)); } },
      { "ceiling",                       [](let const& xs) { return ceiling(car(xs)); } },
      { "truncate",                      [](let const& xs) { return truncate(car(xs)); } },
      { "round",                         [](let const& xs) { return round(car(xs)); } },
      { "exp",                           [](let const& xs) { return exp(car(xs)); } },
      { "log",                           [](let const& xs) { switch (length(xs)) { case 1: return log(car(xs)); case 2: return log(car(xs)) / log(cadr(xs)); default: throw error(make<string>("procedure log takes one or two arguments, but got"), xs); } } },
      { "sin",                           [](let const& xs) { return sin(car(xs)); } },
      { "cos",                           [](let const& xs) { return cos(car(xs)); } },
      { "tan",                           [](let const& xs) { return tan(car(xs)); } },
      { "asin",                          [](let const& xs) { return asin(car(xs)); } },
      { "acos",                          [](let const& xs) { return acos(car(xs)); } },
      { "atan",                          [](let const& xs) { switch (length(xs)) { case 1: return atan(car(xs)); case 2: return atan2(car(xs), cadr(xs)); default: throw error(make<string>("procedure atan takes one or two arguments, but got"), xs); } } },
      { "sinh",                          [](let const& xs) { return sinh(car(xs)); } },
      { "cosh",                          [](let const& xs) { return cosh(car(xs)); } },
      { "tanh",                          [](let const& xs) { return tanh(car(xs)); } },
      { "asinh",                         [](let const& xs) { return asinh(car(xs)); } },
      { "acosh",                         [](let const& xs) { return acosh(car(xs)); } },
      { "atanh",                         [](let const& xs) { return atanh(car(xs)); } },
      { "sqrt",                          [](let const& xs) { return sqrt(car(xs)); } },
      { "exact-integer-square-root",     [](let const& xs) { auto sqrt = [](let const& x) { if (x.is<small_integer>()) { return large_integer(x.as<small_integer>()).sqrt(); } else { return x.as<large_integer>().sqrt(); } }; auto&& [s, r] = sqrt(car(xs)); return cons(make<large_integer>(std::forward<decltype(s)>(s)), make<large_integer>(std::forward<decltype(r)>(r))); } },
      { "expt",                          [](let const& xs) { return pow(car(xs), cadr(xs)); } },
      { "make-rectangular",              [](let const& xs) { return make<complex>(car(xs), cadr(xs)); } },
      { "make-polar",                    [](let const& xs) { let const& radius = car(xs), angle = cadr(xs); return make<complex>(radius * cos(angle), radius * sin(angle)); } },
      { "real-part",                     [](let const& xs) { return real(car(xs)); } },
      { "imag-part",                     [](let const& xs) { return imag(car(xs)); } },
      { "magnitude",                     [](let const& xs) { return magnitude(car(xs)); } },
      { "angle",                         [](let const& xs) { return angle(car(xs)); } },
      { "exact",                         [](let const& xs) { return exact(car(xs)); } },
      { "inexact",                       [](let const& xs) { return inexact(car(xs)); } },
      { "expm1",                         [](let const& xs) { return expm1(car(xs)); } },
      { "log1p",                         [](let const& xs) { return log1p(car(xs)); } },
      { "erf",                           [](let const& xs) { return erf(car(xs)); } },
      { "erfc",                          [](let const& xs) { return erfc(car(xs)); } },
      { "tgamma",                        [](let const& xs) { return tgamma(car(xs)); } },
      { "lgamma",                        [](let const& xs) { return lgamma(car(xs)); } },
      { "ldexp",                         [](let const& xs) { return ldexp(car(xs), cadr(xs)); } },
      { "nextafter",                     [](let const& xs) { return nextafter(car(xs), cadr(xs)); } },
      { "copysign",                      [](let const& xs) { return copysign(car(xs), cadr(xs)); } },
      { "cyl_bessel_j",                  [](let const& xs) { return cyl_bessel_j(car(xs), cadr(xs)); } },
      { "cyl_neumann",                   [](let const& xs) { return cyl_neumann(car(xs), cadr(xs)); } },
      { "bitwise-not",                   [](let const& xs) { return bitwise_not(car(xs)); } },
      { "bitwise-and",                   [](let const& xs) { return fold_left(xs | as_proper_list, make<small_integer>(-1), bitwise_and); } },
      { "bitwise-ior",                   [](let const& xs) { return fold_left(xs | as_proper_list, make<small_integer>( 0), bitwise_ior); } },
      { "bitwise-xor",                   [](let const& xs) { return fold_left(xs | as_proper_list, make<small_integer>( 0), bitwise_xor); } },
      { "bit-shift",                     [](let const& xs) { return bit_shift(car(xs), exact_integer_cast<small_integer>(cadr(xs))); } },
      { "bit-count",                     [](let const& xs) { return bit_count(car(xs)); } },
      { "bit-width",                     [](let const& xs) { return bit_width(car(xs)); } },
      { "number->string",                [](let const& xs) { switch (length(xs)) { case 1: return number_to_string(car(xs), 10); case 2: return number_to_string(car(xs), exact_integer_cast<std::size_t>(cadr(xs))); default: throw error(make<string>("procedure number->string takes one or two arugments, but got"), xs); } } },
      { "string->number",                [](let const& xs) { switch (length(xs)) { case 1: return make_number(car(xs).as<string>().utf8(), 10); case 2: return make_number(car(xs).as<string>().utf8(), exact_integer_cast<std::size_t>(cadr(xs))); default: throw error(make<string>("procedure string->number takes one or two arugments, but got"), xs); } } },

      { "pair?",                         [](let const& xs) { return make<bool>(car(xs).is<pair>()); } },
      { "not-pair?",                     [](let const& xs) { return make<bool>(not car(xs).is<pair>()); } },
      { "cons",                          [](let const& xs) { return cons(car(xs), cadr(xs)); } },
      { "xcons",                         [](let const& xs) { return cons(cadr(xs), car(xs)); } },
      { "cons*",                         [](let const& xs) { if (xs.is<null>()) { throw error(make<string>("procedure cons* takes at least one arugments, but got"), xs); } else if (cdr(xs).is<null>()) { return car(xs); } else { auto node = xs.get(); while (not cdr(node->second).is<null>()) { node = node->second.get(); } node->second = car(node->second); return xs; } } },
      { "car",                           [](let const& xs) { return car(car(xs)); } },
      { "cdr",                           [](let const& xs) { return cdr(car(xs)); } },
      { "caar",                          [](let const& xs) { return caar(car(xs)); } },
      { "cadr",                          [](let const& xs) { return cadr(car(xs)); } },
      { "cdar",                          [](let const& xs) { return cdar(car(xs)); } },
      { "cddr",                          [](let const& xs) { return cddr(car(xs)); } },
      { "caaar",                         [](let const& xs) { return caaar(car(xs)); } },
      { "cdaar",                         [](let const& xs) { return cdaar(car(xs)); } },
      { "caadr",                         [](let const& xs) { return caadr(car(xs)); } },
      { "cdadr",                         [](let const& xs) { return cdadr(car(xs)); } },
      { "cadar",                         [](let const& xs) { return cadar(car(xs)); } },
      { "cddar",                         [](let const& xs) { return cddar(car(xs)); } },
      { "caddr",                         [](let const& xs) { return caddr(car(xs)); } },
      { "cdddr",                         [](let const& xs) { return cdddr(car(xs)); } },
      { "caaaar",                        [](let const& xs) { return caaaar(car(xs)); } },
      { "cdaaar",                        [](let const& xs) { return cdaaar(car(xs)); } },
      { "caaadr",                        [](let const& xs) { return caaadr(car(xs)); } },
      { "cdaadr",                        [](let const& xs) { return cdaadr(car(xs)); } },
      { "caadar",                        [](let const& xs) { return caadar(car(xs)); } },
      { "cdadar",                        [](let const& xs) { return cdadar(car(xs)); } },
      { "caaddr",                        [](let const& xs) { return caaddr(car(xs)); } },
      { "cdaddr",                        [](let const& xs) { return cdaddr(car(xs)); } },
      { "cadaar",                        [](let const& xs) { return cadaar(car(xs)); } },
      { "cddaar",                        [](let const& xs) { return cddaar(car(xs)); } },
      { "cadadr",                        [](let const& xs) { return cadadr(car(xs)); } },
      { "cddadr",                        [](let const& xs) { return cddadr(car(xs)); } },
      { "caddar",                        [](let const& xs) { return caddar(car(xs)); } },
      { "cdddar",                        [](let const& xs) { return cdddar(car(xs)); } },
      { "cadddr",                        [](let const& xs) { return cadddr(car(xs)); } },
      { "cddddr",                        [](let const& xs) { return cddddr(car(xs)); } },
      { "set-car!",                      [](let const& xs) { return car(xs).as_mutable<pair>().first = cadr(xs); } },
      { "set-cdr!",                      [](let const& xs) { return car(xs).as_mutable<pair>().second = cadr(xs); } },

      { "input-port?",                   [](let const& xs) { return make<bool>(car(xs).is_also<input_port>()); } },
      { "output-port?",                  [](let const& xs) { return make<bool>(car(xs).is_also<output_port>()); } },
      { "binary-port?",                  [](let const& xs) { return make<bool>(car(xs).is_also<binary_port>()); } },
      { "textual-port?",                 [](let const& xs) { return make<bool>(car(xs).is_also<textual_port>()); } },
      { "port?",                         [](let const& xs) { return make<bool>(car(xs).is_also<port>()); } },
      { "open?",                         [](let const& xs) { return make<bool>(car(xs).as<port>().is_open()); } },
      { "standard-input-port",           [](let const&   ) { return make<standard_input_port>(); } },
      { "standard-output-port",          [](let const&   ) { return make<standard_output_port>(); } },
      { "standard-error-port",           [](let const&   ) { return make<standard_error_port>(); } },
      { "open-input-file",               [](let const& xs) { return make<input_file_port>(car(xs).as<string>().utf8()); } },
      { "open-output-file",              [](let const& xs) { return make<output_file_port>(car(xs).as<string>().utf8()); } },
      { "open-binary-input-file",        [](let const& xs) { return make<binary_input_file_port>(car(xs).as<string>().utf8()); } },
      { "open-binary-output-file",       [](let const& xs) { return make<binary_output_file_port>(car(xs).as<string>().utf8()); } },
      { "close",                         [](let const& xs) { car(xs).as<port>().close(); return unspecified; } },
      { "open-input-string",             [](let const& xs) { return make<input_string_port>(car(xs).as<string>().utf8()); } },
      { "open-output-string",            [](let const&   ) { return make<output_string_port>(); } },
      { "get-output-string",             [](let const& xs) { return make<string>(car(xs).as<output_string_port>().ostringstream.str()); } },
      { "open-input-u8vector",           [](let const& xs) { return make<input_u8vector_port>(car(xs).as<u8vector>()); } },
      { "open-output-u8vector",          [](let const&   ) { return make<output_u8vector_port>(); } },
      { "get-output-u8vector",           [](let const& xs) { return make<u8vector>(car(xs).as<output_u8vector_port>().vector.data(), car(xs).as<output_u8vector_port>().vector.size()); } },
      { "eof-object?",                   [](let const& xs) { return make<bool>(car(xs).is<eof>()); } },
      { "eof-object",                    [](let const&   ) { return eof_object; } },
      { "flush",                         [](let const& xs) { car(xs).as<output_port>().flush(); return unspecified; } },

      { "closure?",                      [](let const& xs) { return make<bool>(car(xs).is<closure>()); } },
      { "continuation?",                 [](let const& xs) { return make<bool>(car(xs).is<continuation>()); } },
      { "procedure?",                    [](let const& xs) { return make<bool>(car(xs).is<closure>() or car(xs).is<continuation>() or car(xs).is<procedure>()); } },
      { "procedure",                     [](let const& xs) { return make<procedure>(car(xs).as<string>().utf8(), cadr(xs).as<symbol>().name.c_str()); } },

      { "get-char",                      [](let const& xs) { return car(xs).as<textual_input_port>().get(); } },
      { "get-char-ready?",               [](let const& xs) { return make<bool>(car(xs).as<textual_input_port>().get_ready()); } },
      { "get-line",                      [](let const& xs) { return car(xs).as<textual_input_port>().get_line(); } },
      { "peek-char",                     [](let const& xs) { return car(xs).as<textual_input_port>().peek(); } },
      { "read",                          [](let const& xs) { return car(xs).as<textual_input_port>().read(); } },
      { "get-string",                    [](let const& xs) { return cadr(xs).as<textual_input_port>().get(exact_integer_cast<std::size_t>(car(xs))); } },
      { "get-u8",                        [](let const& xs) { return car(xs).as<binary_input_port>().get(); } },
      { "get-u8-ready?",                 [](let const& xs) { return make<bool>(car(xs).as<binary_input_port>().get_ready()); } },
      { "peek-u8",                       [](let const& xs) { return car(xs).as<binary_input_port>().peek(); } },
      { "get-u8vector",                  [](let const& xs) { return cadr(xs).as<binary_input_port>().get(exact_integer_cast<std::size_t>(car(xs))); } },

      { "string?",                       [](let const& xs) { return make<bool>(car(xs).is<string>()); } },
      { "make-string",                   [](let const& xs) { switch (length(xs)) { case 1: return make<string>(exact_integer_cast<std::size_t>(car(xs)), character()); case 2: return make<string>(exact_integer_cast<std::size_t>(car(xs)), cadr(xs).as<character>()); default: throw error(make<string>("procedure make-string takes one or two arugments, but got"), xs); } } },
      { "string",                        [](let const& xs) { return make<string>(xs | as_proper_list); } },
      { "string-length",                 [](let const& xs) { return make<small_integer>(static_cast<small_integer>(car(xs).as<string>().characters.size())); } }, // XXX DIRTY HACK (MAKE large_integer IF THE LENGTH IS GREATER THAN INT_MAX)
      { "string-ref",                    [](let const& xs) { return make<character>(car(xs).as<string>().characters.at(exact_integer_cast<std::size_t>(cadr(xs)))); } },
      { "string-set!",                   [](let const& xs) { car(xs).as_mutable<string>().characters.at(exact_integer_cast<std::size_t>(cadr(xs))) = caddr(xs).as<character>(); return unspecified; } },
      { "string=?",                      [](let const& xs) { auto compare = [](let const& a, let const& b) { return not (a.as<string>() == b.as<string>()); }; return make<bool>(std::ranges::adjacent_find(xs | as_proper_list, compare) == std::ranges::end(xs | as_proper_list)); } },
      { "string<?",                      [](let const& xs) { auto compare = [](let const& a, let const& b) { return not (a.as<string>() <  b.as<string>()); }; return make<bool>(std::ranges::adjacent_find(xs | as_proper_list, compare) == std::ranges::end(xs | as_proper_list)); } },
      { "string>?",                      [](let const& xs) { auto compare = [](let const& a, let const& b) { return not (a.as<string>() >  b.as<string>()); }; return make<bool>(std::ranges::adjacent_find(xs | as_proper_list, compare) == std::ranges::end(xs | as_proper_list)); } },
      { "string<=?",                     [](let const& xs) { auto compare = [](let const& a, let const& b) { return not (a.as<string>() <= b.as<string>()); }; return make<bool>(std::ranges::adjacent_find(xs | as_proper_list, compare) == std::ranges::end(xs | as_proper_list)); } },
      { "string>=?",                     [](let const& xs) { auto compare = [](let const& a, let const& b) { return not (a.as<string>() >= b.as<string>()); }; return make<bool>(std::ranges::adjacent_find(xs | as_proper_list, compare) == std::ranges::end(xs | as_proper_list)); } },
      { "string-ci=?",                   [](let const& xs) { auto compare = [](let const& s1, let const& s2) { auto compare = [](auto const& c1, auto const& c2) { return c1.downcase() == c2.downcase(); }; return not std::lexicographical_compare(s1.as<string>().characters.begin(), s1.as<string>().characters.end(), s2.as<string>().characters.begin(), s2.as<string>().characters.end(), compare); }; return make<bool>(std::ranges::adjacent_find(xs | as_proper_list, compare) == std::ranges::end(xs | as_proper_list)); } },
      { "string-ci<?",                   [](let const& xs) { auto compare = [](let const& s1, let const& s2) { auto compare = [](auto const& c1, auto const& c2) { return c1.downcase() <  c2.downcase(); }; return not std::lexicographical_compare(s1.as<string>().characters.begin(), s1.as<string>().characters.end(), s2.as<string>().characters.begin(), s2.as<string>().characters.end(), compare); }; return make<bool>(std::ranges::adjacent_find(xs | as_proper_list, compare) == std::ranges::end(xs | as_proper_list)); } },
      { "string-ci>?",                   [](let const& xs) { auto compare = [](let const& s1, let const& s2) { auto compare = [](auto const& c1, auto const& c2) { return c1.downcase() >  c2.downcase(); }; return not std::lexicographical_compare(s1.as<string>().characters.begin(), s1.as<string>().characters.end(), s2.as<string>().characters.begin(), s2.as<string>().characters.end(), compare); }; return make<bool>(std::ranges::adjacent_find(xs | as_proper_list, compare) == std::ranges::end(xs | as_proper_list)); } },
      { "string-ci<=?",                  [](let const& xs) { auto compare = [](let const& s1, let const& s2) { auto compare = [](auto const& c1, auto const& c2) { return c1.downcase() <= c2.downcase(); }; return not std::lexicographical_compare(s1.as<string>().characters.begin(), s1.as<string>().characters.end(), s2.as<string>().characters.begin(), s2.as<string>().characters.end(), compare); }; return make<bool>(std::ranges::adjacent_find(xs | as_proper_list, compare) == std::ranges::end(xs | as_proper_list)); } },
      { "string-ci>=?",                  [](let const& xs) { auto compare = [](let const& s1, let const& s2) { auto compare = [](auto const& c1, auto const& c2) { return c1.downcase() >= c2.downcase(); }; return not std::lexicographical_compare(s1.as<string>().characters.begin(), s1.as<string>().characters.end(), s2.as<string>().characters.begin(), s2.as<string>().characters.end(), compare); }; return make<bool>(std::ranges::adjacent_find(xs | as_proper_list, compare) == std::ranges::end(xs | as_proper_list)); } },
      { "string-append",                 [](let const& xs) { let s = make<string>(); for (let const& x : xs | as_proper_list) { s.as<string>().characters.insert(s.as<string>().characters.end(), x.as<string>().characters.begin(), x.as<string>().characters.end()); } return s; } },
      { "string->list",                  [](let const& xs) { auto push = [](let const& xs, character const& c) { return cons(make<character>(c), xs); }; switch (length(xs)) { case 1: return std::accumulate(car(xs).as<string>().characters.rbegin(), car(xs).as<string>().characters.rend(), unit, push); case 2: return std::accumulate(car(xs).as<string>().characters.rbegin(), std::prev(car(xs).as<string>().characters.rend(), exact_integer_cast<std::size_t>(cadr(xs))), unit, push); case 3: return std::accumulate(std::prev(car(xs).as<string>().characters.rend(), exact_integer_cast<std::size_t>(caddr(xs))), std::prev(car(xs).as<string>().characters.rend(), exact_integer_cast<std::size_t>(cadr(xs))), unit, push); default: throw error(make<string>("procedure string->list takes one to three arugments, but got"), xs); } } },
      { "list->string",                  [](let const& xs) { return make<string>(car(xs) | as_proper_list); } },
      { "string-copy",                   [](let const& xs) { switch (length(xs)) { case 1: return make<string>(car(xs).as<string>().characters.begin(), car(xs).as<string>().characters.end()); case 2: return make<string>(std::next(car(xs).as<string>().characters.begin(), exact_integer_cast<std::size_t>(cadr(xs))), car(xs).as<string>().characters.end()); case 3: return make<string>(std::next(car(xs).as<string>().characters.begin(), exact_integer_cast<std::size_t>(cadr(xs))), std::next(car(xs).as<string>().characters.begin(), exact_integer_cast<std::size_t>(caddr(xs)))); default: throw error(make<string>("procedure string-copy takes one to three arugments, but got"), xs); } } },
      { "string-copy!",                  [](let const& xs) { car(xs).as<string>().characters.reserve(car(xs).as<string>().characters.size() + caddr(xs).as<string>().characters.size()); switch (length(xs)) { case 3: std::copy(caddr(xs).as<string>().characters.begin(), caddr(xs).as<string>().characters.end(), std::next(car(xs).as<string>().characters.begin(), exact_integer_cast<std::size_t>(cadr(xs)))); return unspecified; case 4: std::copy(std::next(caddr(xs).as<string>().characters.begin(), exact_integer_cast<std::size_t>(cadddr(xs))), caddr(xs).as<string>().characters.end(), std::next(car(xs).as<string>().characters.begin(), exact_integer_cast<std::size_t>(cadr(xs)))); return unspecified; case 5: std::copy(std::next(caddr(xs).as<string>().characters.begin(), exact_integer_cast<std::size_t>(cadddr(xs))), std::next(caddr(xs).as<string>().characters.begin(), exact_integer_cast<std::size_t>(caddddr(xs))), std::next(car(xs).as<string>().characters.begin(), exact_integer_cast<std::size_t>(cadr(xs)))); return unspecified; default: throw error(make<string>("procedure string-copy takes three to five arugments, but got"), xs); } } },
      { "string-fill!",                  [](let const& xs) { switch (length(xs)) { case 2: std::fill(car(xs).as_mutable<string>().characters.begin(), car(xs).as_mutable<string>().characters.end(), cadr(xs).as<character>()); return unspecified; case 3: std::fill(std::next(car(xs).as_mutable<string>().characters.begin(), exact_integer_cast<std::size_t>(caddr(xs))), car(xs).as_mutable<string>().characters.end(), cadr(xs).as<character>()); return unspecified; case 4: std::fill(std::next(car(xs).as_mutable<string>().characters.begin(), exact_integer_cast<std::size_t>(caddr(xs))), std::next(car(xs).as_mutable<string>().characters.begin(), exact_integer_cast<std::size_t>(cadddr(xs))), cadr(xs).as<character>()); return unspecified; default: throw error(make<string>("procedure string-fill! takes one to three arugments, but got"), xs); } } },

      { "symbol?",                       [](let const& xs) { return make<bool>(car(xs).is<symbol>()); } },
      { "symbol->string",                [](let const& xs) { return make<string>(car(xs).as<symbol>()); } },
      { "string->symbol",                [](let const& xs) { return make_symbol(car(xs).as<string>().utf8()); } },
      { "identifier->symbol",            [](let const& xs) { if (let const& x = car(xs); x.is<syntactic_closure>()) { return cddr(x); } else { return x; } } },

      { "identifier?",                   [](let const& xs) { return make<bool>(car(xs).is_also<identifier>()); } },
      { "transformer?",                  [](let const& xs) { return make<bool>(car(xs).is_also<transformer>()); } },
      { "syntactic-closure?",            [](let const& xs) { return make<bool>(car(xs).is_also<syntactic_closure>()); } },
      { "make-syntactic-closure",        [](let const& xs) { return make<syntactic_closure>(car(xs), cadr(xs), caddr(xs)); } },

      { "features",                      [](let const&   ) { return features(); } },
      { "get-environment-variable",      [](let const& xs) { if (auto s = std::getenv(car(xs).as<string>().utf8().c_str())) { return make<string>(s); } else { return f; } } },
      { "get-environment-variables",     [](let const&   ) { let alist = nullptr; for (auto iter = environ; *iter; ++iter) { if (auto const position = std::string_view(*iter).find_first_of("="); position != std::string::npos) { alist = alist_cons(make<string>(std::string(*iter, position)), make<string>(std::string(*iter + position + 1)), alist); } } return alist; } },

      { "current-jiffy",                 [](let const&   ) { return make<large_integer>(std::chrono::high_resolution_clock::now().time_since_epoch().count()); } },
      { "jiffies-per-second",            [](let const&   ) { return make<large_integer>(std::chrono::high_resolution_clock::period::den); } },

      { "vector?",                       [](let const& xs) { return make<bool>(car(xs).is<vector>()); } },
      { "vector",                        [](let const& xs) { return make<vector>(xs | as_proper_list); } },
      { "make-vector",                   [](let const& xs) { switch (length(xs)) { case 1: return make<vector>(exact_integer_cast<std::size_t>(car(xs)), unspecified); case 2: return make<vector>(exact_integer_cast<std::size_t>(car(xs)), cadr(xs)); default: throw error(make<string>("procedure make-vector takes one or two arugments, but got"), xs); } } },
      { "vector-length",                 [](let const& xs) { return make<small_integer>(static_cast<small_integer>(car(xs).as<vector>().objects.size())); } },
      { "vector-ref",                    [](let const& xs) { return car(xs).as<vector>().objects[exact_integer_cast<std::size_t>(cadr(xs))]; } },
      { "vector-set!",                   [](let const& xs) { car(xs).as_mutable<vector>().objects[exact_integer_cast<std::size_t>(cadr(xs))] = caddr(xs); return unspecified; } },
      { "vector->list",                  [](let const& xs) { switch (length(xs)) { case 1: return std::accumulate(car(xs).as<vector>().objects.rbegin(), car(xs).as<vector>().objects.rend(), unit, xcons); case 2: return std::accumulate(car(xs).as<vector>().objects.rbegin(), std::prev(car(xs).as<vector>().objects.rend(), exact_integer_cast<std::size_t>(cadr(xs))), unit, xcons); case 3: return std::accumulate(std::prev(car(xs).as<vector>().objects.rend(), exact_integer_cast<std::size_t>(caddr(xs))), std::prev(car(xs).as<vector>().objects.rend(), exact_integer_cast<std::size_t>(cadr(xs))), unit, xcons); default: throw error(make<string>("procedure vector->list takes one to three arugments, but got"), xs); } } },
      { "list->vector",                  [](let const& xs) { return make<vector>(car(xs) | as_proper_list); } },
      { "vector->string",                [](let const& xs) { let s = make<string>(); auto push_back = [&](let const& x) { s.as<string>().characters.push_back(x.as<character>()); }; switch (length(xs)) { case 1: std::for_each(car(xs).as<vector>().objects.begin(), car(xs).as<vector>().objects.end(), push_back); return s; case 2: std::for_each(std::next(car(xs).as<vector>().objects.begin(), exact_integer_cast<std::size_t>(cadr(xs))), car(xs).as<vector>().objects.end(), push_back); return s; case 3: std::for_each(std::next(car(xs).as<vector>().objects.begin(), exact_integer_cast<std::size_t>(cadr(xs))), std::next(car(xs).as<vector>().objects.begin(), exact_integer_cast<std::size_t>(caddr(xs))), push_back); return s; default: throw error(make<string>("procedure vector->list takes one to three arugments, but got"), xs); } } },
      { "string->vector",                [](let const& xs) { let v = make<vector>(); for (auto c : car(xs).as<string>().characters) { v.as<vector>().objects.push_back(make<character>(c)); } return v; } },
      { "vector-copy",                   [](let const& xs) { switch (length(xs)) { case 1: return make<vector>(car(xs).as<vector>().objects.begin(), car(xs).as<vector>().objects.end()); case 2: return make<vector>(std::next(car(xs).as<vector>().objects.begin(), exact_integer_cast<std::size_t>(cadr(xs))), car(xs).as<vector>().objects.end()); case 3: return make<vector>(std::next(car(xs).as<vector>().objects.begin(), exact_integer_cast<std::size_t>(cadr(xs))), std::next(car(xs).as<vector>().objects.begin(), exact_integer_cast<std::size_t>(caddr(xs)))); default: throw error(make<string>("procedure vector-copy takes one to three arugments, but got"), xs); } } },
      { "vector-copy!",                  [](let const& xs) { car(xs).as<vector>().objects.reserve(car(xs).as<vector>().objects.size() + caddr(xs).as<vector>().objects.size()); switch (length(xs)) { case 3: std::copy(caddr(xs).as<vector>().objects.begin(), caddr(xs).as<vector>().objects.end(), std::next(car(xs).as<vector>().objects.begin(), exact_integer_cast<std::size_t>(cadr(xs)))); return unspecified; case 4: std::copy(std::next(caddr(xs).as<vector>().objects.begin(), exact_integer_cast<std::size_t>(cadddr(xs))), caddr(xs).as<vector>().objects.end(), std::next(car(xs).as<vector>().objects.begin(), exact_integer_cast<std::size_t>(cadr(xs)))); return unspecified; case 5: std::copy(std::next(caddr(xs).as<vector>().objects.begin(), exact_integer_cast<std::size_t>(cadddr(xs))), std::next(caddr(xs).as<vector>().objects.begin(), exact_integer_cast<std::size_t>(caddddr(xs))), std::next(car(xs).as<vector>().objects.begin(), exact_integer_cast<std::size_t>(cadr(xs)))); return unspecified; default: throw error(make<string>("procedure vector-copy takes three to five arugments, but got"), xs); } } },
      { "vector-append",                 [](let const& xs) { let v = make<vector>(); for (let const& x : xs | as_proper_list) { v.as<vector>().objects.insert(v.as<vector>().objects.end(), x.as<vector>().objects.begin(), x.as<vector>().objects.end()); } return v; } },
      { "vector-fill!",                  [](let const& xs) { switch (length(xs)) { case 2: std::fill(car(xs).as_mutable<vector>().objects.begin(), car(xs).as_mutable<vector>().objects.end(), cadr(xs)); return unspecified; case 3: std::fill(std::next(car(xs).as_mutable<vector>().objects.begin(), exact_integer_cast<std::size_t>(caddr(xs))), car(xs).as_mutable<vector>().objects.end(), cadr(xs)); return unspecified; case 4: std::fill(std::next(car(xs).as_mutable<vector>().objects.begin(), exact_integer_cast<std::size_t>(caddr(xs))), std::next(car(xs).as_mutable<vector>().objects.begin(), exact_integer_cast<std::size_t>(cadddr(xs))), cadr(xs)); return unspecified; default: return unspecified; } } },

      #define REGISTER_VECTOR(TAG) \
      { #TAG "vector?",                  [](let const& xs) { return make<bool>(car(xs).is<TAG##vector>()); } }, \
      { "make-" #TAG "vector",           [](let const& xs) { switch (length(xs)) { case 1: return make<TAG##vector>(static_cast<TAG##vector::values_type::value_type>(0), exact_integer_cast<std::size_t>(car(xs))); case 2: return make<TAG##vector>(TAG##vector::input_cast(cadr(xs)), exact_integer_cast<std::size_t>(car(xs))); default: throw error(make<string>("procedure make-" #TAG "vector takes one or two arguments, but got"), xs); } } }, \
      { #TAG "vector",                   [](let const& xs) { return make<TAG##vector>(xs | as_proper_list); } }, \
      { #TAG "vector-length",            [](let const& xs) { return make<small_integer>(static_cast<small_integer>(car(xs).as<TAG##vector>().values.size())); } }, \
      { #TAG "vector-ref",               [](let const& xs) { return TAG##vector::output_cast(car(xs).as<TAG##vector>().values[exact_integer_cast<std::size_t>(cadr(xs))]); } }, \
      { #TAG "vector-set!",              [](let const& xs) { car(xs).as<TAG##vector>().values[exact_integer_cast<std::size_t>(cadr(xs))] = TAG##vector::input_cast(caddr(xs)); return unspecified; } }, \
      { #TAG "vector-copy",              [](let const& xs) { auto copy = [&](std::size_t begin, std::size_t end) { assert(begin <= end); return make<TAG##vector>(car(xs).as<TAG##vector>().values[std::slice(begin, end - begin, 1)]); }; switch (length(xs)) { case 1: return copy(0, car(xs).as<TAG##vector>().values.size()); case 2: return copy(exact_integer_cast<std::size_t>(cadr(xs)), car(xs).as<TAG##vector>().values.size()); case 3: return copy(exact_integer_cast<std::size_t>(cadr(xs)), exact_integer_cast<std::size_t>(caddr(xs))); default: throw error(make<string>("procedure " #TAG "vector-copy takes one to three arguments, but got"), xs); } } }, \
      { #TAG "vector-copy!",             [](let const& xs) { auto copy = [&](std::size_t at, std::size_t begin, std::size_t end) { assert(begin <= end); auto i = std::slice(at, end - begin, 1); auto j = std::slice(begin, end - begin, 1); car(xs).as_mutable<TAG##vector>().values[i] = caddr(xs).as<TAG##vector>().values[j]; }; switch (length(xs)) { case 3: copy(exact_integer_cast<std::size_t>(cadr(xs)), 0, caddr(xs).as<TAG##vector>().values.size()); return unspecified; case 4: copy(exact_integer_cast<std::size_t>(cadr(xs)), exact_integer_cast<std::size_t>(cadddr(xs)), caddr(xs).as<TAG##vector>().values.size()); return unspecified; case 5: copy(exact_integer_cast<std::size_t>(cadr(xs)), exact_integer_cast<std::size_t>(cadddr(xs)), exact_integer_cast<std::size_t>(caddddr(xs))); return unspecified; default: throw error(make<string>("procedure " #TAG "vector-copy! takes three to five arguments, but got"), xs); } } }, \
      { #TAG "vector-append",            [](let const& xs) { auto const& a = car(xs).as<TAG##vector>(); auto const& b = cadr(xs).as<TAG##vector>(); let const c = make<TAG##vector>(a.values.size() + b.values.size()); c.as<TAG##vector>().values[std::slice(0, a.values.size(), 1)] = a.values; c.as<TAG##vector>().values[std::slice(a.values.size(), b.values.size(), 1)] = b.values; return c; } }, \
      { #TAG "vector->list",             [](let const& xs) { auto list = [](auto&& v, auto&& a, auto&& b) { auto xcons = [](auto&& x, auto&& y) { return cons(TAG##vector::output_cast(y), x); }; return reverse(std::accumulate(std::next(std::begin(v), a), std::next(std::begin(v), b), unit, xcons)); }; switch (length(xs)) { case 1: return list(car(xs).as<TAG##vector>().values, 0, car(xs).as<TAG##vector>().values.size()); case 2: return list(car(xs).as<TAG##vector>().values, exact_integer_cast<std::size_t>(cadr(xs)), car(xs).as<TAG##vector>().values.size()); case 3: return list(car(xs).as<TAG##vector>().values, exact_integer_cast<std::size_t>(cadr(xs)), exact_integer_cast<std::size_t>(caddr(xs))); default: throw error(make<string>("procedure " #TAG "vector->list takes one to three arguments, but got"), xs); } } }, \
      { "list->" #TAG "vector",          [](let const& xs) { return make<TAG##vector>(car(xs) | as_proper_list); } }

      REGISTER_VECTOR(s8), REGISTER_VECTOR(s16), REGISTER_VECTOR(s32), REGISTER_VECTOR(s64),
      REGISTER_VECTOR(u8), REGISTER_VECTOR(u16), REGISTER_VECTOR(u32), REGISTER_VECTOR(u64),
                                                 REGISTER_VECTOR(f32), REGISTER_VECTOR(f64),

      { "u8vector->string",              [](let const& xs) { auto buffer = std::ostringstream(); auto print = [&](auto const& x) { buffer << x; }; switch (length(xs)) { case 1: std::for_each(std::begin(car(xs).as<u8vector>().values), std::end(car(xs).as<u8vector>().values), print); break; case 2: std::for_each(std::next(std::begin(car(xs).as<u8vector>().values), exact_integer_cast<std::size_t>(cadr(xs))), std::end(car(xs).as<u8vector>().values), print); break; case 3: std::for_each(std::next(std::begin(car(xs).as<u8vector>().values), exact_integer_cast<std::size_t>(cadr(xs))), std::next(std::begin(car(xs).as<u8vector>().values), exact_integer_cast<std::size_t>(caddr(xs))), print); break; default: throw error(make<string>("procedure u8vector->string takes one to three arguments, but got"), xs); } return input_string_port(buffer.str()).get(std::numeric_limits<std::size_t>::max()); } },
      { "string->u8vector",              [](let const& xs) { auto convert = [](std::string const& s) { return make<u8vector>(reinterpret_cast<std::uint8_t const*>(s.data()), s.size()); }; return convert(car(xs).as<string>().utf8()); } },

      { "put-char",                      [](let const& xs) { cadr(xs).as<textual_output_port>().put(car(xs).as<character>()); return unspecified; } },
      { "put-string",                    [](let const& xs) { cadr(xs).as<textual_output_port>().put(car(xs).as<string>()); return unspecified; } },
      { "put-u8",                        [](let const& xs) { cadr(xs).as<binary_output_port>().put(exact_integer_cast<std::uint8_t>(car(xs))); return unspecified; } },
      { "put-u8vector",                  [](let const& xs) { cadr(xs).as<binary_output_port>().put(car(xs).as<u8vector>()); return unspecified; } },
      { "write",                         [](let const& xs) { cadr(xs).as<textual_output_port>().write(car(xs)); return unspecified; } },
      { "write-simple",                  [](let const& xs) { cadr(xs).as<textual_output_port>().write_simple(car(xs)); return unspecified; } },
    };

    if (auto iterator = registry.find(name); iterator != registry.end())
    {
      return reinterpret_cast<void *>(iterator->second);
    }
    else
    {
      return nullptr;
    }
  }

  auto boot() -> void
  {
    libraries().emplace("(meevax binary32)", make<library>([](auto define)
    {
      return list(define(make_symbol("binary32?"), make<procedure>("meevax", "binary32?")));
    }));

    libraries().emplace("(meevax binary64)", make<library>([](auto define)
    {
      return list(define(make_symbol("binary64?"                   ), make<procedure>("meevax", "binary64?"                   )),
                  define(make_symbol("binary64-least"              ), make<double>(std::numeric_limits<double>::min()         )),
                  define(make_symbol("binary64-greatest"           ), make<double>(std::numeric_limits<double>::max()         )),
                  define(make_symbol("binary64-epsilon"            ), make<double>(std::numeric_limits<double>::epsilon()     )),
                  define(make_symbol("binary64-integral-part"      ), make<procedure>("meevax", "binary64-integral-part"      )),
                  define(make_symbol("binary64-fractional-part"    ), make<procedure>("meevax", "binary64-fractional-part"    )),
                  define(make_symbol("binary64-log-binary"         ), make<procedure>("meevax", "binary64-log-binary"         )),
                  define(make_symbol("binary64-integer-log-binary" ), make<procedure>("meevax", "binary64-integer-log-binary" )),
                  define(make_symbol("binary64-normalized-fraction"), make<procedure>("meevax", "binary64-normalized-fraction")),
                  define(make_symbol("binary64-exponent"           ), make<procedure>("meevax", "binary64-exponent"           )),
                  define(make_symbol("binary64-sign-bit"           ), make<procedure>("meevax", "binary64-sign-bit"           )),
                  define(make_symbol("binary64-normalized?"        ), make<procedure>("meevax", "binary64-normalized?"        )),
                  define(make_symbol("binary64-denormalized?"      ), make<procedure>("meevax", "binary64-denormalized?"      )),
                  define(make_symbol("binary64-max"                ), make<procedure>("meevax", "binary64-max"                )),
                  define(make_symbol("binary64-min"                ), make<procedure>("meevax", "binary64-min"                )),
                  define(make_symbol("binary64-fused-multiply-add" ), make<procedure>("meevax", "binary64-fused-multiply-add" )),
                  define(make_symbol("binary64-remquo"             ), make<procedure>("meevax", "binary64-remquo"             )));
    }));

    libraries().emplace("(meevax boolean)", make<library>([](auto define)
    {
      return list(define(make_symbol("boolean?"), make<procedure>("meevax", "boolean?")),
                  define(make_symbol("not"     ), make<procedure>("meevax", "not"     )));
    }));

    libraries().emplace("(meevax box)", make<library>([](auto define)
    {
      return list(define(make_symbol("box"     ), make<procedure>("meevax", "box"     )),
                  define(make_symbol("box-ref" ), make<procedure>("meevax", "box-ref" )),
                  define(make_symbol("box-set!"), make<procedure>("meevax", "box-set!")),
                  define(make_symbol("box?"    ), make<procedure>("meevax", "box?"    )));
    }));

    libraries().emplace("(meevax character)", make<library>([](auto define)
    {
      return list(define(make_symbol("char?"           ), make<procedure>("meevax", "char?"           )),
                  define(make_symbol("char=?"          ), make<procedure>("meevax", "char=?"          )),
                  define(make_symbol("char<?"          ), make<procedure>("meevax", "char<?"          )),
                  define(make_symbol("char>?"          ), make<procedure>("meevax", "char>?"          )),
                  define(make_symbol("char<=?"         ), make<procedure>("meevax", "char<=?"         )),
                  define(make_symbol("char>=?"         ), make<procedure>("meevax", "char>=?"         )),
                  define(make_symbol("char-ci=?"       ), make<procedure>("meevax", "char-ci=?"       )),
                  define(make_symbol("char-ci<?"       ), make<procedure>("meevax", "char-ci<?"       )),
                  define(make_symbol("char-ci>?"       ), make<procedure>("meevax", "char-ci>?"       )),
                  define(make_symbol("char-ci<=?"      ), make<procedure>("meevax", "char-ci<=?"      )),
                  define(make_symbol("char-ci>=?"      ), make<procedure>("meevax", "char-ci>=?"      )),
                  define(make_symbol("char-alphabetic?"), make<procedure>("meevax", "char-alphabetic?")),
                  define(make_symbol("char-numeric?"   ), make<procedure>("meevax", "char-numeric?"   )),
                  define(make_symbol("char-whitespace?"), make<procedure>("meevax", "char-whitespace?")),
                  define(make_symbol("char-upper-case?"), make<procedure>("meevax", "char-upper-case?")),
                  define(make_symbol("char-lower-case?"), make<procedure>("meevax", "char-lower-case?")),
                  define(make_symbol("digit-value"     ), make<procedure>("meevax", "digit-value"     )),
                  define(make_symbol("char->integer"   ), make<procedure>("meevax", "char->integer"   )),
                  define(make_symbol("integer->char"   ), make<procedure>("meevax", "integer->char"   )),
                  define(make_symbol("char-upcase"     ), make<procedure>("meevax", "char-upcase"     )),
                  define(make_symbol("char-downcase"   ), make<procedure>("meevax", "char-downcase"   )));
    }));

    libraries().emplace("(meevax context)", make<library>([](auto define)
    {
      return list(define(make_symbol("emergency-exit"), make<procedure>("meevax", "emergency-exit")),
                  define(make_symbol("command-line"  ), make<procedure>("meevax", "command-line"  )));
    }));

    libraries().emplace("(meevax comparator)", make<library>([](auto define)
    {
      return list(define(make_symbol("eq?"),    make<procedure>("meevax", "eq?"   )),
                  define(make_symbol("eqv?"),   make<procedure>("meevax", "eqv?"  )),
                  define(make_symbol("equal?"), make<procedure>("meevax", "equal?")));
    }));

    libraries().emplace("(meevax core)", make<library>([](auto define)
    {
      for (let const& binding : core_syntactic_environment().as<syntactic_environment>().second | as_proper_list)
      {
        define(car(binding), cdr(binding));
      }

      return map(car, core_syntactic_environment().as<syntactic_environment>().second);
    }));

    libraries().emplace("(meevax environment)", make<library>([](auto define)
    {
      return list(define(make_symbol("environment"            ), make<procedure>("meevax", "environment"            )),
                  define(make_symbol("eval"                   ), make<procedure>("meevax", "eval"                   )),
                  define(make_symbol("expand"                 ), make<procedure>("meevax", "expand"                 )),
                  define(make_symbol("interaction-environment"), make<procedure>("meevax", "interaction-environment")),
                  define(make_symbol("load"                   ), make<procedure>("meevax", "load"                   )));
    }));

    libraries().emplace("(meevax error)", make<library>([](auto define)
    {
      return list(define(make_symbol("throw"                        ), make<procedure>("meevax", "throw"                        )),
                  define(make_symbol("error-object"                 ), make<procedure>("meevax", "error-object"                 )),
                  define(make_symbol("error-object?"                ), make<procedure>("meevax", "error-object?"                )),
                  define(make_symbol("read-error?"                  ), make<procedure>("meevax", "read-error?"                  )),
                  define(make_symbol("file-error?"                  ), make<procedure>("meevax", "file-error?"                  )),
                  define(make_symbol("kernel-exception-handler-set!"), make<procedure>("meevax", "kernel-exception-handler-set!")));
    }));

    libraries().emplace("(meevax file)", make<library>([](auto define)
    {
      return list(define(make_symbol("file-exists?"       ), make<procedure>("meevax", "file-exists?"       )),
                  define(make_symbol("delete-file"        ), make<procedure>("meevax", "delete-file"        )),
                  define(make_symbol("library-directories"), make<procedure>("meevax", "library-directories")));
    }));

    libraries().emplace("(meevax instruction)", make<library>([](auto define)
    {
      return list(define(make_symbol("secd-call"),              make<instruction>(instruction::secd_call             )),
                  define(make_symbol("secd-cons"),              make<instruction>(instruction::secd_cons             )),
                  define(make_symbol("secd-current"),           make<instruction>(instruction::secd_current          )),
                  define(make_symbol("secd-drop"),              make<instruction>(instruction::secd_drop             )),
                  define(make_symbol("secd-dummy"),             make<instruction>(instruction::secd_dummy            )),
                  define(make_symbol("secd-install"),           make<instruction>(instruction::secd_install          )),
                  define(make_symbol("secd-join"),              make<instruction>(instruction::secd_join             )),
                  define(make_symbol("secd-letrec"),            make<instruction>(instruction::secd_letrec           )),
                  define(make_symbol("secd-load-absolute"),     make<instruction>(instruction::secd_load_absolute    )),
                  define(make_symbol("secd-load-closure"),      make<instruction>(instruction::secd_load_closure     )),
                  define(make_symbol("secd-load-constant"),     make<instruction>(instruction::secd_load_constant    )),
                  define(make_symbol("secd-load-continuation"), make<instruction>(instruction::secd_load_continuation)),
                  define(make_symbol("secd-load-relative"),     make<instruction>(instruction::secd_load_relative    )),
                  define(make_symbol("secd-load-variadic"),     make<instruction>(instruction::secd_load_variadic    )),
                  define(make_symbol("secd-return"),            make<instruction>(instruction::secd_return           )),
                  define(make_symbol("secd-select"),            make<instruction>(instruction::secd_select           )),
                  define(make_symbol("secd-stop"),              make<instruction>(instruction::secd_stop             )),
                  define(make_symbol("secd-store-absolute"),    make<instruction>(instruction::secd_store_absolute   )),
                  define(make_symbol("secd-store-relative"),    make<instruction>(instruction::secd_store_relative   )),
                  define(make_symbol("secd-store-variadic"),    make<instruction>(instruction::secd_store_variadic   )),
                  define(make_symbol("secd-tail-call"),         make<instruction>(instruction::secd_tail_call        )),
                  define(make_symbol("secd-tail-letrec"),       make<instruction>(instruction::secd_tail_letrec      )),
                  define(make_symbol("secd-tail-select"),       make<instruction>(instruction::secd_tail_select      )));
    }));

    libraries().emplace("(meevax integer32)", make<library>([](auto define)
    {
      return list(define(make_symbol("integer32?"     ), make<procedure>("meevax", "integer32?")),
                  define(make_symbol("integer32-width"), make<small_integer>(32)),
                  define(make_symbol("integer32-min"  ), make<small_integer>(std::numeric_limits<small_integer>::min())),
                  define(make_symbol("integer32-max"  ), make<small_integer>(std::numeric_limits<small_integer>::max())));
    }));

    libraries().emplace("(meevax list)", make<library>([](auto define)
    {
      return list(define(make_symbol("null?"          ), make<procedure>("meevax", "null?"          )),
                  define(make_symbol("list?"          ), make<procedure>("meevax", "list?"          )),
                  define(make_symbol("list"           ), make<procedure>("meevax", "list"           )),
                  define(make_symbol("make-list"      ), make<procedure>("meevax", "make-list"      )),
                  define(make_symbol("iota"           ), make<procedure>("meevax", "iota"           )),
                  define(make_symbol("circular-list?" ), make<procedure>("meevax", "circular-list?" )),
                  define(make_symbol("circular-list"  ), make<procedure>("meevax", "circular-list"  )),
                  define(make_symbol("dotted-list?"   ), make<procedure>("meevax", "dotted-list?"   )),
                  define(make_symbol("null-list?"     ), make<procedure>("meevax", "null-list?"     )),
                  define(make_symbol("last"           ), make<procedure>("meevax", "last"           )),
                  define(make_symbol("last-pair"      ), make<procedure>("meevax", "last-pair"      )),
                  define(make_symbol("length"         ), make<procedure>("meevax", "length"         )),
                  define(make_symbol("length+"        ), make<procedure>("meevax", "length+"        )),
                  define(make_symbol("append"         ), make<procedure>("meevax", "append"         )),
                  define(make_symbol("append!"        ), make<procedure>("meevax", "append!"        )),
                  define(make_symbol("append-reverse" ), make<procedure>("meevax", "append-reverse" )),
                  define(make_symbol("append-reverse!"), make<procedure>("meevax", "append-reverse!")),
                  define(make_symbol("reverse"        ), make<procedure>("meevax", "reverse"        )),
                  define(make_symbol("reverse!"       ), make<procedure>("meevax", "reverse!"       )),
                  define(make_symbol("concatenate"    ), make<procedure>("meevax", "concatenate"    )),
                  define(make_symbol("concatenate!"   ), make<procedure>("meevax", "concatenate!"   )),
                  define(make_symbol("list-copy"      ), make<procedure>("meevax", "list-copy"      )),
                  define(make_symbol("list-ref"       ), make<procedure>("meevax", "list-ref"       )),
                  define(make_symbol("list-tail"      ), make<procedure>("meevax", "list-tail"      )),
                  define(make_symbol("first"          ), make<procedure>("meevax", "first"          )),
                  define(make_symbol("second"         ), make<procedure>("meevax", "second"         )),
                  define(make_symbol("third"          ), make<procedure>("meevax", "third"          )),
                  define(make_symbol("fourth"         ), make<procedure>("meevax", "fourth"         )),
                  define(make_symbol("fifth"          ), make<procedure>("meevax", "fifth"          )),
                  define(make_symbol("sixth"          ), make<procedure>("meevax", "sixth"          )),
                  define(make_symbol("seventh"        ), make<procedure>("meevax", "seventh"        )),
                  define(make_symbol("eighth"         ), make<procedure>("meevax", "eighth"         )),
                  define(make_symbol("ninth"          ), make<procedure>("meevax", "ninth"          )),
                  define(make_symbol("tenth"          ), make<procedure>("meevax", "tenth"          )),
                  define(make_symbol("take"           ), make<procedure>("meevax", "take"           )),
                  define(make_symbol("take!"          ), make<procedure>("meevax", "take!"          )),
                  define(make_symbol("take-right"     ), make<procedure>("meevax", "take-right"     )),
                  define(make_symbol("drop"           ), make<procedure>("meevax", "drop"           )),
                  define(make_symbol("drop-right"     ), make<procedure>("meevax", "drop-right"     )),
                  define(make_symbol("drop-right!"    ), make<procedure>("meevax", "drop-right!"    )),
                  define(make_symbol("memq"           ), make<procedure>("meevax", "memq"           )),
                  define(make_symbol("memv"           ), make<procedure>("meevax", "memv"           )),
                  define(make_symbol("assq"           ), make<procedure>("meevax", "assq"           )),
                  define(make_symbol("assv"           ), make<procedure>("meevax", "assv"           )),
                  define(make_symbol("alist-cons"     ), make<procedure>("meevax", "alist-cons"     )),
                  define(make_symbol("alist-copy"     ), make<procedure>("meevax", "alist-copy"     )));
    }));

    libraries().emplace("(meevax number)", make<library>([](auto define)
    {
      return list(define(make_symbol("number?"                  ), make<procedure>("meevax", "number?"                  )),
                  define(make_symbol("complex?"                 ), make<procedure>("meevax", "complex?"                 )),
                  define(make_symbol("real?"                    ), make<procedure>("meevax", "real?"                    )),
                  define(make_symbol("rational?"                ), make<procedure>("meevax", "rational?"                )),
                  define(make_symbol("integer?"                 ), make<procedure>("meevax", "integer?"                 )),
                  define(make_symbol("exact?"                   ), make<procedure>("meevax", "exact?"                   )),
                  define(make_symbol("inexact?"                 ), make<procedure>("meevax", "inexact?"                 )),
                  define(make_symbol("exact-integer?"           ), make<procedure>("meevax", "exact-integer?"           )),
                  define(make_symbol("finite?"                  ), make<procedure>("meevax", "finite?"                  )),
                  define(make_symbol("infinite?"                ), make<procedure>("meevax", "infinite?"                )),
                  define(make_symbol("nan?"                     ), make<procedure>("meevax", "nan?"                     )),
                  define(make_symbol("="                        ), make<procedure>("meevax", "="                        )),
                  define(make_symbol("<"                        ), make<procedure>("meevax", "<"                        )),
                  define(make_symbol("<="                       ), make<procedure>("meevax", "<="                       )),
                  define(make_symbol(">"                        ), make<procedure>("meevax", ">"                        )),
                  define(make_symbol(">="                       ), make<procedure>("meevax", ">="                       )),
                  define(make_symbol("zero?"                    ), make<procedure>("meevax", "zero?"                    )),
                  define(make_symbol("positive?"                ), make<procedure>("meevax", "positive?"                )),
                  define(make_symbol("negative?"                ), make<procedure>("meevax", "negative?"                )),
                  define(make_symbol("odd?"                     ), make<procedure>("meevax", "odd?"                     )),
                  define(make_symbol("even?"                    ), make<procedure>("meevax", "even?"                    )),
                  define(make_symbol("max"                      ), make<procedure>("meevax", "max"                      )),
                  define(make_symbol("min"                      ), make<procedure>("meevax", "min"                      )),
                  define(make_symbol("+"                        ), make<procedure>("meevax", "+"                        )),
                  define(make_symbol("*"                        ), make<procedure>("meevax", "*"                        )),
                  define(make_symbol("-"                        ), make<procedure>("meevax", "-"                        )),
                  define(make_symbol("/"                        ), make<procedure>("meevax", "/"                        )),
                  define(make_symbol("abs"                      ), make<procedure>("meevax", "abs"                      )),
                  define(make_symbol("quotient"                 ), make<procedure>("meevax", "quotient"                 )),
                  define(make_symbol("remainder"                ), make<procedure>("meevax", "remainder"                )),
                  define(make_symbol("modulo"                   ), make<procedure>("meevax", "modulo"                   )),
                  define(make_symbol("gcd"                      ), make<procedure>("meevax", "gcd"                      )),
                  define(make_symbol("lcm"                      ), make<procedure>("meevax", "lcm"                      )),
                  define(make_symbol("numerator"                ), make<procedure>("meevax", "numerator"                )),
                  define(make_symbol("denominator"              ), make<procedure>("meevax", "denominator"              )),
                  define(make_symbol("floor"                    ), make<procedure>("meevax", "floor"                    )),
                  define(make_symbol("ceiling"                  ), make<procedure>("meevax", "ceiling"                  )),
                  define(make_symbol("truncate"                 ), make<procedure>("meevax", "truncate"                 )),
                  define(make_symbol("round"                    ), make<procedure>("meevax", "round"                    )),
                  define(make_symbol("exp"                      ), make<procedure>("meevax", "exp"                      )),
                  define(make_symbol("log"                      ), make<procedure>("meevax", "log"                      )),
                  define(make_symbol("sin"                      ), make<procedure>("meevax", "sin"                      )),
                  define(make_symbol("cos"                      ), make<procedure>("meevax", "cos"                      )),
                  define(make_symbol("tan"                      ), make<procedure>("meevax", "tan"                      )),
                  define(make_symbol("asin"                     ), make<procedure>("meevax", "asin"                     )),
                  define(make_symbol("acos"                     ), make<procedure>("meevax", "acos"                     )),
                  define(make_symbol("atan"                     ), make<procedure>("meevax", "atan"                     )),
                  define(make_symbol("sinh"                     ), make<procedure>("meevax", "sinh"                     )),
                  define(make_symbol("cosh"                     ), make<procedure>("meevax", "cosh"                     )),
                  define(make_symbol("tanh"                     ), make<procedure>("meevax", "tanh"                     )),
                  define(make_symbol("asinh"                    ), make<procedure>("meevax", "asinh"                    )),
                  define(make_symbol("acosh"                    ), make<procedure>("meevax", "acosh"                    )),
                  define(make_symbol("atanh"                    ), make<procedure>("meevax", "atanh"                    )),
                  define(make_symbol("sqrt"                     ), make<procedure>("meevax", "sqrt"                     )),
                  define(make_symbol("exact-integer-square-root"), make<procedure>("meevax", "exact-integer-square-root")),
                  define(make_symbol("expt"                     ), make<procedure>("meevax", "expt"                     )),
                  define(make_symbol("make-rectangular"         ), make<procedure>("meevax", "make-rectangular"         )),
                  define(make_symbol("make-polar"               ), make<procedure>("meevax", "make-polar"               )),
                  define(make_symbol("real-part"                ), make<procedure>("meevax", "real-part"                )),
                  define(make_symbol("imag-part"                ), make<procedure>("meevax", "imag-part"                )),
                  define(make_symbol("magnitude"                ), make<procedure>("meevax", "magnitude"                )),
                  define(make_symbol("angle"                    ), make<procedure>("meevax", "angle"                    )),
                  define(make_symbol("exact"                    ), make<procedure>("meevax", "exact"                    )),
                  define(make_symbol("inexact"                  ), make<procedure>("meevax", "inexact"                  )),
                  define(make_symbol("expm1"                    ), make<procedure>("meevax", "expm1"                    )),
                  define(make_symbol("log1p"                    ), make<procedure>("meevax", "log1p"                    )),
                  define(make_symbol("erf"                      ), make<procedure>("meevax", "erf"                      )),
                  define(make_symbol("erfc"                     ), make<procedure>("meevax", "erfc"                     )),
                  define(make_symbol("tgamma"                   ), make<procedure>("meevax", "tgamma"                   )),
                  define(make_symbol("lgamma"                   ), make<procedure>("meevax", "lgamma"                   )),
                  define(make_symbol("ldexp"                    ), make<procedure>("meevax", "ldexp"                    )),
                  define(make_symbol("nextafter"                ), make<procedure>("meevax", "nextafter"                )),
                  define(make_symbol("copysign"                 ), make<procedure>("meevax", "copysign"                 )),
                  define(make_symbol("cyl_bessel_j"             ), make<procedure>("meevax", "cyl_bessel_j"             )),
                  define(make_symbol("cyl_neumann"              ), make<procedure>("meevax", "cyl_neumann"              )),
                  define(make_symbol("e"                        ), make<double>(std::numbers::e)),
                  define(make_symbol("pi"                       ), make<double>(std::numbers::pi)),
                  define(make_symbol("euler"                    ), make<double>(std::numbers::egamma)),
                  define(make_symbol("phi"                      ), make<double>(std::numbers::phi)),
                  define(make_symbol("bitwise-not"              ), make<procedure>("meevax", "bitwise-not"              )),
                  define(make_symbol("bitwise-and"              ), make<procedure>("meevax", "bitwise-and"              )),
                  define(make_symbol("bitwise-ior"              ), make<procedure>("meevax", "bitwise-ior"              )),
                  define(make_symbol("bitwise-xor"              ), make<procedure>("meevax", "bitwise-xor"              )),
                  define(make_symbol("bit-shift"                ), make<procedure>("meevax", "bit-shift"                )),
                  define(make_symbol("bit-count"                ), make<procedure>("meevax", "bit-count"                )),
                  define(make_symbol("bit-width"                ), make<procedure>("meevax", "bit-width"                )),
                  define(make_symbol("number->string"           ), make<procedure>("meevax", "number->string"           )),
                  define(make_symbol("string->number"           ), make<procedure>("meevax", "string->number"           )));
    }));

    libraries().emplace("(meevax pair)", make<library>([](auto define)
    {
      return list(define(make_symbol("pair?"    ), make<procedure>("meevax", "pair?"    )),
                  define(make_symbol("not-pair?"), make<procedure>("meevax", "not-pair?")),
                  define(make_symbol("cons"     ), make<procedure>("meevax", "cons"     )),
                  define(make_symbol("xcons"    ), make<procedure>("meevax", "xcons"    )),
                  define(make_symbol("cons*"    ), make<procedure>("meevax", "cons*"    )),
                  define(make_symbol("car"      ), make<procedure>("meevax", "car"      )),
                  define(make_symbol("cdr"      ), make<procedure>("meevax", "cdr"      )),
                  define(make_symbol("caar"     ), make<procedure>("meevax", "caar"     )),
                  define(make_symbol("cadr"     ), make<procedure>("meevax", "cadr"     )),
                  define(make_symbol("cdar"     ), make<procedure>("meevax", "cdar"     )),
                  define(make_symbol("cddr"     ), make<procedure>("meevax", "cddr"     )),
                  define(make_symbol("caaar"    ), make<procedure>("meevax", "caaar"    )),
                  define(make_symbol("cdaar"    ), make<procedure>("meevax", "cdaar"    )),
                  define(make_symbol("caadr"    ), make<procedure>("meevax", "caadr"    )),
                  define(make_symbol("cdadr"    ), make<procedure>("meevax", "cdadr"    )),
                  define(make_symbol("cadar"    ), make<procedure>("meevax", "cadar"    )),
                  define(make_symbol("cddar"    ), make<procedure>("meevax", "cddar"    )),
                  define(make_symbol("caddr"    ), make<procedure>("meevax", "caddr"    )),
                  define(make_symbol("cdddr"    ), make<procedure>("meevax", "cdddr"    )),
                  define(make_symbol("caaaar"   ), make<procedure>("meevax", "caaaar"   )),
                  define(make_symbol("cdaaar"   ), make<procedure>("meevax", "cdaaar"   )),
                  define(make_symbol("caaadr"   ), make<procedure>("meevax", "caaadr"   )),
                  define(make_symbol("cdaadr"   ), make<procedure>("meevax", "cdaadr"   )),
                  define(make_symbol("caadar"   ), make<procedure>("meevax", "caadar"   )),
                  define(make_symbol("cdadar"   ), make<procedure>("meevax", "cdadar"   )),
                  define(make_symbol("caaddr"   ), make<procedure>("meevax", "caaddr"   )),
                  define(make_symbol("cdaddr"   ), make<procedure>("meevax", "cdaddr"   )),
                  define(make_symbol("cadaar"   ), make<procedure>("meevax", "cadaar"   )),
                  define(make_symbol("cddaar"   ), make<procedure>("meevax", "cddaar"   )),
                  define(make_symbol("cadadr"   ), make<procedure>("meevax", "cadadr"   )),
                  define(make_symbol("cddadr"   ), make<procedure>("meevax", "cddadr"   )),
                  define(make_symbol("caddar"   ), make<procedure>("meevax", "caddar"   )),
                  define(make_symbol("cdddar"   ), make<procedure>("meevax", "cdddar"   )),
                  define(make_symbol("cadddr"   ), make<procedure>("meevax", "cadddr"   )),
                  define(make_symbol("cddddr"   ), make<procedure>("meevax", "cddddr"   )),
                  define(make_symbol("set-car!" ), make<procedure>("meevax", "set-car!" )),
                  define(make_symbol("set-cdr!" ), make<procedure>("meevax", "set-cdr!" )));
    }));

    libraries().emplace("(meevax port)", make<library>([](auto define)
    {
      return list(define(make_symbol("input-port?"            ), make<procedure>("meevax", "input-port?"            )),
                  define(make_symbol("output-port?"           ), make<procedure>("meevax", "output-port?"           )),
                  define(make_symbol("binary-port?"           ), make<procedure>("meevax", "binary-port?"           )),
                  define(make_symbol("textual-port?"          ), make<procedure>("meevax", "textual-port?"          )),
                  define(make_symbol("port?"                  ), make<procedure>("meevax", "port?"                  )),
                  define(make_symbol("open?"                  ), make<procedure>("meevax", "open?"                  )),
                  define(make_symbol("standard-input-port"    ), make<procedure>("meevax", "standard-input-port"    )),
                  define(make_symbol("standard-output-port"   ), make<procedure>("meevax", "standard-output-port"   )),
                  define(make_symbol("standard-error-port"    ), make<procedure>("meevax", "standard-error-port"    )),
                  define(make_symbol("open-input-file"        ), make<procedure>("meevax", "open-input-file"        )),
                  define(make_symbol("open-output-file"       ), make<procedure>("meevax", "open-output-file"       )),
                  define(make_symbol("open-binary-input-file" ), make<procedure>("meevax", "open-binary-input-file" )),
                  define(make_symbol("open-binary-output-file"), make<procedure>("meevax", "open-binary-output-file")),
                  define(make_symbol("close"                  ), make<procedure>("meevax", "close"                  )),
                  define(make_symbol("open-input-string"      ), make<procedure>("meevax", "open-input-string"      )),
                  define(make_symbol("open-output-string"     ), make<procedure>("meevax", "open-output-string"     )),
                  define(make_symbol("get-output-string"      ), make<procedure>("meevax", "get-output-string"      )),
                  define(make_symbol("open-input-u8vector"    ), make<procedure>("meevax", "open-input-u8vector"    )),
                  define(make_symbol("open-output-u8vector"   ), make<procedure>("meevax", "open-output-u8vector"   )),
                  define(make_symbol("get-output-u8vector"    ), make<procedure>("meevax", "get-output-u8vector"    )),
                  define(make_symbol("eof-object?"            ), make<procedure>("meevax", "eof-object?"            )),
                  define(make_symbol("eof-object"             ), make<procedure>("meevax", "eof-object"             )),
                  define(make_symbol("flush"                  ), make<procedure>("meevax", "flush"                  )));
    }));

    libraries().emplace("(meevax procedure)", make<library>([](auto define)
    {
      return list(define(make_symbol("closure?"     ), make<procedure>("meevax", "closure?"     )),
                  define(make_symbol("continuation?"), make<procedure>("meevax", "continuation?")),
                  define(make_symbol("procedure?"   ), make<procedure>("meevax", "procedure?"   )),
                  define(make_symbol("procedure"    ), make<procedure>("meevax", "procedure"    )));
    }));

    libraries().emplace("(meevax read)", make<library>([](auto define)
    {
      return list(define(make_symbol("get-char"       ), make<procedure>("meevax", "get-char"       )),
                  define(make_symbol("get-char-ready?"), make<procedure>("meevax", "get-char-ready?")),
                  define(make_symbol("get-line"       ), make<procedure>("meevax", "get-line"       )),
                  define(make_symbol("peek-char"      ), make<procedure>("meevax", "peek-char"      )),
                  define(make_symbol("read"           ), make<procedure>("meevax", "read"           )),
                  define(make_symbol("get-string"     ), make<procedure>("meevax", "get-string"     )),
                  define(make_symbol("get-u8"         ), make<procedure>("meevax", "get-u8"         )),
                  define(make_symbol("get-u8-ready?"  ), make<procedure>("meevax", "get-u8-ready?"  )),
                  define(make_symbol("peek-u8"        ), make<procedure>("meevax", "peek-u8"        )),
                  define(make_symbol("get-u8vector"   ), make<procedure>("meevax", "get-u8vector"   )));
    }));

    libraries().emplace("(meevax string)", make<library>([](auto define)
    {
      return list(define(make_symbol("string?"      ), make<procedure>("meevax", "string?"      )),
                  define(make_symbol("make-string"  ), make<procedure>("meevax", "make-string"  )),
                  define(make_symbol("string"       ), make<procedure>("meevax", "string"       )),
                  define(make_symbol("string-length"), make<procedure>("meevax", "string-length")),
                  define(make_symbol("string-ref"   ), make<procedure>("meevax", "string-ref"   )),
                  define(make_symbol("string-set!"  ), make<procedure>("meevax", "string-set!"  )),
                  define(make_symbol("string=?"     ), make<procedure>("meevax", "string=?"     )),
                  define(make_symbol("string<?"     ), make<procedure>("meevax", "string<?"     )),
                  define(make_symbol("string>?"     ), make<procedure>("meevax", "string>?"     )),
                  define(make_symbol("string<=?"    ), make<procedure>("meevax", "string<=?"    )),
                  define(make_symbol("string>=?"    ), make<procedure>("meevax", "string>=?"    )),
                  define(make_symbol("string-ci=?"  ), make<procedure>("meevax", "string-ci=?"  )),
                  define(make_symbol("string-ci<?"  ), make<procedure>("meevax", "string-ci<?"  )),
                  define(make_symbol("string-ci>?"  ), make<procedure>("meevax", "string-ci>?"  )),
                  define(make_symbol("string-ci<=?" ), make<procedure>("meevax", "string-ci<=?" )),
                  define(make_symbol("string-ci>=?" ), make<procedure>("meevax", "string-ci>=?" )),
                  define(make_symbol("string-append"), make<procedure>("meevax", "string-append")),
                  define(make_symbol("string->list" ), make<procedure>("meevax", "string->list" )),
                  define(make_symbol("list->string" ), make<procedure>("meevax", "list->string" )),
                  define(make_symbol("string-copy"  ), make<procedure>("meevax", "string-copy"  )),
                  define(make_symbol("string-copy!" ), make<procedure>("meevax", "string-copy!" )),
                  define(make_symbol("string-fill!" ), make<procedure>("meevax", "string-fill!" )));
    }));

    libraries().emplace("(meevax symbol)", make<library>([](auto define)
    {
      return list(define(make_symbol("symbol?"           ), make<procedure>("meevax", "symbol?"           )),
                  define(make_symbol("symbol->string"    ), make<procedure>("meevax", "symbol->string"    )),
                  define(make_symbol("string->symbol"    ), make<procedure>("meevax", "string->symbol"    )),
                  define(make_symbol("identifier->symbol"), make<procedure>("meevax", "identifier->symbol")));
    }));

    libraries().emplace("(meevax syntactic-closure)", make<library>([](auto define)
    {
      return list(define(make_symbol("identifier?"           ), make<procedure>("meevax", "identifier?"           )),
                  define(make_symbol("transformer?"          ), make<procedure>("meevax", "transformer?"          )),
                  define(make_symbol("syntactic-closure?"    ), make<procedure>("meevax", "syntactic-closure?"    )),
                  define(make_symbol("make-syntactic-closure"), make<procedure>("meevax", "make-syntactic-closure")));
    }));

    libraries().emplace("(meevax system)", make<library>([](auto define)
    {
      return list(define(make_symbol("features"                 ), make<procedure>("meevax", "features"                 )),
                  define(make_symbol("get-environment-variable" ), make<procedure>("meevax", "get-environment-variable" )),
                  define(make_symbol("get-environment-variables"), make<procedure>("meevax", "get-environment-variables")));
    }));

    libraries().emplace("(meevax time)", make<library>([](auto define)
    {
      return list(define(make_symbol("current-jiffy"     ), make<procedure>("meevax", "current-jiffy"     )),
                  define(make_symbol("jiffies-per-second"), make<procedure>("meevax", "jiffies-per-second")));
    }));

    libraries().emplace("(meevax vector)", make<library>([](auto define)
    {
      return list(define(make_symbol("vector?"       ), make<procedure>("meevax", "vector?"       )),
                  define(make_symbol("vector"        ), make<procedure>("meevax", "vector"        )),
                  define(make_symbol("make-vector"   ), make<procedure>("meevax", "make-vector"   )),
                  define(make_symbol("vector-length" ), make<procedure>("meevax", "vector-length" )),
                  define(make_symbol("vector-ref"    ), make<procedure>("meevax", "vector-ref"    )),
                  define(make_symbol("vector-set!"   ), make<procedure>("meevax", "vector-set!"   )),
                  define(make_symbol("vector->list"  ), make<procedure>("meevax", "vector->list"  )),
                  define(make_symbol("list->vector"  ), make<procedure>("meevax", "list->vector"  )),
                  define(make_symbol("vector->string"), make<procedure>("meevax", "vector->string")),
                  define(make_symbol("string->vector"), make<procedure>("meevax", "string->vector")),
                  define(make_symbol("vector-copy"   ), make<procedure>("meevax", "vector-copy"   )),
                  define(make_symbol("vector-copy!"  ), make<procedure>("meevax", "vector-copy!"  )),
                  define(make_symbol("vector-append" ), make<procedure>("meevax", "vector-append" )),
                  define(make_symbol("vector-fill!"  ), make<procedure>("meevax", "vector-fill!"  )));
    }));

    libraries().emplace("(meevax vector homogeneous)", make<library>([](auto define)
    {
      #define DEFINE_VECTOR(TAG)                                               \
      define(make_symbol(#TAG "vector?"        ), make<procedure>("meevax", #TAG "vector?"        )), \
      define(make_symbol("make-" #TAG "vector" ), make<procedure>("meevax", "make-" #TAG "vector" )), \
      define(make_symbol(#TAG "vector"         ), make<procedure>("meevax", #TAG "vector"         )), \
      define(make_symbol(#TAG "vector-length"  ), make<procedure>("meevax", #TAG "vector-length"  )), \
      define(make_symbol(#TAG "vector-ref"     ), make<procedure>("meevax", #TAG "vector-ref"     )), \
      define(make_symbol(#TAG "vector-set!"    ), make<procedure>("meevax", #TAG "vector-set!"    )), \
      define(make_symbol(#TAG "vector-copy"    ), make<procedure>("meevax", #TAG "vector-copy"    )), \
      define(make_symbol(#TAG "vector-copy!"   ), make<procedure>("meevax", #TAG "vector-copy!"   )), \
      define(make_symbol(#TAG "vector-append"  ), make<procedure>("meevax", #TAG "vector-append"  )), \
      define(make_symbol(#TAG "vector->list"   ), make<procedure>("meevax", #TAG "vector->list"   )), \
      define(make_symbol("list->" #TAG "vector"), make<procedure>("meevax", "list->" #TAG "vector"))

      return list(DEFINE_VECTOR(s8), DEFINE_VECTOR(s16), DEFINE_VECTOR(s32), DEFINE_VECTOR(s64),
                  DEFINE_VECTOR(u8), DEFINE_VECTOR(u16), DEFINE_VECTOR(u32), DEFINE_VECTOR(u64),
                                                         DEFINE_VECTOR(f32), DEFINE_VECTOR(f64),
                  define(make_symbol("u8vector->string"), make<procedure>("meevax", "u8vector->string")),
                  define(make_symbol("string->u8vector"), make<procedure>("meevax", "string->u8vector")));
    }));

    libraries().emplace("(meevax write)", make<library>([](auto define)
    {
      return list(define(make_symbol("put-char"    ), make<procedure>("meevax", "put-char"    )),
                  define(make_symbol("put-string"  ), make<procedure>("meevax", "put-string"  )),
                  define(make_symbol("put-u8"      ), make<procedure>("meevax", "put-u8"      )),
                  define(make_symbol("put-u8vector"), make<procedure>("meevax", "put-u8vector")),
                  define(make_symbol("write"       ), make<procedure>("meevax", "write"       )),
                  define(make_symbol("write-simple"), make<procedure>("meevax", "write-simple")));
    }));
  }
} // namespace meevax::kernel
