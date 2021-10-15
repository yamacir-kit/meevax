/*
   Copyright 2018-2021 Tatsuya Yamasaki.

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

#include <regex>

#include <meevax/kernel/number.hpp>
#include <meevax/posix/vt10x.hpp>

namespace meevax
{
inline namespace kernel
{
  ratio::ratio(std::string const& token, int radix)
  {
    std::regex static const pattern { "([+-]?[0-9a-f]+)/([0-9a-f]+)" };

    if (std::smatch result; std::regex_match(token, result, pattern))
    {
      auto numerator = exact_integer(result.str(1), radix);

      car(*this) = make(numerator);

      auto denominator = exact_integer(result.str(2), radix);

      cdr(*this) = make(denominator);
    }
    else
    {
      throw read_error(make<string>("not a ratio"), make<string>(token));
    }
  }

  auto ratio::exact() const -> value_type
  {
    return simple();
  }

  auto ratio::denominator() const -> exact_integer const&
  {
    return cdr(*this).as<exact_integer>();
  }

  auto ratio::inexact() const -> pair::value_type
  {
    return make<double_float>(numerator().inexact().as<double_float>() / denominator().inexact().as<double_float>());
  }

  auto ratio::invert() const -> ratio
  {
    return ratio(cdr(*this), car(*this));
  }

  auto ratio::is_integer() const -> bool
  {
    return denominator() == 1;
  }

  auto ratio::numerator() const -> exact_integer const&
  {
    return car(*this).as<exact_integer>();
  }

  auto ratio::reduce() const -> ratio
  {
    if (auto const d = exact_integer(gcd, numerator(), denominator()); d != 1)
    {
      return ratio(make<exact_integer>(div, numerator(), d), make<exact_integer>(div, denominator(), d));
    }
    else
    {
      return *this;
    }
  }

  auto ratio::simple() const -> value_type
  {
    if (auto x = reduce(); x.is_integer())
    {
      return car(x);
    }
    else
    {
      return make(x);
    }
  }

  #define DEFINE(NAME)                                                         \
  auto ratio::NAME() const -> value_type                                       \
  {                                                                            \
    if (const double_float x {                                                 \
          std::NAME(numerator().inexact().as<double_float>() / denominator().inexact().as<double_float>()) \
        }; x.is_integer())                                                     \
    {                                                                          \
      return make<exact_integer>(x.value);                                     \
    }                                                                          \
    else                                                                       \
    {                                                                          \
      return make(x);                                                          \
    }                                                                          \
  }                                                                            \
  static_assert(true)

  DEFINE(sin); DEFINE(asin); DEFINE(sinh); DEFINE(asinh); DEFINE(exp);
  DEFINE(cos); DEFINE(acos); DEFINE(cosh); DEFINE(acosh); DEFINE(log);
  DEFINE(tan); DEFINE(atan); DEFINE(tanh); DEFINE(atanh); DEFINE(sqrt);

  DEFINE(floor);
  DEFINE(ceil);
  DEFINE(trunc);
  DEFINE(round);

  #undef DEFINE

  #define DEFINE(NAME)                                                         \
  auto ratio::NAME(pair::const_reference x) const -> value_type                \
  {                                                                            \
    if (const double_float n {                                                 \
          std::NAME(numerator().inexact().as<double_float>() / denominator().inexact().as<double_float>(), \
                    x.inexact().as<double_float>())                            \
        }; n.is_integer())                                                     \
    {                                                                          \
      return make<exact_integer>(n.value);                                     \
    }                                                                          \
    else                                                                       \
    {                                                                          \
      return make(n);                                                          \
    }                                                                          \
  }                                                                            \
  static_assert(true)

  DEFINE(atan2);
  DEFINE(pow);

  #undef DEFINE

  auto operator <<(std::ostream & os, ratio const& datum) -> std::ostream &
  {
    return os << cyan << car(datum) << cyan << "/" << cyan << cdr(datum) << reset;
  }
} // namespace kernel
} // namespace meevax
