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

namespace meevax
{
inline namespace kernel
{
  ratio::ratio(double x)
  {
    mpq_t value;
    mpq_init(value);
    mpq_set_d(value, x);

    numerator() = make<exact_integer>(mpq_numref(value));
    denominator() = make<exact_integer>(mpq_denref(value));

    mpq_clear(value);
  }

  ratio::ratio(std::string const& token, int radix)
  {
    std::regex static const pattern { "([+-]?[0-9a-f]+)/([0-9a-f]+)" };

    if (std::smatch result; std::regex_match(token, result, pattern))
    {
      auto n = exact_integer(result.str(1), radix);

      numerator() = make(n);

      auto d = exact_integer(result.str(2), radix);

      denominator() = make(d);
    }
    else
    {
      throw read_error(make<string>("not a ratio"), make<string>(token));
    }
  }

  auto ratio::exact() const -> object
  {
    return simple();
  }

  auto ratio::denominator() const -> const_reference
  {
    return second;
  }

  auto ratio::denominator() -> reference
  {
    return second;
  }

  auto ratio::inexact() const -> object
  {
    return make<double_float>(numerator().as<exact_integer>().inexact().as<double_float>() / denominator().as<exact_integer>().inexact().as<double_float>());
  }

  auto ratio::invert() const -> ratio
  {
    return ratio(denominator(), numerator());
  }

  auto ratio::is_integer() const -> bool
  {
    return denominator().as<exact_integer>() == 1;
  }

  auto ratio::numerator() const -> const_reference
  {
    return first;
  }

  auto ratio::numerator() -> reference
  {
    return first;
  }

  auto ratio::reduce() const -> ratio
  {
    if (auto const d = exact_integer(gcd, numerator().as<exact_integer>(), denominator().as<exact_integer>()); d != 1)
    {
      return ratio(make<exact_integer>(div, numerator().as<exact_integer>(), d), make<exact_integer>(div, denominator().as<exact_integer>(), d));
    }
    else
    {
      return *this;
    }
  }

  auto ratio::simple() const -> object
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
  auto ratio::NAME() const -> object                                           \
  {                                                                            \
    if (const double_float x {                                                 \
          std::NAME(numerator().as<exact_integer>().inexact().as<double_float>() / denominator().as<exact_integer>().inexact().as<double_float>()) \
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
  auto ratio::NAME(const_reference x) const -> object                          \
  {                                                                            \
    if (const double_float n {                                                 \
          std::NAME(numerator().as<exact_integer>().inexact().as<double_float>() / denominator().as<exact_integer>().inexact().as<double_float>(), \
                    x.as<number>().inexact().as<double_float>())               \
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
    return os << datum.numerator() << cyan("/") << datum.denominator();
  }
} // namespace kernel
} // namespace meevax
