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

#include <regex>

#include <meevax/kernel/number.hpp>

namespace meevax
{
inline namespace kernel
{
  ratio::ratio(double x)
  {
    mpq_init(value);
    mpq_set_d(value, x);

    numerator() = make<exact_integer>(mpq_numref(value));
    denominator() = make<exact_integer>(mpq_denref(value));

    mpq_clear(value);
  }

  ratio::ratio(external_representation const& token, int radix)
  {
    std::regex static const pattern { "([+-]?[0-9a-f]+)/([0-9a-f]+)" };

    if (mpq_init(value); not std::regex_match(token, pattern) or mpq_set_str(value, token.c_str(), radix))
    {
      mpq_clear(value);
      throw error();
    }
    else // TEMPORARY!!!
    {
      numerator() = make<exact_integer>(mpq_numref(value));
      denominator() = make<exact_integer>(mpq_denref(value));
      mpq_clear(value);
    }
  }

  auto ratio::denominator() const -> const_reference
  {
    return second;
  }

  auto ratio::denominator() -> reference
  {
    return second;
  }

  auto ratio::invert() const -> ratio
  {
    return ratio(denominator(), numerator());
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
    auto gcd = [](exact_integer const& a, exact_integer const& b)
    {
      exact_integer n;
      mpz_gcd(n.value, a.value, b.value);
      return n;
    };

    auto div = [](exact_integer const& a, exact_integer const& b)
    {
      exact_integer n;
      mpz_div(n.value, a.value, b.value);
      return n;
    };

    if (auto x = gcd(numerator().as<exact_integer>(), denominator().as<exact_integer>()); x != 1)
    {
      return ratio(make<exact_integer>(div(numerator().as<exact_integer>(), x)),
                   make<exact_integer>(div(denominator().as<exact_integer>(), x)));
    }
    else
    {
      return *this;
    }
  }

  auto ratio::simple() const -> value_type
  {
    if (auto x = reduce(); is_integer()(x))
    {
      return x.numerator();
    }
    else
    {
      return make(x);
    }
  }

  ratio::operator double() const
  {
    return static_cast<double>(numerator().as<exact_integer>()) / static_cast<double>(denominator().as<exact_integer>());
  }

  auto operator <<(std::ostream & os, ratio const& datum) -> std::ostream &
  {
    return os << datum.numerator() << cyan("/") << datum.denominator();
  }
} // namespace kernel
} // namespace meevax
