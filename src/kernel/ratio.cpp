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
  ratio::ratio()
  {
    mpq_init(value);
  }

  ratio::ratio(ratio const& other)
  {
    mpq_init(value);
    mpq_set(value, other.value);
    mpq_canonicalize(value);
  }

  ratio::ratio(ratio && other)
  {
    mpq_init(value);
    mpq_swap(value, other.value);
    mpq_canonicalize(value);
  }

  ratio::ratio(const_reference x, const_reference y)
  {
    mpq_init(value);
    mpq_set_num(value, x.as<exact_integer>().value);
    mpq_set_den(value, y.as<exact_integer>().value);
    mpq_canonicalize(value);
  }

  ratio::ratio(double x)
  {
    mpq_init(value);
    mpq_set_d(value, x);
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
      mpq_canonicalize(value);
    }
  }

  ratio::~ratio()
  {
    mpq_clear(value);
  }

  auto ratio::denominator() const -> value_type
  {
    return make<exact_integer>(mpq_denref(value));
  }

  auto ratio::invert() const -> ratio
  {
    return ratio(denominator(), numerator());
  }

  auto ratio::numerator() const -> value_type
  {
    return make<exact_integer>(mpq_numref(value));
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
