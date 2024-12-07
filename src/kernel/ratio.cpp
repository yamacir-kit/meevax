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

#include <regex>

#include <meevax/kernel/number.hpp>

namespace meevax::inline kernel
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

  ratio::ratio(exact_integer const& z)
  {
    mpq_init(value);
    mpq_set_z(value, z.value);
  }

  ratio::ratio(exact_integer const& n, exact_integer const& d)
  {
    mpq_init(value);
    mpq_set_num(value, n.value);
    mpq_set_den(value, d.value);
    mpq_canonicalize(value);
  }

  ratio::ratio(double x)
  {
    mpq_init(value);
    mpq_set_d(value, x);
  }

  ratio::ratio(std::string const& token, int radix)
  {
    if (mpq_init(value); mpq_set_str(value, token.c_str(), radix))
    {
      mpq_clear(value);
      throw std::invalid_argument("not a ratio");
    }
    else
    {
      mpq_canonicalize(value);
    }
  }

  ratio::~ratio()
  {
    mpq_clear(value);
  }

  auto ratio::denominator() const -> exact_integer
  {
    return exact_integer(mpq_denref(value));
  }

  auto ratio::numerator() const -> exact_integer
  {
    return exact_integer(mpq_numref(value));
  }

  ratio::operator int() const
  {
    return cmath_cast<int>(mpq_get_d(value));
  }

  ratio::operator double() const
  {
    return mpq_get_d(value);
  }

  auto operator <<(std::ostream & os, ratio const& datum) -> std::ostream &
  {
    auto free = [](char * data)
    {
      void (*free)(void *, std::size_t);
      mp_get_memory_functions(nullptr, nullptr, &free);
      std::invoke(free, static_cast<void *>(data), std::strlen(data) + 1);
    };

    return os << cyan(std::unique_ptr<char, decltype(free)>(mpq_get_str(nullptr, 10, datum.value), free).get());
  }
} // namespace meevax::kernel
