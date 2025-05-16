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

#include <cstring> // std::strlen
#include <memory> // std::unique_ptr

#include <meevax/kernel/error.hpp>
#include <meevax/kernel/exact_integer.hpp>

namespace meevax::inline kernel
{
  let const e0 = make<std::int32_t>(0);
  let const e1 = make<std::int32_t>(1);

  exact_integer::exact_integer() noexcept
  {
    mpz_init(value);
  }

  exact_integer::exact_integer(exact_integer const& other) noexcept
  {
    mpz_init_set(value, other.value);
  }

  exact_integer::exact_integer(exact_integer && other) noexcept
  {
    mpz_init(value);
    mpz_swap(value, other.value);
  }

  exact_integer::~exact_integer()
  {
    mpz_clear(value);
  }

  exact_integer::exact_integer(mpz_t const z) noexcept
  {
    mpz_init_set(value, z);
  }

  exact_integer::exact_integer(std::string const& s, int radix)
  {
    if (mpz_init_set_str(value, (s.at(0) == '+' ? s.substr(1) : s).c_str(), radix))
    {
      mpz_clear(value);
      throw std::invalid_argument("not an exact-integer");
    }
  }

  auto exact_integer::operator=(exact_integer const& rhs) -> exact_integer &
  {
    mpz_set(value, rhs.value);
    return *this;
  }

  auto exact_integer::operator=(exact_integer && other) noexcept -> exact_integer &
  {
    mpz_swap(value, other.value);
    return *this;
  }

  auto exact_integer::operator=(std::string const& s) -> exact_integer &
  {
    if (mpz_set_str(value, s.c_str(), 0))
    {
      throw std::invalid_argument(s);
    }
    else
    {
      return *this;
    }
  }

  exact_integer::operator bool() const
  {
    return (*value)._mp_size;
  }

  auto exact_integer::sqrt() const -> std::tuple<exact_integer, exact_integer>
  {
    exact_integer s, r;
    mpz_rootrem(s.value, r.value, value, 2);
    return std::make_tuple(s, r);
  }

  auto operator <<(std::ostream & os, exact_integer const& datum) -> std::ostream &
  {
    return os << yellow(std::unique_ptr<char, gmp_free>(mpz_get_str(nullptr, 10, datum.value)).get());
  }
} // namespace meevax::kernel
