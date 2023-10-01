/*
   Copyright 2018-2023 Tatsuya Yamasaki.

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

namespace meevax
{
inline namespace kernel
{
  let const e0 = make<exact_integer>(0);
  let const e1 = make<exact_integer>(1);

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

  exact_integer::exact_integer(std::int8_t si)
  {
    mpz_init_set_si(value, si);
  }

  exact_integer::exact_integer(std::int16_t si)
  {
    mpz_init_set_si(value, si);
  }

  exact_integer::exact_integer(std::int32_t si)
  {
    mpz_init_set_si(value, si);
  }

  exact_integer::exact_integer(std::int64_t si)
  {
    mpz_init_set_si(value, si);
  }

  exact_integer::exact_integer(std::uint8_t ui)
  {
    mpz_init_set_ui(value, ui);
  }

  exact_integer::exact_integer(std::uint16_t ui)
  {
    mpz_init_set_ui(value, ui);
  }

  exact_integer::exact_integer(std::uint32_t ui)
  {
    mpz_init_set_ui(value, ui);
  }

  exact_integer::exact_integer(std::uint64_t ui)
  {
    mpz_init_set_ui(value, ui);
  }

  exact_integer::exact_integer(double d)
  {
    mpz_init_set_d(value, d);
  }

  exact_integer::exact_integer(std::string const& s, int radix)
  {
    if (mpz_init_set_str(value, (s.at(0) == '+' ? s.substr(1) : s).c_str(), radix))
    {
      mpz_clear(value);
      throw std::invalid_argument("not a integer");
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

  exact_integer::operator std::int8_t() const
  {
    return mpz_get_si(value);
  }

  exact_integer::operator std::int16_t() const
  {
    return mpz_get_si(value);
  }

  exact_integer::operator std::int32_t() const
  {
    return mpz_get_si(value);
  }

  exact_integer::operator std::int64_t() const
  {
    return mpz_get_si(value);
  }

  exact_integer::operator std::uint8_t() const
  {
    return mpz_get_ui(value);
  }

  exact_integer::operator std::uint16_t() const
  {
    return mpz_get_ui(value);
  }

  exact_integer::operator std::uint32_t() const
  {
    return mpz_get_ui(value);
  }

  exact_integer::operator std::uint64_t() const
  {
    return mpz_get_ui(value);
  }

  exact_integer::operator float() const
  {
    return mpz_get_d(value);
  }

  exact_integer::operator double() const
  {
    return mpz_get_d(value);
  }

  auto exact_integer::square_root() const -> std::tuple<exact_integer, exact_integer>
  {
    exact_integer s, r;
    mpz_rootrem(s.value, r.value, value, 2);
    return std::make_tuple(s, r);
  }

  auto operator ==(exact_integer const& a, int const b) -> bool { return a == static_cast<signed long>(b); }
  auto operator !=(exact_integer const& a, int const b) -> bool { return a != static_cast<signed long>(b); }
  auto operator < (exact_integer const& a, int const b) -> bool { return a <  static_cast<signed long>(b); }
  auto operator <=(exact_integer const& a, int const b) -> bool { return a <= static_cast<signed long>(b); }
  auto operator > (exact_integer const& a, int const b) -> bool { return a >  static_cast<signed long>(b); }
  auto operator >=(exact_integer const& a, int const b) -> bool { return a >= static_cast<signed long>(b); }

  auto operator ==(exact_integer const& a, signed long const b) -> bool { return mpz_cmp_si(a.value, b) == 0; }
  auto operator !=(exact_integer const& a, signed long const b) -> bool { return mpz_cmp_si(a.value, b) != 0; }
  auto operator < (exact_integer const& a, signed long const b) -> bool { return mpz_cmp_si(a.value, b) <  0; }
  auto operator <=(exact_integer const& a, signed long const b) -> bool { return mpz_cmp_si(a.value, b) <= 0; }
  auto operator > (exact_integer const& a, signed long const b) -> bool { return mpz_cmp_si(a.value, b) >  0; }
  auto operator >=(exact_integer const& a, signed long const b) -> bool { return mpz_cmp_si(a.value, b) >= 0; }

  auto operator ==(exact_integer const& a, unsigned long const b) -> bool { return mpz_cmp_ui(a.value, b) == 0; }
  auto operator !=(exact_integer const& a, unsigned long const b) -> bool { return mpz_cmp_ui(a.value, b) != 0; }
  auto operator < (exact_integer const& a, unsigned long const b) -> bool { return mpz_cmp_ui(a.value, b) <  0; }
  auto operator <=(exact_integer const& a, unsigned long const b) -> bool { return mpz_cmp_ui(a.value, b) <= 0; }
  auto operator > (exact_integer const& a, unsigned long const b) -> bool { return mpz_cmp_ui(a.value, b) >  0; }
  auto operator >=(exact_integer const& a, unsigned long const b) -> bool { return mpz_cmp_ui(a.value, b) >= 0; }

  auto operator <<(std::ostream & os, exact_integer const& datum) -> std::ostream &
  {
    return os << cyan(std::unique_ptr<char, gmp_free>(mpz_get_str(nullptr, 10, datum.value)).get());
  }
} // namespace kernel
} // namespace meevax
