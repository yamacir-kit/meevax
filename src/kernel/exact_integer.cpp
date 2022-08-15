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

#include <cstring> // std::strlen

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

  exact_integer::exact_integer(mpz_t const z) noexcept
  {
    mpz_init_set(value, z);
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

  exact_integer::exact_integer(int rhs)
    : exact_integer(static_cast<signed long>(rhs))
  {}

  exact_integer::exact_integer(signed long rhs)
  {
    mpz_init_set_si(value, rhs);
  }

  exact_integer::exact_integer(unsigned long rhs)
  {
    mpz_init_set_ui(value, rhs);
  }

  exact_integer::exact_integer(double rhs)
  {
    mpz_init_set_d(value, rhs);
  }

  exact_integer::exact_integer(external_representation const& s, int radix)
  {
    if (mpz_init_set_str(value, s.c_str(), radix))
    {
      mpz_clear(value);
      throw error();
    }
  }

  exact_integer::~exact_integer()
  {
    mpz_clear(value);
  }

  auto exact_integer::operator=(exact_integer const& rhs) -> exact_integer &
  {
    mpz_set(value, rhs.value);
    return *this;
  }

  auto exact_integer::operator=(exact_integer && rhs) noexcept -> exact_integer &
  {
    swap(rhs);
    return *this;
  }

  auto exact_integer::operator=(external_representation const& s) -> exact_integer &
  {
    if (mpz_set_str(value, s.c_str(), 0))
    {
      throw error(make<meevax::string>("invalid argument"), make<meevax::string>(s));
    }
    else
    {
      return *this;
    }
  }

  auto exact_integer::swap(exact_integer & rhs) noexcept -> void
  {
    std::swap(*value, *rhs.value);
  }

  exact_integer::operator bool() const
  {
    return (*value)._mp_size;
  }

  exact_integer::operator int() const
  {
    return static_cast<signed long>(*this);
  }

  exact_integer::operator signed long() const
  {
    return mpz_get_si(value);
  }

  exact_integer::operator unsigned long() const
  {
    return mpz_get_ui(value);
  }

  exact_integer::operator float() const
  {
    return static_cast<double>(*this);
  }

  exact_integer::operator double() const
  {
    return mpz_get_d(value);
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
    auto free = [](char * data)
    {
      void (*free)(void *, std::size_t);
      mp_get_memory_functions(nullptr, nullptr, &free);
      std::invoke(free, static_cast<void *>(data), std::strlen(data) + 1);
    };

    return os << cyan(std::unique_ptr<char, decltype(free)>(mpz_get_str(nullptr, 10, datum.value), free).get());
  }

  auto exact_integer_sqrt(exact_integer const& x) -> std::tuple<exact_integer, exact_integer>
  {
    exact_integer s, r;
    mpz_rootrem(s.value, r.value, x.value, 2);
    return std::make_tuple(s, r);
  }
} // namespace kernel
} // namespace meevax
