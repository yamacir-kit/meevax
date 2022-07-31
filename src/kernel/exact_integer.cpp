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
#include <meevax/kernel/floating_point.hpp>

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

  exact_integer::exact_integer(mpz_t given) noexcept
  {
    mpz_init_set(value, given);
  }

  exact_integer::exact_integer(exact_integer const& given) noexcept
  {
    mpz_init_set(value, given.value);
  }

  exact_integer::exact_integer(exact_integer && rhs) noexcept
  {
    *value = *rhs.value;
    mpz_init(rhs.value);
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
      throw read_error(make<meevax::string>("not an exact-integer"), make<meevax::string>(s));
    }
  }

  exact_integer::exact_integer(addition, exact_integer const& a, exact_integer const& b)
    : exact_integer {}
  {
    mpz_add(value, a.value, b.value);
  }

  exact_integer::exact_integer(subtraction, exact_integer const& a, exact_integer const& b)
    : exact_integer {}
  {
    mpz_sub(value, a.value, b.value);
  }

  exact_integer::exact_integer(multiplication, exact_integer const& a, exact_integer const& b)
    : exact_integer {}
  {
    mpz_mul(value, a.value, b.value);
  }

  exact_integer::exact_integer(division, exact_integer const& a, exact_integer const& b)
    : exact_integer {}
  {
    mpz_div(value, a.value, b.value);
  }

  exact_integer::exact_integer(modulo, exact_integer const& a, exact_integer const& b)
    : exact_integer {}
  {
    mpz_tdiv_r(value, a.value, b.value);
  }

  exact_integer::exact_integer(greatest_common_divisor, exact_integer const& a, exact_integer const& b)
    : exact_integer {}
  {
    mpz_gcd(value, a.value, b.value);
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

  auto exact_integer::exact() const -> value_type
  {
    return make(*this);
  }

  auto exact_integer::floor_remainder(exact_integer const& divisor) const -> exact_integer
  {
    exact_integer result {};
    mpz_fdiv_r(result.value, value, divisor.value);
    return result;
  }

  auto exact_integer::floor_quotient(exact_integer const& divisor) const -> exact_integer
  {
    exact_integer result {};
    mpz_fdiv_q(result.value, value, divisor.value);
    return result;
  }

  auto exact_integer::inexact() const -> value_type
  {
    return make<double_float>(static_cast<double>(*this));
  }

  auto exact_integer::is_integer() const -> bool
  {
    return true;
  }

  auto exact_integer::string(int radix) const -> external_representation
  {
    auto deallocate = [](char * data)
    {
      using gmp_free_function = void (*)(void *, std::size_t);
      gmp_free_function current_free_function;
      mp_get_memory_functions(nullptr, nullptr, &current_free_function);
      std::invoke(current_free_function, static_cast<void *>(data), std::strlen(data) + 1);
    };

    std::unique_ptr<char, decltype(deallocate)> result { mpz_get_str(nullptr, radix, value), deallocate };

    return result.get();
  }

  auto exact_integer::swap(exact_integer & rhs) noexcept -> void
  {
    std::swap(*value, *rhs.value);
  }

  auto exact_integer::truncate_remainder(exact_integer const& divisor) const -> exact_integer
  {
    exact_integer result {};
    mpz_tdiv_r(result.value, value, divisor.value);
    return result;
  }

  auto exact_integer::truncate_quotient(exact_integer const& divisor) const -> exact_integer
  {
    exact_integer result {};
    mpz_tdiv_q(result.value, value, divisor.value);
    return result;
  }

  #define DEFINE(NAME)                                                         \
  auto exact_integer::NAME() const -> value_type                               \
  {                                                                            \
    if (const double_float n { std::NAME(static_cast<double>(*this)) }; n.is_integer()) \
    {                                                                          \
      return make<exact_integer>(n.value);                                     \
    }                                                                          \
    else                                                                       \
    {                                                                          \
      return make(n);                                                          \
    }                                                                          \
  } static_assert(true)

                                                          DEFINE(exp);
                                                          DEFINE(log);
               DEFINE(atan);                              DEFINE(sqrt);

  DEFINE(floor);
  DEFINE(ceil);
  DEFINE(trunc);
  DEFINE(round);

  #undef DEFINE

  #define DEFINE(NAME)                                                         \
  auto exact_integer::NAME(const_reference x) const -> value_type              \
  {                                                                            \
    if (const double_float n {                                                 \
          std::NAME(static_cast<double>(*this),                                \
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

  exact_integer::operator external_representation() const
  {
    return string();
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

  auto operator ==(exact_integer const& a, double const b) -> bool { return mpz_cmp_d(a.value, b) == 0; }
  auto operator !=(exact_integer const& a, double const b) -> bool { return mpz_cmp_d(a.value, b) != 0; }
  auto operator < (exact_integer const& a, double const b) -> bool { return mpz_cmp_d(a.value, b) <  0; }
  auto operator <=(exact_integer const& a, double const b) -> bool { return mpz_cmp_d(a.value, b) <= 0; }
  auto operator > (exact_integer const& a, double const b) -> bool { return mpz_cmp_d(a.value, b) >  0; }
  auto operator >=(exact_integer const& a, double const b) -> bool { return mpz_cmp_d(a.value, b) >= 0; }

  auto operator <<(std::ostream & os, exact_integer const& datum) -> std::ostream &
  {
    return os << cyan(static_cast<external_representation>(datum));
  }
} // namespace kernel
} // namespace meevax
