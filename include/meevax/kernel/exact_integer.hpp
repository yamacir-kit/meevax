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

#ifndef INCLUDED_MEEVAX_KERNEL_EXACT_INTEGER_HPP
#define INCLUDED_MEEVAX_KERNEL_EXACT_INTEGER_HPP

#include <gmp.h>

#include <meevax/functional/greatest_common_divisor.hpp>
#include <meevax/kernel/numeric_tower.hpp>
#include <meevax/kernel/pair.hpp>

namespace meevax
{
inline namespace kernel
{
  struct exact_integer
  {
    using value_type = mpz_t;

    value_type value;

    static constexpr std::true_type is_integer {};

    explicit exact_integer() noexcept;

    exact_integer(exact_integer const&) noexcept;

    explicit exact_integer(exact_integer &&) noexcept;

    explicit exact_integer(int);

    explicit exact_integer(signed long);

    explicit exact_integer(unsigned long);

    explicit exact_integer(double);

    explicit exact_integer(std::string const&, int = 0);

    explicit exact_integer(addition, exact_integer const&, exact_integer const&);

    explicit exact_integer(subtraction, exact_integer const&, exact_integer const&);

    explicit exact_integer(multiplication, exact_integer const&, exact_integer const&);

    explicit exact_integer(division, exact_integer const&, exact_integer const&);

    explicit exact_integer(modulo, exact_integer const&, exact_integer const&);

    explicit exact_integer(greatest_common_divisor, exact_integer const&, exact_integer const&);

    ~exact_integer();

    auto operator=(exact_integer const&) -> exact_integer &;

    auto operator=(exact_integer &&) noexcept -> exact_integer &;

    auto operator=(std::string const&) -> exact_integer &;

    auto as_exact() const noexcept -> exact_integer const&;

    template <typename T, REQUIRES(std::is_floating_point<T>)>
    auto as_inexact() const
    {
      return floating_point(static_cast<T>(*this));
    }

    auto floor_remainder(exact_integer const&) const -> exact_integer;

    auto floor_quotient(exact_integer const&) const -> exact_integer;

    auto string(int = 10) const -> std::string; // TODO RENAME TO 'string'

    auto swap(exact_integer &) noexcept -> void;

    auto truncate_remainder(exact_integer const&) const -> exact_integer;

    auto truncate_quotient(exact_integer const&) const -> exact_integer;

    explicit operator bool() const;

    explicit operator int() const;

    explicit operator signed long() const;

    explicit operator unsigned long() const;

    explicit operator float() const;

    explicit operator double() const;

    explicit operator std::string() const;
  };

  auto operator ==(exact_integer const&, int const) -> bool;
  auto operator !=(exact_integer const&, int const) -> bool;
  auto operator < (exact_integer const&, int const) -> bool;
  auto operator <=(exact_integer const&, int const) -> bool;
  auto operator > (exact_integer const&, int const) -> bool;
  auto operator >=(exact_integer const&, int const) -> bool;

  auto operator ==(exact_integer const&, signed long const) -> bool;
  auto operator !=(exact_integer const&, signed long const) -> bool;
  auto operator < (exact_integer const&, signed long const) -> bool;
  auto operator <=(exact_integer const&, signed long const) -> bool;
  auto operator > (exact_integer const&, signed long const) -> bool;
  auto operator >=(exact_integer const&, signed long const) -> bool;

  auto operator ==(exact_integer const&, unsigned long const) -> bool;
  auto operator !=(exact_integer const&, unsigned long const) -> bool;
  auto operator < (exact_integer const&, unsigned long const) -> bool;
  auto operator <=(exact_integer const&, unsigned long const) -> bool;
  auto operator > (exact_integer const&, unsigned long const) -> bool;
  auto operator >=(exact_integer const&, unsigned long const) -> bool;

  auto operator ==(exact_integer const&, double const) -> bool;
  auto operator !=(exact_integer const&, double const) -> bool;
  auto operator < (exact_integer const&, double const) -> bool;
  auto operator <=(exact_integer const&, double const) -> bool;
  auto operator > (exact_integer const&, double const) -> bool;
  auto operator >=(exact_integer const&, double const) -> bool;

  auto operator <<(std::ostream &, exact_integer const&) -> std::ostream &;

  let extern const e0, e1; // Frequently used exact-integer values.
} // namespace kernel
} // namespace meevax

#endif // INCLUDED_MEEVAX_KERNEL_EXACT_INTEGER_HPP
