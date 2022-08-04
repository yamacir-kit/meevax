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

#ifndef INCLUDED_MEEVAX_KERNEL_EXACT_INTEGER_HPP
#define INCLUDED_MEEVAX_KERNEL_EXACT_INTEGER_HPP

#include <gmp.h>

#include <meevax/kernel/pair.hpp>

namespace meevax
{
inline namespace kernel
{
  struct exact_integer
  {
    mpz_t value;

    explicit exact_integer() noexcept;

    explicit exact_integer(mpz_t) noexcept;

    exact_integer(exact_integer const&) noexcept;

    explicit exact_integer(exact_integer &&) noexcept;

    explicit exact_integer(int);

    explicit exact_integer(signed long);

    explicit exact_integer(unsigned long);

    explicit exact_integer(double);

    explicit exact_integer(external_representation const&, int = 0);

    ~exact_integer();

    auto operator=(exact_integer const&) -> exact_integer &;

    auto operator=(exact_integer &&) noexcept -> exact_integer &;

    auto operator=(external_representation const&) -> exact_integer &;

    auto string(int = 10) const -> external_representation;

    auto swap(exact_integer &) noexcept -> void;

    explicit operator bool() const;

    operator int() const;

    operator signed long() const;

    operator unsigned long() const;

    explicit operator float() const;

    explicit operator double() const;

    explicit operator external_representation() const;
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

  auto operator <<(std::ostream &, exact_integer const&) -> std::ostream &;

  auto exact_integer_sqrt(exact_integer const&) -> std::tuple<exact_integer, exact_integer>;

  let extern const e0, e1; // Frequently used exact-integer values.
} // namespace kernel
} // namespace meevax

#endif // INCLUDED_MEEVAX_KERNEL_EXACT_INTEGER_HPP
