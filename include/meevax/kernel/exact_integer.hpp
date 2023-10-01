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

    exact_integer() noexcept;

    exact_integer(exact_integer const&) noexcept;

    exact_integer(exact_integer &&) noexcept;

    ~exact_integer();

    explicit exact_integer(mpz_t const) noexcept;

    explicit exact_integer(std::int8_t);

    explicit exact_integer(std::int16_t);

    explicit exact_integer(std::int32_t);

    explicit exact_integer(std::int64_t);

    explicit exact_integer(std::uint8_t);

    explicit exact_integer(std::uint16_t);

    explicit exact_integer(std::uint32_t);

    explicit exact_integer(std::uint64_t);

    explicit exact_integer(double);

    explicit exact_integer(std::string const&, int = 0);

    auto operator=(exact_integer const&) -> exact_integer &;

    auto operator=(exact_integer &&) noexcept -> exact_integer &;

    auto operator=(std::string const&) -> exact_integer &;

    explicit operator bool() const;

    operator std::int8_t() const;

    operator std::int16_t() const;

    operator std::int32_t() const;

    operator std::int64_t() const;

    operator std::uint8_t() const;

    operator std::uint16_t() const;

    operator std::uint32_t() const;

    operator std::uint64_t() const;

    explicit operator float() const;

    explicit operator double() const;

    auto square_root() const -> std::tuple<exact_integer, exact_integer>;
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

  struct gmp_free
  {
    void (*free)(void *, std::size_t);

    explicit gmp_free()
    {
      mp_get_memory_functions(nullptr, nullptr, &free);
    }

    auto operator ()(char * data) const -> void
    {
      free(static_cast<void *>(data), std::strlen(data) + 1);
    }
  };

  let extern const e0, e1; // Frequently used exact-integer values.
} // namespace kernel
} // namespace meevax

#endif // INCLUDED_MEEVAX_KERNEL_EXACT_INTEGER_HPP
