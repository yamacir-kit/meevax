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

#ifndef INCLUDED_MEEVAX_KERNEL_EXACT_INTEGER_HPP
#define INCLUDED_MEEVAX_KERNEL_EXACT_INTEGER_HPP

#include <gmp.h>

#include <cstring> // std::strlen

#include <meevax/kernel/pair.hpp>

namespace meevax::inline kernel
{
  struct exact_integer
  {
    mpz_t value;

    exact_integer() noexcept;

    exact_integer(exact_integer const&) noexcept;

    exact_integer(exact_integer &&) noexcept;

    ~exact_integer();

    explicit exact_integer(mpz_t const) noexcept;

    template <typename T, typename = std::enable_if_t<std::is_arithmetic_v<T>>>
    explicit exact_integer(T x)
    {
      if constexpr (std::is_floating_point_v<T>)
      {
        mpz_init_set_d(value, x);
      }
      else if constexpr (std::is_signed_v<T>)
      {
        mpz_init_set_si(value, x);
      }
      else
      {
        mpz_init_set_ui(value, x);
      }
    }

    explicit exact_integer(std::string const&, int = 0);

    auto operator=(exact_integer const&) -> exact_integer &;

    auto operator=(exact_integer &&) noexcept -> exact_integer &;

    auto operator=(std::string const&) -> exact_integer &;

    explicit operator bool() const;

    template <typename T, typename = std::enable_if_t<std::is_arithmetic_v<T>>>
    explicit operator T() const
    {
      if constexpr (std::is_floating_point_v<T>)
      {
        return mpz_get_d(value);
      }
      else if constexpr (std::is_signed_v<T>)
      {
        return mpz_get_si(value);
      }
      else
      {
        return mpz_get_ui(value);
      }
    }

    auto sqrt() const -> std::tuple<exact_integer, exact_integer>;
  };

  #define DEFINE_COMPARISON_OPERATOR(SYMBOL)                                   \
  template <typename T, typename = std::enable_if_t<std::is_arithmetic_v<T>>>  \
  auto operator SYMBOL(exact_integer const& a, T b)                            \
  {                                                                            \
    if constexpr (std::is_floating_point_v<T>)                                 \
    {                                                                          \
      return mpz_cmp_d(a.value, b) SYMBOL 0;                                   \
    }                                                                          \
    else if constexpr (std::is_signed_v<T>)                                    \
    {                                                                          \
      return mpz_cmp_si(a.value, b) SYMBOL 0;                                  \
    }                                                                          \
    else                                                                       \
    {                                                                          \
      return mpz_cmp_ui(a.value, b) SYMBOL 0;                                  \
    }                                                                          \
  } static_assert(true)

  DEFINE_COMPARISON_OPERATOR(==);
  DEFINE_COMPARISON_OPERATOR(!=);
  DEFINE_COMPARISON_OPERATOR(< );
  DEFINE_COMPARISON_OPERATOR(<=);
  DEFINE_COMPARISON_OPERATOR(> );
  DEFINE_COMPARISON_OPERATOR(>=);

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
} // namespace meevax::kernel

#endif // INCLUDED_MEEVAX_KERNEL_EXACT_INTEGER_HPP
