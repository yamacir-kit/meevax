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

#include <meevax/kernel/number/bitwise.hpp>

namespace meevax::inline kernel::number
{
  auto bitwise_not(object const& x) -> object
  {
    auto f = []<typename T>(T const& x)
    {
      if constexpr (std::is_same_v<T, large_integer>)
      {
        auto i = large_integer();
        mpz_com(i.value, x.value);
        return i;
      }
      else
      {
        return ~x;
      }
    };

    return apply_to<exact_integer>(f, x);
  }

  #define DEFINE_BITWISE2(NAME, MPZ, OPERATOR)                                 \
  auto bitwise_##NAME(object const& x, object const& y) -> object              \
  {                                                                            \
    auto f = []<typename T, typename U>(T const& x, U const& y)                \
    {                                                                          \
      if constexpr (std::is_same_v<T, large_integer>)                          \
      {                                                                        \
        if constexpr (std::is_same_v<U, large_integer>)                        \
        {                                                                      \
          auto i = large_integer();                                            \
          mpz_##MPZ(i.value, x.value, y.value);                                \
          return i;                                                            \
        }                                                                      \
        else                                                                   \
        {                                                                      \
          auto i = large_integer();                                            \
          mpz_##MPZ(i.value, x.value, large_integer(y).value);                 \
          return i;                                                            \
        }                                                                      \
      }                                                                        \
      else                                                                     \
      {                                                                        \
        if constexpr (std::is_same_v<U, large_integer>)                        \
        {                                                                      \
          auto i = large_integer();                                            \
          mpz_##MPZ(i.value, large_integer(x).value, y.value);                 \
          return i;                                                            \
        }                                                                      \
        else                                                                   \
        {                                                                      \
          return x OPERATOR y;                                                 \
        }                                                                      \
      }                                                                        \
    };                                                                         \
                                                                               \
    return apply_to<exact_integers>(f, x, y);                                  \
  }

  DEFINE_BITWISE2(and, and, &)
  DEFINE_BITWISE2(ior, ior, |)
  DEFINE_BITWISE2(xor, xor, ^)

  auto bit_shift(object const& x, small_integer c) -> object
  {
    auto f = [c]<typename T>(T const& x)
    {
      if constexpr (std::is_same_v<T, large_integer>)
      {
        if (c < 0)
        {
          auto i = large_integer();
          mpz_fdiv_q_2exp(i.value, x.value, -c);
          return i;
        }
        else if (c == 0)
        {
          return x;
        }
        else // 0 < c
        {
          auto i = large_integer();
          mpz_mul_2exp(i.value, x.value, c);
          return i;
        }
      }
      else
      {
        if (c < 0)
        {
          return canonicalize(x >> -c);
        }
        else if (c == 0)
        {
          return canonicalize(x);
        }
        else // 0 < c
        {
          if (64 <= c or x < (std::numeric_limits<std::int64_t>::min() >> c) or (std::numeric_limits<std::int64_t>::max() >> c) < x)
          {
            auto i = large_integer();
            mpz_mul_2exp(i.value, large_integer(x).value, c);
            return make(i);
          }
          else
          {
            return canonicalize(x << c);
          }
        }
      }
    };

    return apply_to<exact_integer>(f, x);
  }

  auto bit_count(object const& x) -> object
  {
    auto f = []<typename T>(T const& x)
    {
      if constexpr (std::is_same_v<T, large_integer>)
      {
        if (x < 0_i64)
        {
          auto i = large_integer();
          mpz_com(i.value, x.value);
          return static_cast<small_integer>(mpz_popcount(i.value));
        }
        else
        {
          return static_cast<small_integer>(mpz_popcount(x.value));
        }
      }
      else
      {
        return static_cast<small_integer>(std::popcount(static_cast<std::uint64_t>(x < 0 ? ~x : x)));
      }
    };

    return apply_to<exact_integer>(f, x);
  }

  auto bit_width(object const& x) -> object
  {
    auto f = []<typename T>(T const& x)
    {
      if constexpr (std::is_same_v<T, large_integer>)
      {
        if (auto sign = mpz_sgn(x.value); sign < 0)
        {
          auto i = large_integer();
          mpz_com(i.value, x.value);
          return static_cast<std::int64_t>(mpz_sizeinbase(i.value, 2));
        }
        else if (sign == 0)
        {
          return 0_i64;
        }
        else // 0 < sign
        {
          return static_cast<std::int64_t>(mpz_sizeinbase(x.value, 2));
        }
      }
      else
      {
        if (x < 0)
        {
          return 64_i64 - std::countl_zero(static_cast<std::uint64_t>(~x));
        }
        else if (x == 0)
        {
          return x;
        }
        else // 0 < x
        {
          return 64_i64 - std::countl_zero(static_cast<std::uint64_t>(x));
        }
      }
    };

    return apply_to<exact_integer>(f, x);
  }
} // namespace meevax::kernel::number
