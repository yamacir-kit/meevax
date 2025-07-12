/*
   Copyright 2018-2025 Tatsuya Yamasaki.

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

#include <meevax/kernel/number/power.hpp>

namespace meevax::inline kernel::number
{
  auto pow(object const& x, object const& y) -> object
  {
    auto f = []<typename T, typename U>(T const& x, U const& y)
    {
      if constexpr (std::is_same_v<T, complex> or
                    std::is_same_v<U, complex>)
      {
        auto inexact = [](auto&& x)
        {
          if constexpr (std::is_same_v<std::decay_t<decltype(x)>, complex>)
          {
            return static_cast<std::complex<double>>(std::forward<decltype(x)>(x));
          }
          else
          {
            return static_cast<double>(std::forward<decltype(x)>(x));
          }
        };

        auto const z = std::pow(inexact(std::forward<decltype(x)>(x)),
                                inexact(std::forward<decltype(y)>(y)));

        return complex(make(z.real()),
                       make(z.imag()));
      }
      else if constexpr (std::is_same_v<T, widen_integer> and
                         std::is_same_v<U, widen_integer>)
      {
        if (auto result = std::pow(x, y); std::numeric_limits<small_integer>::min() <= result and result <= std::numeric_limits<small_integer>::max())
        {
          return make(static_cast<small_integer>(result));
        }
        else
        {
          return make<large_integer>(result);
        }
      }
      else if constexpr (std::is_same_v<T, large_integer> and
                         std::is_same_v<U, large_integer>)
      {
        large_integer result {};
        mpz_pow_ui(result.value, x.value, static_cast<unsigned long>(y));
        return result;
      }
      else
      {
        return std::pow(static_cast<double>(std::forward<decltype(x)>(x)),
                        static_cast<double>(std::forward<decltype(y)>(y)));
      }
    };

    return apply_to<complex_numbers>(f, x, y);
  }

  auto sqrt(object const& x) -> object
  {
    auto f = []<typename T>(T const& x)
    {
      if constexpr (std::is_same_v<T, complex>)
      {
        auto const z = std::sqrt(static_cast<std::complex<double>>(x));

        return complex(make(z.real()),
                       make(z.imag()));
      }
      else
      {
        auto sqrt = [](auto const& x)
        {
          if constexpr (std::is_same_v<T, widen_integer>)
          {
            auto s = std::sqrt(static_cast<double>(x));

            if (auto i = static_cast<small_integer>(s); i * i == x)
            {
              return make(i);
            }
            else
            {
              return make(s);
            }
          }
          else if constexpr (std::is_same_v<T, large_integer>)
          {
            auto const [s, r] = x.sqrt();

            return r == 0_i64 ? make(s) : make(std::sqrt(static_cast<double>(x)));
          }
          else
          {
            return make(std::sqrt(static_cast<double>(x)));
          }
        };

        return x < 0_i64 ? make<complex>(e0, sqrt(0_i64 - x)) : sqrt(x);
      }
    };

    return apply_to<complex_number>(f, x);
  }
} // namespace meevax::kernel::number
