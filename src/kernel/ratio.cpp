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

#include <meevax/kernel/number.hpp>
#include <meevax/posix/vt10x.hpp>

namespace meevax
{
inline namespace kernel
{
  auto ratio::as_exact() const noexcept -> ratio const&
  {
    return *this;
  }

  auto ratio::denominator() const -> exact_integer const&
  {
    return std::get<1>(*this).as<exact_integer>();
  }

  auto ratio::invert() const -> ratio
  {
    return ratio(std::get<1>(*this), std::get<0>(*this));
  }

  auto ratio::is_integer() const -> bool
  {
    return denominator() == 1;
  }

  auto ratio::numerator() const -> exact_integer const&
  {
    return std::get<0>(*this).as<exact_integer>();
  }

  auto ratio::reduce() const -> ratio
  {
    if (auto const common_divisor = numerator().gcd(denominator()); common_divisor != 1)
    {
      return ratio(make<exact_integer>(numerator(), std::divides(), common_divisor),
                   make<exact_integer>(denominator(), std::divides(), common_divisor));
    }
    else
    {
      return *this;
    }
  }

  auto operator <<(std::ostream & os, ratio const& datum) -> std::ostream &
  {
    return os << cyan << car(datum)
              << cyan << "/"
              << cyan << cdr(datum) << reset;
  }
} // namespace kernel
} // namespace meevax
