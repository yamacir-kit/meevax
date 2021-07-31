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
  auto ratio::numerator() const -> exact_integer const&
  {
    return std::get<0>(*this).as<exact_integer>();
  }

  auto ratio::denominator() const -> exact_integer const&
  {
    return std::get<1>(*this).as<exact_integer>();
  }

  auto ratio::is_integer() const -> bool
  {
    return cdr(*this).as<exact_integer>() == 1;
  }

  auto ratio::invert() const -> ratio
  {
    return ratio(cdr(*this),
                 car(*this));
  }

  auto ratio::reduce() const -> ratio
  {
    using boost::multiprecision::gcd;

    if (const exact_integer divisor {
          gcd(car(*this).as<exact_integer>().value,
              cdr(*this).as<exact_integer>().value) }; divisor != 1)
    {
      return ratio(make<exact_integer>(car(*this).as<exact_integer>().value / divisor.value),
                   make<exact_integer>(cdr(*this).as<exact_integer>().value / divisor.value));
    }
    else
    {
      return *this;
    }
  }

  auto operator <<(output_port & port, ratio const& datum) -> output_port &
  {
    return port << cyan << car(datum)
                << cyan << "/"
                << cyan << cdr(datum) << reset;
  }
} // namespace kernel
} // namespace meevax
