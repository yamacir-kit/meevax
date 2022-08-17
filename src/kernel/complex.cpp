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

#include <meevax/kernel/number.hpp>
#include <meevax/kernel/reader.hpp>

namespace meevax
{
inline namespace kernel
{
  complex::complex(std::string const& token, int radix)
  {
    if (std::smatch result; std::regex_match(token, result, pattern()))
    {
      PRINT(result.str(0));
      PRINT(result.str(1));
      PRINT(result.str(2));
    }
    else
    {
      throw std::invalid_argument("not a complex number");
    }
  }

  auto complex::imag() const noexcept -> const_reference
  {
    return second;
  }

  auto complex::pattern() -> std::regex const&
  {
    std::regex static const pattern { R"(([+-]?.*)([+-].*)i)" };
    return pattern;
  }

  auto complex::real() const noexcept -> const_reference
  {
    return first;
  }

  auto operator <<(std::ostream & os, complex const& z) -> std::ostream &
  {
    return os << z.real() << cyan(apply<std::less<void>>(e0, z.imag()).as<bool>() ? '+' : '-') << z.imag() << cyan("i");
  }
} // namespace kernel
} // namespace meevax
