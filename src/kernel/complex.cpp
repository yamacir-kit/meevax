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

#include <regex>

#include <meevax/kernel/number.hpp>
#include <meevax/kernel/reader.hpp>

namespace meevax
{
inline namespace kernel
{
  complex::complex(std::string const& token, int radix)
  {
    std::regex static const rectangular { R"(([+-]?.*)([+-].*)i)" };

    std::regex static const polar { R"(([+-]?.*)@([+-]?.*))" };

    if (std::smatch result; std::regex_match(token, result, rectangular))
    {
      std::get<0>(*this) = string_to_real(result[1].length() == 0 ?                 "0" : result.str(1), radix);
      std::get<1>(*this) = string_to_real(result[2].length() == 1 ? result.str(2) + "1" : result.str(2), radix);
    }
    else if (std::regex_match(token, result, polar))
    {
      auto const magnitude = string_to_real(result.str(1), radix);
      auto const angle     = string_to_real(result.str(2), radix);

      std::get<0>(*this) = magnitude * apply<cos>(angle);
      std::get<1>(*this) = magnitude * apply<sin>(angle);
    }
    else
    {
      throw std::invalid_argument("not a complex number");
    }
  }

  auto complex::canonicalize() const -> value_type
  {
    if (apply<equal_to>(imag(), e0).as<bool>())
    {
      return real();
    }
    else
    {
      return make(*this);
    }
  }

  auto complex::imag() const noexcept -> const_reference
  {
    return second;
  }

  auto complex::real() const noexcept -> const_reference
  {
    return first;
  }

  complex::operator std::complex<double>()
  {
    assert(apply<is_real>(real()));
    assert(apply<is_real>(imag()));

    return std::complex(apply<inexact>(real()).as<double>(),
                        apply<inexact>(imag()).as<double>());
  }

  auto operator <<(std::ostream & os, complex const& z) -> std::ostream &
  {
    if (apply<equal_to>(z.imag(), e0).as<bool>())
    {
      return os << z.real();
    }
    else
    {
      auto explicitly_signed = [](auto const& number)
      {
        switch (auto const s = lexical_cast<std::string>(number); s[0])
        {
        case '+':
        case '-':
          return s;

        default:
          return "+" + s;
        }
      };

      return os << z.real() << cyan(explicitly_signed(z.imag()), "i");
    }
  }
} // namespace kernel
} // namespace meevax
