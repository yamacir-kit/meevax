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

#include <regex>

#include <meevax/kernel/list.hpp>
#include <meevax/kernel/number.hpp>

namespace meevax::inline kernel
{
  complex::complex(std::string const& token, int radix)
  {
    std::regex static const rectangular { R"(([+-]?.*)([+-].*)i)" };

    std::regex static const polar { R"(([+-]?.*)@([+-]?.*))" };

    if (std::smatch result; std::regex_match(token, result, rectangular))
    {
      std::get<0>(*this) = make_real(result[1].length() == 0 ?                 "0" : result.str(1), radix);
      std::get<1>(*this) = make_real(result[2].length() == 1 ? result.str(2) + "1" : result.str(2), radix);
    }
    else if (std::regex_match(token, result, polar))
    {
      auto const magnitude = make_real(result.str(1), radix);
      auto const angle     = make_real(result.str(2), radix);

      std::get<0>(*this) = magnitude * cos(angle);
      std::get<1>(*this) = magnitude * sin(angle);
    }
    else
    {
      throw std::invalid_argument("not a complex number");
    }
  }

  auto complex::canonicalize() const -> object
  {
    if (equals(imag(), e0))
    {
      return real();
    }
    else
    {
      return make(*this);
    }
  }

  auto complex::imag() const noexcept -> object const&
  {
    return second;
  }

  auto complex::real() const noexcept -> object const&
  {
    return first;
  }

  complex::operator std::complex<int>() const
  {
    assert(is_real(real()));
    assert(is_real(imag()));

    auto to_int = [](let const& x)
    {
      if (x.is<exact_integer>())
      {
        return static_cast<int>(x.as<exact_integer>());
      }
      else if (x.is<ratio>())
      {
        return static_cast<int>(x.as<ratio>());
      }
      else
      {
        assert(x.is<std::int32_t>());
        return static_cast<int>(x.as<std::int32_t>());
      }
    };

    return std::complex(to_int(exact(real())),
                        to_int(exact(imag())));
  }

  complex::operator std::complex<double>() const
  {
    assert(is_real(real()));
    assert(is_real(imag()));

    auto to_double = [](let const& x)
    {
      if (x.is<double>())
      {
        return x.as<double>();
      }
      else
      {
        assert(x.is<float>());
        return static_cast<double>(x.as<float>());
      }
    };

    return std::complex(to_double(inexact(real())),
                        to_double(inexact(imag())));
  }

  auto operator <<(std::ostream & os, complex const& z) -> std::ostream &
  {
    if (equals(z.imag(), e0))
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

  auto real_part(object const& x) -> object const&
  {
    return x.is<complex>() ? car(x) : x;
  }

  auto imag_part(object const& x) -> object const&
  {
    return x.is<complex>() ? cdr(x) : e0;
  }

  auto magnitude(object const& x) -> object
  {
    auto hypotenuse = [](let const& x, let const& y)
    {
      return sqrt(x * x + y * y);
    };

    return hypotenuse(real_part(x),
                      imag_part(x));
  }

  auto angle(object const& x) -> object
  {
    return atan(real_part(x),
                imag_part(x));
  }
} // namespace meevax::kernel
