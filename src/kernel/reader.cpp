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

#include <regex>
#include <string_view>

#include <meevax/kernel/reader.hpp>
#include <meevax/kernel/string.hpp>

namespace meevax
{
inline namespace kernel
{
  auto circulate(object const& xs, object const& x, std::string const& n) -> void
  {
    if (xs.is<pair>())
    {
      circulate(car(xs), x, n);

      if (cdr(xs).is<datum_label>() and cdr(xs).as<datum_label>().n == n)
      {
        cdr(xs) = x;
      }
      else
      {
        circulate(cdr(xs), x, n);
      }
    }
  }

  auto circulate(object const& xs, std::string const& n) -> void
  {
    return circulate(xs, xs, n);
  }

  auto make_integer(std::string const& token, int radix) -> object
  {
    return make<exact_integer>(token, radix);
  }

  auto make_rational(std::string const& token, int radix) -> object
  {
    try
    {
      return make_integer(token, radix);
    }
    catch (std::invalid_argument const&)
    {
      return make(ratio(token, radix));
    }
  }

  auto make_real(std::string const& token, int radix) -> object
  {
    try
    {
      return make_rational(token, radix);
    }
    catch (std::invalid_argument const&)
    {
      std::unordered_map<std::string_view, double> static const constants
      {
        // R7RS 7.1.1. Lexical structure
        { "+inf.0", +std::numeric_limits<double>::infinity()  },
        { "-inf.0", -std::numeric_limits<double>::infinity()  },
        { "+nan.0", +std::numeric_limits<double>::quiet_NaN() },
        { "-nan.0", -std::numeric_limits<double>::quiet_NaN() },

        // SRFI-144
        { "fl-e",         M_E        },
        { "fl-log2-e",    M_LOG2E    },
        { "fl-log10-e",   M_LOG10E   },
        { "fl-log-2",     M_LN2      },
        { "fl-1/log-2",   M_LN2      },
        { "fl-log-10",    M_LN10     },
        { "fl-1/log-10",  M_LN10     },
        { "fl-pi",        M_PI       },
        { "fl-1/pi",      M_1_PI     },
        { "fl-pi/2",      M_PI_2     },
        { "fl-pi/4",      M_PI_4     },
        { "fl-2/pi",      M_2_PI     },
        { "fl-2/sqrt-pi", M_2_SQRTPI },
        { "fl-sqrt-2",    M_SQRT2    },
        { "fl-1/sqrt-2",  M_SQRT1_2  },
      };

      auto static const pattern = std::regex(R"(([+-]?(?:\d+\.?|\d*\.\d+))([DEFLSdefls][+-]?\d+)?)");

      if (auto iter = constants.find(token); iter != std::end(constants))
      {
        return make(iter->second);
      }
      else if (std::regex_match(token, pattern))
      {
        return make(lexical_cast<double>(token));
      }
      else
      {
        throw std::invalid_argument("not a real number");
      }
    }
  }

  auto make_complex(std::string const& token, int radix) -> object
  {
    try
    {
      return make_real(token, radix);
    }
    catch (std::invalid_argument const&)
    {
      return make(complex(token, radix));
    }
  }

  auto make_number(std::string const& token, int radix) -> object
  {
    try
    {
      return make_complex(token, radix);
    }
    catch (std::invalid_argument const&)
    {
      throw std::invalid_argument("not a number");
    }
  }
} // namespace kernel
} // namespace meevax
