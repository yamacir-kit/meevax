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

#include <meevax/kernel/list.hpp>
#include <meevax/kernel/textual_output_port.hpp>

namespace meevax
{
inline namespace kernel
{
  auto textual_output_port::flush() -> void
  {
    static_cast<std::ostream &>(*this) << std::flush;
  }

  auto textual_output_port::put(character const& c) -> void
  {
    static_cast<std::ostream &>(*this) << static_cast<std::string>(c);
  }

  auto textual_output_port::put(string const& s) -> void
  {
    static_cast<std::ostream &>(*this) << static_cast<std::string>(s);
  }

  auto textual_output_port::write(object const& x) -> void
  {
    static_cast<std::ostream &>(*this) << x;
  }

  auto textual_output_port::write_simple(object const& x) -> void
  {
    if (auto & os = static_cast<std::ostream &>(*this); x.is<pair>())
    {
      os << magenta("(");

      write_simple(car(x));

      for (let rest = cdr(x); not rest.is<null>(); rest = cdr(rest))
      {
        if (rest.is<pair>())
        {
          os << " ";

          write_simple(car(rest));
        }
        else // xs is the last element of dotted-list.
        {
          os << magenta(" . ");

          write_simple(rest);

          os << magenta(")");
        }
      }

      os << magenta(")");
    }
    else
    {
      os << x;
    }
  }
} // namespace kernel
} // namespace meevax
