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

#ifndef INCLUDED_MEEVAX_IOSTREAM_ESCAPE_SEQUENCE_HPP
#define INCLUDED_MEEVAX_IOSTREAM_ESCAPE_SEQUENCE_HPP

#include <functional> // std::reference_wrapper

#include <meevax/iostream/is_console.hpp>

namespace meevax
{
inline namespace iostream
{
  template <typename T>
  struct escape_sequence
  {
    char const* command;

    std::reference_wrapper<T> datum;

    explicit constexpr escape_sequence(char const* command, T&& datum)
      : command { command }
      , datum { std::cref(std::forward<decltype(datum)>(datum)) }
    {}
  };

  template <>
  struct escape_sequence<char const*>
  {
    char const* command;

    char const* datum;

    explicit constexpr escape_sequence(char const* command, char const* datum)
      : command { command }
      , datum { std::forward<decltype(datum)>(datum) }
    {}
  };

  template <typename T>
  auto operator <<(std::ostream & os, escape_sequence<T> const& sequence) -> std::ostream &
  {
    return is_console(os) ? os << "\x1b[" << sequence.command << sequence.datum << "\x1b[0m" : os << sequence.datum;
  }

  auto magenta = [](auto&& datum)
  {
    return escape_sequence<typename std::decay<decltype(datum)>::type>("35m", std::forward<decltype(datum)>(datum));
  };
} // namespace iostream
} // namespace meevax

#endif // INCLUDED_MEEVAX_IOSTREAM_ESCAPE_SEQUENCE_HPP
