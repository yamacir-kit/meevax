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
#include <type_traits>
#include <utility> // std::tuple

#include <meevax/iostream/is_console.hpp>

namespace meevax
{
inline namespace iostream
{
  template <typename... Ts>
  struct escape_sequence
  {
    char const* command;

    std::tuple<
      typename std::conditional<
        std::is_scalar<typename std::decay<Ts>::type>::value,
        typename std::decay<Ts>::type,
        std::reference_wrapper<typename std::decay<Ts>::type>
      >::type...
    > references;

    explicit constexpr escape_sequence(char const* command, Ts&&... xs)
      : command { command }
      , references { std::forward<decltype(xs)>(xs)... }
    {}

    friend auto operator <<(std::ostream & os, escape_sequence const& sequence) -> std::ostream &
    {
      auto print = [&](auto&& ... xs) -> std::ostream &
      {
        return (os << ... << xs);
      };

      if (is_console(os))
      {
        os << "\x1b[" << sequence.command;
        std::apply(print, sequence.references);
        return os << "\x1b[0m";
      }
      else
      {
        std::apply(print, sequence.references);
        return os;
      }
    }
  };

  template <typename... Ts>
  escape_sequence(char const*, Ts&&...) -> escape_sequence<Ts...>;

  #define DEFINE(COMMAND, NAME)                                                \
  auto NAME = [](auto&&... xs)                                                 \
  {                                                                            \
    return escape_sequence(COMMAND, std::forward<decltype(xs)>(xs)...); \
  }

  inline namespace foreground
  {
    // DEFINE("30m", black);
    DEFINE("31m", red);
    // DEFINE("32m", green);
    // DEFINE("33m", yellow);
    // DEFINE("34m", blue);
    DEFINE("35m", magenta);
    // DEFINE("36m", cyan);
    // DEFINE("37m", white);
  }

  namespace background
  {
    // DEFINE("40m", black);
    // DEFINE("41m", red);
    // DEFINE("42m", green);
    // DEFINE("43m", yellow);
    // DEFINE("44m", blue);
    // DEFINE("45m", magenta);
    // DEFINE("46m", cyan);
    // DEFINE("47m", white);
  }

  #undef DEFINE
} // namespace iostream
} // namespace meevax

#endif // INCLUDED_MEEVAX_IOSTREAM_ESCAPE_SEQUENCE_HPP
