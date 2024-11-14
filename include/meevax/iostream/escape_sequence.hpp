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

#ifndef INCLUDED_MEEVAX_IOSTREAM_ESCAPE_SEQUENCE_HPP
#define INCLUDED_MEEVAX_IOSTREAM_ESCAPE_SEQUENCE_HPP

#include <tuple>

#include <meevax/iostream/is_console.hpp>
#include <meevax/utility/unwrap_reference_wrapper.hpp>

namespace meevax::inline iostream
{
  template <typename... Ts>
  struct escape_sequence
  {
    char const* command;

    std::tuple<
      std::conditional_t<
        not std::is_reference_v<Ts> or std::is_scalar_v<std::remove_reference_t<Ts>>,
        std::decay_t<Ts>,
        std::reference_wrapper<std::remove_reference_t<Ts>>
      >...
    > references;

    template <typename T>
    explicit constexpr escape_sequence(T&& x, Ts&&... xs)
      : command { std::forward<decltype(x)>(x) }
      , references { std::forward<decltype(xs)>(xs)... }
    {}

    friend auto operator <<(std::ostream & os, escape_sequence const& sequence) -> std::ostream &
    {
      auto write = [&](auto&&... xs)
      {
        (os << ... << unwrap_reference_wrapper(xs));
      };

      if (is_console(os))
      {
        os << "\x1b[" << sequence.command;
        std::apply(write, sequence.references);
        return os << "\x1b[0m";
      }
      else
      {
        std::apply(write, sequence.references);
        return os;
      }
    }
  };

  template <typename T, typename... Ts>
  escape_sequence(T&&, Ts&&...) -> escape_sequence<Ts...>;

  #define DEFINE(COMMAND, NAME)                                                \
  inline auto NAME = [](auto&&... xs)                                          \
  {                                                                            \
    return escape_sequence(COMMAND, std::forward<decltype(xs)>(xs)...);        \
  }

  DEFINE("0m", normal);
  DEFINE("1m", bold);
  DEFINE("2m", faint);
  DEFINE("3m", italic); // Not widely supported. Sometimes treated as inverse.
  DEFINE("4m", underline);
  DEFINE("5m", slow_blink); // Less than 150 per minite.
  DEFINE("6m", rapid_blink); // More than 150 per minite. Not widely supported.
  DEFINE("7m", reverse);
  DEFINE("8m", conceal); // Not widely supported.

  inline namespace foreground
  {
    DEFINE("30m", black);
    DEFINE("31m", red);
    DEFINE("32m", green);
    DEFINE("33m", yellow);
    DEFINE("34m", blue);
    DEFINE("35m", magenta);
    DEFINE("36m", cyan);
    DEFINE("37m", white);
  }

  namespace background
  {
    DEFINE("40m", black);
    DEFINE("41m", red);
    DEFINE("42m", green);
    DEFINE("43m", yellow);
    DEFINE("44m", blue);
    DEFINE("45m", magenta);
    DEFINE("46m", cyan);
    DEFINE("47m", white);
  }

  #undef DEFINE
} // namespace meevax::iostream

#endif // INCLUDED_MEEVAX_IOSTREAM_ESCAPE_SEQUENCE_HPP
