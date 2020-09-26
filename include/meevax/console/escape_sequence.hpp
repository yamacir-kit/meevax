#ifndef INCLUDED_MEEVAX_CONSOLE_ESCAPE_SEQUENCE_HPP
#define INCLUDED_MEEVAX_CONSOLE_ESCAPE_SEQUENCE_HPP

#include <meevax/console/capability.hpp>

namespace meevax { inline namespace console
{
  constexpr auto control_sequnce_introducer { "\x1b[" };

  template <typename... Ts>
  auto escape_sequence(std::ostream& os, Ts&&... xs) -> auto&
  {
    return is_console(os) ? os << control_sequnce_introducer, (os << ... << xs) : os;
  }

  struct cursor_move
  {
    using value_type = std::size_t;

    const value_type value;

    const char code;
  };

  auto operator <<(std::ostream& port, const cursor_move& move) -> decltype(port)
  {
    return escape_sequence(port, move.value, move.code);
  }

  #define DEFINE_CURSOR_MOVE(CODE, DIRECTION)                                  \
  struct cursor_##DIRECTION                                                    \
    : public cursor_move                                                       \
  {                                                                            \
    constexpr cursor_##DIRECTION(cursor_move::value_type value)                \
      : cursor_move { value, CODE }                                            \
    {}                                                                         \
  }

  DEFINE_CURSOR_MOVE('A', up);
  DEFINE_CURSOR_MOVE('B', down);
  DEFINE_CURSOR_MOVE('C', forward);
  DEFINE_CURSOR_MOVE('D', backward);

  #undef DEFINE_CURSOR_MOVE

  #define DEFINE_ESCAPE_SEQUENCE(CODE, NAME)                                   \
  auto NAME = [](std::ostream& os) -> auto&                                    \
  {                                                                            \
    return escape_sequence(os, CODE);                                          \
  }

  DEFINE_ESCAPE_SEQUENCE("0m", reset);
  DEFINE_ESCAPE_SEQUENCE("1m", bold);
  DEFINE_ESCAPE_SEQUENCE("2m", faint);
  DEFINE_ESCAPE_SEQUENCE("3m", italic); // Not widely supported. Sometimes treated as inverse.
  DEFINE_ESCAPE_SEQUENCE("4m", underline);
  DEFINE_ESCAPE_SEQUENCE("5m", slow_blink); // Less than 150 per minite.
  DEFINE_ESCAPE_SEQUENCE("6m", rapid_blink); // More than 150 per minite. Not widely supported.
  DEFINE_ESCAPE_SEQUENCE("7m", reverse);
  DEFINE_ESCAPE_SEQUENCE("8m", conceal); // Not widely supported.

  inline namespace foreground
  {
    DEFINE_ESCAPE_SEQUENCE("30m", black);
    DEFINE_ESCAPE_SEQUENCE("31m", red);
    DEFINE_ESCAPE_SEQUENCE("32m", green);
    DEFINE_ESCAPE_SEQUENCE("33m", yellow);
    DEFINE_ESCAPE_SEQUENCE("34m", blue);
    DEFINE_ESCAPE_SEQUENCE("35m", magenta);
    DEFINE_ESCAPE_SEQUENCE("36m", cyan);
    DEFINE_ESCAPE_SEQUENCE("37m", white);
  }

  namespace background
  {
    DEFINE_ESCAPE_SEQUENCE("40m", black);
    DEFINE_ESCAPE_SEQUENCE("41m", red);
    DEFINE_ESCAPE_SEQUENCE("42m", green);
    DEFINE_ESCAPE_SEQUENCE("43m", yellow);
    DEFINE_ESCAPE_SEQUENCE("44m", blue);
    DEFINE_ESCAPE_SEQUENCE("45m", magenta);
    DEFINE_ESCAPE_SEQUENCE("46m", cyan);
    DEFINE_ESCAPE_SEQUENCE("47m", white);
  }

  DEFINE_ESCAPE_SEQUENCE("?25h", show_cursor);
  DEFINE_ESCAPE_SEQUENCE("?25l", hide_cursor);

  #undef DEFINE_ESCAPE_SEQUENCE
}} // namespace meevax::terminal

#endif // INCLUDED_MEEVAX_CONSOLE_ESCAPE_SEQUENCE_HPP
