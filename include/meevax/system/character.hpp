#ifndef INCLUDED_MEEVAX_SYSTEM_CHARACTER_HPP
#define INCLUDED_MEEVAX_SYSTEM_CHARACTER_HPP

#include <string>

namespace meevax::system
{
  using char8_t = char;

  struct character
    : public std::basic_string<char8_t>
  {
    template <typename... Ts>
    constexpr character(Ts&&... args)
      : std::basic_string<char8_t> {std::forward<Ts>(args)...}
    {}
  };

  std::ostream& operator<<(std::ostream& os, const character& c)
  {
    return os << "\x1B[0;36m#\\" << static_cast<const std::basic_string<char8_t>&>(c) << "\x1b[0m";
  }

  constexpr auto backspace {u8'\b'};
} // namespace meevax::system

#endif // INCLUDED_MEEVAX_SYSTEM_CHARACTER_HPP

