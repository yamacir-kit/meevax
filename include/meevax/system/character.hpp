#ifndef INCLUDED_MEEVAX_SYSTEM_CHARACTER_HPP
#define INCLUDED_MEEVAX_SYSTEM_CHARACTER_HPP

#include <string>
#include <unordered_map>

#include <meevax/system/pair.hpp>

namespace meevax::system
{
  struct character
    : public std::string
  {
    template <typename... Ts>
    explicit constexpr character(Ts&&... xs)
      : std::string {std::forward<Ts>(xs)...}
    {}
  };

  std::ostream& operator<<(std::ostream& os, const character& c)
  {
    return os << "\x1b[0;36m#\\" << static_cast<const std::string&>(c) << "\x1b[0m";
  }

  extern "C" std::unordered_map<std::string, object> characters;
} // namespace meevax::system

#endif // INCLUDED_MEEVAX_SYSTEM_CHARACTER_HPP

