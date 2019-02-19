#ifndef INCLUDED_MEEVAX_SYSTEM_CHARACTER_HPP
#define INCLUDED_MEEVAX_SYSTEM_CHARACTER_HPP

#include <utility>

#include <meevax/character/unicode.hpp>

namespace meevax::system
{
  struct character
    : public character::unicode<8>
  {
    template <typename... Ts>
    constexpr character(Ts&&... args)
      : character::unicode<8> {std::forward<Ts>(args)...}
    {}
  };
} // namespace meevax::system

#endif // INCLUDED_MEEVAX_SYSTEM_CHARACTER_HPP

