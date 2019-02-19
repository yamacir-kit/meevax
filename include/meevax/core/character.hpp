#ifndef INCLUDED_MEEVAX_CORE_CHARACTER_HPP
#define INCLUDED_MEEVAX_CORE_CHARACTER_HPP

#include <utility>

#include <meevax/character/unicode.hpp>

namespace meevax::core
{
  struct character
    : public character::unicode<8>
  {
    template <typename... Ts>
    constexpr character(Ts&&... args)
      : character::unicode<8> {std::forward<Ts>(args)...}
    {}
  };
} // namespace meevax::core

#endif // INCLUDED_MEEVAX_CORE_CHARACTER_HPP

