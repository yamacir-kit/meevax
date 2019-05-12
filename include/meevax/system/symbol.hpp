#ifndef INCLUDED_MEEVAX_SYSTEM_SYMBOL_HPP
#define INCLUDED_MEEVAX_SYSTEM_SYMBOL_HPP

#include <string>

#include <meevax/system/cursor.hpp>

namespace meevax::system
{
  struct symbol
    : public std::string
  {
    template <typename... Ts>
    constexpr symbol(Ts&&... args)
      : std::string {std::forward<Ts>(args)...}
    {}
  };
} // namespace meevax::system

#endif // INCLUDED_MEEVAX_SYSTEM_SYMBOL_HPP

