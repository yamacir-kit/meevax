#ifndef INCLUDED_MEEVAX_SYSTEM_SYMBOL_HPP
#define INCLUDED_MEEVAX_SYSTEM_SYMBOL_HPP

#include <iostream>
#include <string>
#include <utility>

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

  std::ostream& operator<<(std::ostream& os, const symbol& symbol)
  {
    return os << static_cast<const std::string&>(symbol);
  }
} // namespace meevax::system

#endif // INCLUDED_MEEVAX_SYSTEM_SYMBOL_HPP

