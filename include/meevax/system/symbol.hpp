#ifndef INCLUDED_MEEVAX_SYSTEM_SYMBOL_HPP
#define INCLUDED_MEEVAX_SYSTEM_SYMBOL_HPP

#include <string>

#include <meevax/system/writer.hpp>

namespace meevax::system
{
  struct symbol
    : public std::string
  {
    template <typename... Ts>
    explicit constexpr symbol(Ts&&... args)
      : std::string {std::forward<Ts>(args)...}
    {}

    operator std::string() const
    {
      return *this;
    }
  };

  auto operator<<(std::ostream& os, const symbol& symbol)
    -> decltype(os)
  {
    if (symbol.empty())
    {
      return os << "\x1b[35m" << "#("
                << "\x1b[32m" << "symbol"
                << "\x1b[0m " << "#;" << &symbol
                << "\x1b[35m" << ")"
                << "\x1b[0m";
    }
    else
    {
      return os << static_cast<const std::string&>(symbol);
    }
  }
} // namespace meevax::system

#endif // INCLUDED_MEEVAX_SYSTEM_SYMBOL_HPP

