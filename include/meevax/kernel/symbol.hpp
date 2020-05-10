#ifndef INCLUDED_MEEVAX_KERNEL_SYMBOL_HPP
#define INCLUDED_MEEVAX_KERNEL_SYMBOL_HPP

#include <meevax/kernel/object.hpp>

namespace meevax::kernel
{
  struct symbol
    : public std::string
  {
    template <typename... Ts>
    explicit constexpr symbol(Ts&&... operands)
      : std::string {std::forward<decltype(operands)>(operands)...}
    {}

    friend auto operator<<(std::ostream& os, const symbol& symbol)
      -> decltype(os)
    {
      if (symbol.empty())
      {
        return os << console::magenta << "#("
                  << console::green << "symbol"
                  << console::reset
                  << console::faint << " #;" << &symbol
                  << console::magenta << ")"
                  << console::reset;
      }
      else
      {
        return os << console::reset << static_cast<const std::string&>(symbol);
      }
    }
  };
} // namespace meevax::kernel

#endif // INCLUDED_MEEVAX_KERNEL_SYMBOL_HPP

