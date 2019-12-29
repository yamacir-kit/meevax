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
        return os << highlight::syntax << "#("
                  << highlight::type << "symbol"
                  << attribute::normal << highlight::comment << " #;" << &symbol
                  << highlight::syntax << ")"
                  << attribute::normal;
      }
      else
      {
        return os << attribute::normal << static_cast<const std::string&>(symbol);
      }
    }
  };
} // namespace meevax::kernel

#endif // INCLUDED_MEEVAX_KERNEL_SYMBOL_HPP

