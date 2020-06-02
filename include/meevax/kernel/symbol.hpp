#ifndef INCLUDED_MEEVAX_KERNEL_SYMBOL_HPP
#define INCLUDED_MEEVAX_KERNEL_SYMBOL_HPP

#include <meevax/kernel/object.hpp>

namespace meevax::kernel
{
  struct symbol
    : public std::string
  {
    template <typename... Ts>
    explicit constexpr symbol(Ts&&... xs)
      : std::string { std::forward<decltype(xs)>(xs)... }
    {}

    friend auto operator<<(std::ostream& os, const symbol& symbol)
      -> decltype(os)
    {
      if (symbol.empty())
      {
        /* ---- From R7RS 2.1. Identifiers -------------------------------------
         *
         * Note that || is a valid identifier that is different from any other
         * identifier.
         *
         * ------------------------------------------------------------------ */
        return os << "||";
      }
      else
      {
        return os << static_cast<const std::string&>(symbol);
      }
    }
  };
} // namespace meevax::kernel

#endif // INCLUDED_MEEVAX_KERNEL_SYMBOL_HPP

