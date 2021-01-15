#ifndef INCLUDED_MEEVAX_KERNEL_SYMBOL_HPP
#define INCLUDED_MEEVAX_KERNEL_SYMBOL_HPP

#include <string>

#include <meevax/kernel/preface.hpp> // for bytestring

namespace meevax
{
inline namespace kernel
{
  struct symbol
    : public bytestring
  {
    template <typename... Ts>
    explicit constexpr symbol(Ts&&... xs)
      : bytestring { std::forward<decltype(xs)>(xs)... }
    {}
  };

  auto operator <<(std::ostream & port, symbol const&) -> decltype(port);
} // namespace kernel
} // namespace meevax

#endif // INCLUDED_MEEVAX_KERNEL_SYMBOL_HPP
