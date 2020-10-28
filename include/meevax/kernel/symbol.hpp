#ifndef INCLUDED_MEEVAX_KERNEL_SYMBOL_HPP
#define INCLUDED_MEEVAX_KERNEL_SYMBOL_HPP

#include <string>

namespace meevax { inline namespace kernel
{
  struct symbol
    : public std::string
  {
    template <typename... Ts>
    explicit constexpr symbol(Ts&&... xs)
      : std::string { std::forward<decltype(xs)>(xs)... }
    {}
  };

  auto operator <<(std::ostream& port, const symbol&) -> decltype(port);
}} // namespace meevax::kernel

#endif // INCLUDED_MEEVAX_KERNEL_SYMBOL_HPP
