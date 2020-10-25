#ifndef INCLUDED_MEEVAX_KERNEL_BOOLEAN_HPP
#define INCLUDED_MEEVAX_KERNEL_BOOLEAN_HPP

#include <meevax/kernel/pair.hpp>

namespace meevax { inline namespace kernel
{
  struct boolean
  {
    const bool value;

    constexpr operator bool() const noexcept
    {
      return value;
    }
  };

  auto operator <<(std::ostream& port, const boolean&) -> decltype(port);

  extern let const t;
  extern let const f;
}} // namespace meevax::kernel

#endif // INCLUDED_MEEVAX_KERNEL_BOOLEAN_HPP
