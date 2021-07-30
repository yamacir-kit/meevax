#ifndef INCLUDED_MEEVAX_KERNEL_BOOLEAN_HPP
#define INCLUDED_MEEVAX_KERNEL_BOOLEAN_HPP

#include <meevax/kernel/pair.hpp>

namespace meevax
{
inline namespace kernel
{
  struct boolean
  {
    const bool value;

    constexpr boolean(bool value)
      : value { value }
    {}

    constexpr operator bool() const noexcept
    {
      return value;
    }
  };

  auto operator <<(std::ostream & port, boolean const&) -> std::ostream &;

  extern let const t;
  extern let const f;

  auto if_(let const& x) -> bool;
} // namespace kernel
} // namespace meevax

#endif // INCLUDED_MEEVAX_KERNEL_BOOLEAN_HPP
