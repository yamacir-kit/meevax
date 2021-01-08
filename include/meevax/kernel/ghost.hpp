#ifndef INCLUDED_MEEVAX_KERNEL_GHOST_HPP
#define INCLUDED_MEEVAX_KERNEL_GHOST_HPP

#include <meevax/kernel/pair.hpp>

namespace meevax
{
inline namespace kernel
{
  struct unspecified_t
  {};

  auto operator <<(std::ostream& port, const unspecified_t&) -> decltype(port);

  extern let const unspecified;

  struct undefined_t
  {};

  auto operator <<(std::ostream& port, const undefined_t&) -> decltype(port);

  extern let const undefined;
} // namespace kernel
} // namespace meevax

#endif // INCLUDED_MEEVAX_KERNEL_GHOST_HPP
