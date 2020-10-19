#ifndef INCLUDED_MEEVAX_KERNEL_GHOST_HPP
#define INCLUDED_MEEVAX_KERNEL_GHOST_HPP

#include <meevax/kernel/pair.hpp>

namespace meevax { inline namespace kernel
{
  struct unspecified_t
  {};

  auto operator <<(std::ostream& os, const unspecified_t&) -> decltype(os);

  extern let const unspecified;

  struct undefined_t
  {};

  auto operator <<(std::ostream& os, const undefined_t&) -> decltype(os);

  extern let const undefined;
}} // namespace meevax::kernel

// TODO MOVE FOLLOWING DEFINITIONS INTO src/ghost.cpp
namespace meevax { inline namespace kernel
{
  auto operator <<(std::ostream& os, const unspecified_t&) -> decltype(os)
  {
    return os << faint << "#;unspecified" << reset;
  }

  let const unspecified { make<unspecified_t>() };

  auto operator <<(std::ostream& os, const undefined_t&) -> decltype(os)
  {
    return os << faint << "#;undefined" << reset;
  }

  let const undefined { make<undefined_t>() };
}} // namespace meevax::kernel

#endif // INCLUDED_MEEVAX_KERNEL_GHOST_HPP
