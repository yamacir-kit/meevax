#ifndef INCLUDED_MEEVAX_KERNEL_CONTINUATION_HPP
#define INCLUDED_MEEVAX_KERNEL_CONTINUATION_HPP

#include <meevax/kernel/pair.hpp>

namespace meevax
{
inline namespace kernel
{
  struct continuation
    : public virtual pair
  {
    using pair::pair;
  };

  auto operator <<(std::ostream & port, continuation const&) -> decltype(port);
} // namespace kernel
} // namespace meevax

#endif // INCLUDED_MEEVAX_KERNEL_CONTINUATION_HPP
