#ifndef INCLUDED_MEEVAX_KERNEL_CONTINUATION_HPP
#define INCLUDED_MEEVAX_KERNEL_CONTINUATION_HPP

#include <meevax/kernel/list.hpp>

namespace meevax
{
inline namespace kernel
{
  struct continuation
    : public virtual pair
  {
    using pair::pair;

    auto s() const { return   car(*this); }
    auto e() const { return  cadr(*this); }
    auto c() const { return caddr(*this); }
    auto d() const { return cdddr(*this); }
  };

  auto operator <<(std::ostream &, continuation const&) -> std::ostream &;
} // namespace kernel
} // namespace meevax

#endif // INCLUDED_MEEVAX_KERNEL_CONTINUATION_HPP
