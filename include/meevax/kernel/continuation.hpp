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

    decltype(auto) s() const { return   car(*this); }
    decltype(auto) e() const { return  cadr(*this); }
    decltype(auto) c() const { return caddr(*this); }
    decltype(auto) d() const { return cdddr(*this); }
  };

  auto operator <<(std::ostream & port, continuation const&) -> decltype(port);
} // namespace kernel
} // namespace meevax

#endif // INCLUDED_MEEVAX_KERNEL_CONTINUATION_HPP
