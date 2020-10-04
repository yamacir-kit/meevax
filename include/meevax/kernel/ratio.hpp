#ifndef INCLUDED_MEEVAX_KERNEL_RATIO_HPP
#define INCLUDED_MEEVAX_KERNEL_RATIO_HPP

#include <meevax/kernel/pair.hpp>

namespace meevax { inline namespace kernel
{
  struct ratio
    : public virtual pair
  {
  };

  auto operator <<(std::ostream& port, const ratio& rhs) -> decltype(auto)
  {
    return port << cyan << car(rhs)
                << cyan << "/"
                << cyan << cdr(rhs) << reset;
  }
}} // namespace meevax::kernel

#endif // INCLUDED_MEEVAX_KERNEL_RATIO_HPP
