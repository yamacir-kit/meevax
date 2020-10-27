#ifndef INCLUDED_MEEVAX_KERNEL_CLOSURE_HPP
#define INCLUDED_MEEVAX_KERNEL_CLOSURE_HPP

#include <meevax/kernel/pair.hpp>

namespace meevax { inline namespace kernel
{
  struct closure
    : public virtual pair
  {
    using pair::pair;
  };

  auto operator <<(std::ostream& port, const closure& datum) -> decltype(port);
}} // namespace meevax::kernel

#endif // INCLUDED_MEEVAX_KERNEL_CLOSURE_HPP
