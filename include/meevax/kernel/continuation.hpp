#ifndef INCLUDED_MEEVAX_KERNEL_CONTINUATION_HPP
#define INCLUDED_MEEVAX_KERNEL_CONTINUATION_HPP

#include <meevax/kernel/pair.hpp>

namespace meevax::kernel
{
  DERIVE(continuation, public virtual, pair)

  std::ostream& operator<<(std::ostream& os, const continuation& continuation)
  {
    return os << highlight::syntax << "#("
              << highlight::constructor << "continuation"
              << attribute::normal << highlight::comment << " ;#" << &continuation << attribute::normal
              << highlight::syntax << ")"
              << attribute::normal;
  }
} // namespace meevax::kernel

#endif // INCLUDED_MEEVAX_KERNEL_CONTINUATION_HPP

