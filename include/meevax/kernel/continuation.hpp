#ifndef INCLUDED_MEEVAX_KERNEL_CONTINUATION_HPP
#define INCLUDED_MEEVAX_KERNEL_CONTINUATION_HPP

#include <meevax/kernel/pair.hpp>

namespace meevax::kernel
{
  struct continuation
    : public virtual pair
  {
    using identity = continuation;

    using pair::pair;

    friend auto operator<<(std::ostream& os, const identity& i)
      -> decltype(os)
    {
      return os << highlight::syntax << "#("
                << highlight::constructor << "continuation"
                << attribute::normal << highlight::comment << " ;#" << &i << attribute::normal
                << highlight::syntax << ")"
                << attribute::normal;
    }
  };
} // namespace meevax::kernel

#endif // INCLUDED_MEEVAX_KERNEL_CONTINUATION_HPP

