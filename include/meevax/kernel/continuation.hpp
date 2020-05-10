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
      return os << console::magenta << "#,("
                << console::green << "continuation"
                << console::reset
                << console::faint << " ;#" << &i
                << console::reset
                << console::magenta << ")"
                << console::reset;
    }
  };
} // namespace meevax::kernel

#endif // INCLUDED_MEEVAX_KERNEL_CONTINUATION_HPP

