#ifndef INCLUDED_MEEVAX_KERNEL_CONTINUATION_HPP
#define INCLUDED_MEEVAX_KERNEL_CONTINUATION_HPP

#include <meevax/kernel/pair.hpp>

namespace meevax { inline namespace kernel
{
  struct continuation
    : public virtual pair
  {
    using pair::pair;

    friend auto operator<<(std::ostream& os, const continuation& k)
      -> decltype(os)
    {
      return os << magenta << "#,("
                << green << "continuation" << reset
                << faint << " ;#" << &k << reset
                << magenta << ")"
                << reset;
    }
  };
}} // namespace meevax::kernel

#endif // INCLUDED_MEEVAX_KERNEL_CONTINUATION_HPP
