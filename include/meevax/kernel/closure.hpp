#ifndef INCLUDED_MEEVAX_KERNEL_CLOSURE_HPP
#define INCLUDED_MEEVAX_KERNEL_CLOSURE_HPP

#include <meevax/kernel/pair.hpp>

namespace meevax { inline namespace kernel
{
  struct closure
    : public virtual pair
  {
    using pair::pair; // inheriting constructors

    friend auto operator <<(std::ostream& os, const closure& c)
      -> decltype(os)
    {
      return os << magenta << "#,("
                << green << "closure" << reset
                << faint << " #;" << &c
                << reset
                << magenta << ")"
                << reset;
    }
  };
}} // namespace meevax::kernel

#endif // INCLUDED_MEEVAX_KERNEL_CLOSURE_HPP
