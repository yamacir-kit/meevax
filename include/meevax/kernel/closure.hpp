#ifndef INCLUDED_MEEVAX_KERNEL_CLOSURE_HPP
#define INCLUDED_MEEVAX_KERNEL_CLOSURE_HPP

#include <meevax/kernel/pair.hpp>

namespace meevax::kernel
{
  // closure is pair of expression and lexical-environment
  struct closure
    : public virtual pair
  {
    using identity = closure;

    using pair::pair; // inheriting constructors

    friend auto operator <<(std::ostream& os, const identity& i)
      -> decltype(os)
    {
      return os << posix::highlight::syntax  << "#,("
                << posix::highlight::type    << "closure"
                << posix::attribute::normal
                << posix::highlight::comment << " #;" << &i
                << posix::attribute::normal
                << posix::highlight::syntax  << ")"
                << posix::attribute::normal;
    }
  };
} // namespace meevax::kernel

#endif // INCLUDED_MEEVAX_KERNEL_CLOSURE_HPP

