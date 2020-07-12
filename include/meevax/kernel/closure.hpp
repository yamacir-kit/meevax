#ifndef INCLUDED_MEEVAX_KERNEL_CLOSURE_HPP
#define INCLUDED_MEEVAX_KERNEL_CLOSURE_HPP

#include <meevax/kernel/pair.hpp>

namespace meevax { inline namespace kernel
{
  /* ==== Closure ==============================================================
  *
  * Closure is pair of expression and lexical-environment
  *
  * ========================================================================= */
  struct closure
    : public virtual pair
  {
    using pair::pair; // inheriting constructors

    friend auto operator <<(std::ostream& os, const closure& c)
      -> decltype(os)
    {
      return os << console::magenta  << "#,("
                << console::green    << "closure"
                << console::reset
                << console::faint << " #;" << &c
                << console::reset
                << console::magenta  << ")"
                << console::reset;
    }
  };
}} // namespace meevax::kernel

#endif // INCLUDED_MEEVAX_KERNEL_CLOSURE_HPP
