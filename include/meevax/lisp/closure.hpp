#ifndef INCLUDED_MEEVAX_LISP_CLOSURE_HPP
#define INCLUDED_MEEVAX_LISP_CLOSURE_HPP

#include <meevax/lisp/cell.hpp>

namespace meevax::lisp
{
  // Closure is pair of function and environment.
  struct closure
    : public virtual cell
  {};
} // namespace meevax::lisp

#endif // INCLUDED_MEEVAX_LISP_CLOSURE_HPP

