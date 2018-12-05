#ifndef INCLUDED_MEEVAX_CORE_CLOSURE_HPP
#define INCLUDED_MEEVAX_CORE_CLOSURE_HPP

#include <meevax/core/pair.hpp>

namespace meevax::core
{
  // Closure is pair of function and environment.
  struct closure
    : public virtual pair
  {};
} // namespace meevax::core

#endif // INCLUDED_MEEVAX_CORE_CLOSURE_HPP

