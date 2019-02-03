#ifndef INCLUDED_MEEVAX_CORE_CLOSURE_HPP
#define INCLUDED_MEEVAX_CORE_CLOSURE_HPP

#include <iostream>

#include <meevax/core/pair.hpp>

namespace meevax::core
{
  // Closure is pair of function and environment.
  struct closure
    : public virtual pair
  {
    friend std::ostream& operator<<(std::ostream& os, const closure&)
    {
      return os << "<closure>";
    }
  };
} // namespace meevax::core

#endif // INCLUDED_MEEVAX_CORE_CLOSURE_HPP

