#ifndef INCLUDED_MEEVAX_SYSTEM_CLOSURE_HPP
#define INCLUDED_MEEVAX_SYSTEM_CLOSURE_HPP

#include <meevax/system/pair.hpp>

namespace meevax::system
{
  // Closure is pair of function and environment.
  struct closure
    : public virtual pair
  {
    friend std::ostream& operator<<(std::ostream& os, const closure&)
    {
      return os << "#<closure>";
    }
  };
} // namespace meevax::system

#endif // INCLUDED_MEEVAX_SYSTEM_CLOSURE_HPP

