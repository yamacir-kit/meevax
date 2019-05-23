#ifndef INCLUDED_MEEVAX_SYSTEM_CLOSURE_HPP
#define INCLUDED_MEEVAX_SYSTEM_CLOSURE_HPP

#include <meevax/system/pair.hpp>

namespace meevax::system
{
  struct closure // is pair of compiled procedure and environment.
    : public virtual pair
  {
    template <typename... Ts>
    constexpr closure(Ts&&... args)
      : pair {std::forward<Ts>(args)...}
    {}
  };

  std::ostream& operator<<(std::ostream&, const closure&);
} // namespace meevax::system

#endif // INCLUDED_MEEVAX_SYSTEM_CLOSURE_HPP

