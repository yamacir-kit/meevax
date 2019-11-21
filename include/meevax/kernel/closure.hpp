#ifndef INCLUDED_MEEVAX_KERNEL_CLOSURE_HPP
#define INCLUDED_MEEVAX_KERNEL_CLOSURE_HPP

#include <meevax/kernel/pair.hpp>

namespace meevax::kernel
{
  // closure is pair of expression and lexical-environment
  struct closure
    : public virtual pair
  {
    template <typename... Ts>
    explicit closure(Ts&&... arguments)
      : pair {std::forward<decltype(arguments)>(arguments)...}
    {}
  };

  std::ostream& operator<<(std::ostream& os, const closure& closure)
  {
    return os << highlight::syntax << "#("
              << highlight::constructor << "closure"
              << attribute::normal << highlight::comment << " #;" << &closure << attribute::normal
              << highlight::syntax << ")"
              << attribute::normal;
  }
} // namespace meevax::kernel

#endif // INCLUDED_MEEVAX_KERNEL_CLOSURE_HPP

