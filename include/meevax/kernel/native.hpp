#ifndef INCLUDED_MEEVAX_KERNEL_NATIVE_HPP
#define INCLUDED_MEEVAX_KERNEL_NATIVE_HPP

#include <functional> // std::funstion

#include <meevax/kernel/list.hpp>

#define NATIVE(NAME) \
  meevax::kernel::object NAME([[maybe_unused]] const meevax::kernel::iterator& operands)

namespace meevax::kernel
{
  struct native
    : public std::function<NATIVE()>
  {
    using signature = NATIVE((*));

    const std::string name;

    template <typename... Ts>
    native(const std::string& name, Ts&&... operands)
      : std::function<NATIVE()> {std::forward<decltype(operands)>(operands)...}
      , name {name}
    {}
  };

  // XXX Symmetry breaking
  std::ostream& operator<<(std::ostream& os, const native& native)
  {
    return os << highlight::syntax << "#("
              << highlight::constructor << "native"
              << attribute::normal << " " << native.name
              << highlight::syntax << ")"
              << attribute::normal;
  }
} // namespace meevax::kernel

#endif // INCLUDED_MEEVAX_KERNEL_NATIVE_HPP

