#ifndef INCLUDED_MEEVAX_KERNEL_PROCEDURE_HPP
#define INCLUDED_MEEVAX_KERNEL_PROCEDURE_HPP

#include <functional> // std::funstion

#include <meevax/kernel/list.hpp>

#define PROCEDURE(NAME) \
  const meevax::kernel::object NAME([[maybe_unused]] const meevax::kernel::iterator& operands)

namespace meevax::kernel
{
  struct procedure
    : public std::function<PROCEDURE()>
  {
    using signature = PROCEDURE((*));

    const std::string name;

    template <typename... Ts>
    procedure(const std::string& name, Ts&&... operands)
      : std::function<PROCEDURE()> {std::forward<decltype(operands)>(operands)...}
      , name {name}
    {}
  };

  // XXX Symmetry breaking
  std::ostream& operator<<(std::ostream& os, const procedure& procedure)
  {
    return os << highlight::syntax << "#("
              << highlight::constructor << "procedure"
              << attribute::normal << " " << procedure.name
              << highlight::syntax << ")"
              << attribute::normal;
  }
} // namespace meevax::kernel

#endif // INCLUDED_MEEVAX_KERNEL_PROCEDURE_HPP

