#ifndef INCLUDED_MEEVAX_KERNEL_PROCEDURE_HPP
#define INCLUDED_MEEVAX_KERNEL_PROCEDURE_HPP

#include <functional> // std::funstion
#include <numeric> // std::accumulate

#include <meevax/kernel/list.hpp>

#define PROCEDURE(NAME)                                                        \
  const meevax::kernel::object NAME(                                           \
    [[maybe_unused]] const meevax::kernel::resource& resource,                 \
    [[maybe_unused]] const meevax::kernel::homoiconic_iterator& operands)

namespace meevax::kernel
{
  struct procedure
    : public std::function<PROCEDURE()>
  {
    using signature = PROCEDURE((*));

    const std::string name;

    template <typename... Ts>
    procedure(const std::string& name, Ts&&... operands)
      : std::function<PROCEDURE()> {
          std::forward<decltype(operands)>(operands)...
        }
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

namespace meevax::for_api
{
  #define MEEVAX_BOOLEAN(...) \
  (__VA_ARGS__ ? meevax::kernel::true_object : meevax::kernel::false_object)

  // TODO Rename simply "MEEVAX_FOLD"
  #define MEEVAX_FOLD_ARGUMENTS(INIT, ...) \
  std::accumulate(std::begin(operands), std::end(operands), INIT, __VA_ARGS__)

  #define MEEVAX_BINARY_OPERATION(...) \
  std::invoke(__VA_ARGS__, meevax::kernel::car(operands), meevax::kernel::cadr(operands))
} // namespace meevax::for_api

#endif // INCLUDED_MEEVAX_KERNEL_PROCEDURE_HPP

