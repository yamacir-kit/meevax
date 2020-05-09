#ifndef INCLUDED_MEEVAX_KERNEL_PROCEDURE_HPP
#define INCLUDED_MEEVAX_KERNEL_PROCEDURE_HPP

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
    explicit procedure(const std::string& name, Ts&&... operands)
      : std::function<PROCEDURE()> {
          std::forward<decltype(operands)>(operands)...
        }
      , name {name}
    {}

    friend auto operator<<(std::ostream& os, const procedure& procedure)
      -> decltype(auto)
    {
      return os << console::magenta  << "#("
                << console::green    << "procedure "
                << console::reset  << procedure.name
                << console::faint << " #;" << &procedure
                << console::reset
                << console::magenta  << ")"
                << console::reset;
    }
  };
} // namespace meevax::kernel

#define MEEVAX_API_TYPE_PREDICATE(...)                                         \
  kernel::convert(                                                          \
    meevax::kernel::car(operands).is<__VA_ARGS__>())

#define MEEVAX_API_FOLD(X, ...)                                                \
  std::accumulate(std::begin(X), std::end(X), __VA_ARGS__)

#define MEEVAX_BINARY_OPERATION(...) \
std::invoke(__VA_ARGS__, meevax::kernel::car(operands), meevax::kernel::cadr(operands))

#endif // INCLUDED_MEEVAX_KERNEL_PROCEDURE_HPP

