#ifndef INCLUDED_MEEVAX_KERNEL_PROCEDURE_HPP
#define INCLUDED_MEEVAX_KERNEL_PROCEDURE_HPP

#include <numeric> // std::accumulate

#include <meevax/kernel/list.hpp>

#define PROCEDURE(...) \
  const meevax::kernel::object __VA_ARGS__([[maybe_unused]] const meevax::kernel::object& xs)

namespace meevax::kernel
{
  struct procedure
    : public std::function<PROCEDURE()>
  {
    using signature = PROCEDURE((*));

    const std::string name;

    template <typename... Ts>
    explicit procedure(const std::string& name, Ts&&... xs)
      : std::function<PROCEDURE()> { std::forward<decltype(xs)>(xs)...  }
      , name {name}
    {}

    friend auto operator<<(std::ostream& os, const procedure& proc)
      -> decltype(auto)
    {
      return os << console::magenta << "#,("
                << console::green << "procedure "
                << console::reset << proc.name
                << console::faint << " #;" << &proc
                << console::reset
                << console::magenta << ")"
                << console::reset;
    }
  };
} // namespace meevax::kernel

#define MEEVAX_API_TYPE_PREDICATE(...) \
  kernel::convert(meevax::kernel::car(xs).is<__VA_ARGS__>())

#define MEEVAX_API_FOLD(X, ...) \
  std::accumulate(std::begin(X), std::end(X), __VA_ARGS__)

#define MEEVAX_BINARY_OPERATION(...) \
  std::invoke(__VA_ARGS__, meevax::kernel::car(xs), meevax::kernel::cadr(xs))

#endif // INCLUDED_MEEVAX_KERNEL_PROCEDURE_HPP

