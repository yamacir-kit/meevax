#ifndef INCLUDED_MEEVAX_KERNEL_PROCEDURE_HPP
#define INCLUDED_MEEVAX_KERNEL_PROCEDURE_HPP

#include <numeric> // std::accumulate

#include <meevax/kernel/linker.hpp>
#include <meevax/kernel/list.hpp>

namespace meevax { inline namespace kernel
{
  #if __has_cpp_attribute(maybe_unused)
  #define PROCEDURE(...) const meevax::kernel::object __VA_ARGS__([[maybe_unused]] const meevax::kernel::object& xs)
  #else
  #define PROCEDURE(...) const meevax::kernel::object __VA_ARGS__(                 const meevax::kernel::object& xs)
  #endif // __has_cpp_attribute(maybe_unused)

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
      return os << magenta << "#,("
                << green << "procedure "
                << reset << proc.name
                << faint << " #;" << &proc << reset
                << magenta << ")"
                << reset;
    }
  };
}} // namespace meevax::kernel

#endif // INCLUDED_MEEVAX_KERNEL_PROCEDURE_HPP
