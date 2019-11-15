#ifndef INCLUDED_MEEVAX_KERNEL_SPECIAL_HPP
#define INCLUDED_MEEVAX_KERNEL_SPECIAL_HPP

#include <functional> // std::function

#include <meevax/kernel/object.hpp>

#define SPECIAL(NAME) \
  const meevax::kernel::object NAME(const meevax::kernel::object&, \
                                    const meevax::kernel::object&, \
                                    const meevax::kernel::object&, \
                                    const bool)

namespace meevax::kernel
{
  struct special
    : public std::function<SPECIAL()>
  {
    using signature = SPECIAL((*));

    const std::string name;

    template <typename... Ts>
    special(const std::string& name, Ts&&... operands)
      : std::function<SPECIAL()> {std::forward<decltype(operands)>(operands)...}
      , name {name}
    {}
  };

  std::ostream& operator<<(std::ostream& os, const special& special)
  {
    return os << highlight::syntax << "#("
              << highlight::constructor << "special"
              << attribute::normal << " " << special.name
              << highlight::syntax << ")"
              << attribute::normal;
  }
} // namespace meevax::kernel

#endif // INCLUDED_MEEVAX_KERNEL_SPECIAL_HPP

