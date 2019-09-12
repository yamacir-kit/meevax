#ifndef INCLUDED_MEEVAX_SYSTEM_NATIVE_HPP
#define INCLUDED_MEEVAX_SYSTEM_NATIVE_HPP

#include <functional> // std::funstion

#include <meevax/system/list.hpp>

// TODO Rename args to operands
#define NATIVE(NAME) \
  meevax::system::object NAME([[maybe_unused]] const meevax::system::iterator& args)

namespace meevax::system
{
  struct native
    : public std::function<NATIVE()>
  {
    using signature = NATIVE((*));

    const std::string name;

    template <typename... Ts>
    native(const std::string& name, Ts&&... args)
      : std::function<NATIVE()> {std::forward<Ts>(args)...}
      , name {name}
    {}
  };

  // XXX Symmetry breaking
  std::ostream& operator<<(std::ostream& os, const native& native)
  {
    os << "\x1b[35m" << "#("
       << "\x1b[32m" << "native"
       << "\x1b[0m " << native.name
       << "\x1b[35m" << ")";

    return os << "\x1b[0m";
  }
} // namespace meevax::system

#endif // INCLUDED_MEEVAX_SYSTEM_NATIVE_HPP

