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
    return os << highlight::syntax << "#("
              << highlight::constructor << "native"
              << attribute::normal << " " << native.name
              << highlight::syntax << ")"
              << attribute::normal;
  }
} // namespace meevax::system

#endif // INCLUDED_MEEVAX_SYSTEM_NATIVE_HPP

