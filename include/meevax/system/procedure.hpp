#ifndef INCLUDED_MEEVAX_SYSTEM_PROCEDURE_HPP
#define INCLUDED_MEEVAX_SYSTEM_PROCEDURE_HPP

#include <functional> // std::funstion

#include <meevax/system/iterator.hpp>

#define PROCEDURE(NAME) \
  meevax::system::object NAME(const meevax::system::iterator& args)

namespace meevax::system
{
  // TODO RENAME TO "primitive"
  struct procedure
    : public std::function<PROCEDURE()>
  {
    using signature = PROCEDURE((*));

    const std::string name;

    template <typename... Ts>
    procedure(const std::string& name, Ts&&... args)
      : std::function<PROCEDURE()> {std::forward<Ts>(args)...}
      , name {name}
    {}
  };

  // XXX Symmetry breaking
  std::ostream& operator<<(std::ostream& os, const procedure& procedure)
  {
    os << "\x1b[35m" << "#("
       << "\x1b[32m" << "native"
       << "\x1b[0m " << procedure.name
       << "\x1b[35m" << ")";

    return os << "\x1b[0m";
  }
} // namespace meevax::system

#endif // INCLUDED_MEEVAX_SYSTEM_PROCEDURE_HPP

