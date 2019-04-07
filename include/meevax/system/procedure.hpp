#ifndef INCLUDED_MEEVAX_SYSTEM_PROCEDURE_HPP
#define INCLUDED_MEEVAX_SYSTEM_PROCEDURE_HPP

#include <functional> // std::funstion
#include <iostream>
#include <string>
#include <utility> // std::forward

#include <meevax/system/cursor.hpp>

namespace meevax::system
{
  struct procedure
    : public std::function<cursor (const cursor&)>
  {
    const std::string name;

    using signature = cursor (*)(const cursor&);

    template <typename... Ts>
    procedure(const std::string& name, Ts&&... args)
      : std::function<cursor (const cursor&)> {std::forward<Ts>(args)...}
      , name {name}
    {}
  };

  std::ostream& operator<<(std::ostream& os, const procedure& procedure)
  {
    return os << "#<procedure " << procedure.name << ">";
  }
} // namespace meevax::system

#endif // INCLUDED_MEEVAX_SYSTEM_PROCEDURE_HPP

