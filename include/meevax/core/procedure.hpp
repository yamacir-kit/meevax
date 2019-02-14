#ifndef INCLUDED_MEEVAX_CORE_PROCEDURE_HPP
#define INCLUDED_MEEVAX_CORE_PROCEDURE_HPP

#include <functional> // std::funstion
#include <iostream>
#include <string>

#include <meevax/core/cursor.hpp>

namespace meevax::core
{
  class procedure
    : public std::function<cursor (const cursor&)>
  {
    const std::string name;

  public:
    template <typename... Ts>
    procedure(const std::string& name, Ts&&... args)
      : std::function<cursor (const cursor&)> {std::forward<Ts>(args)...}
      , name {name}
    {}

    friend std::ostream& operator<<(std::ostream& os, const procedure& procedure)
    {
      return os << "<procedure: " << procedure.name << ">";
    }
  };
} // namespace meevax::core

#endif // INCLUDED_MEEVAX_CORE_PROCEDURE_HPP

