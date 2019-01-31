#ifndef INCLUDED_MEEVAX_CORE_PROCEDURE_HPP
#define INCLUDED_MEEVAX_CORE_PROCEDURE_HPP

#include <functional>
#include <iostream>
#include <string>

#include <meevax/core/pair.hpp>

namespace meevax::core
{
  class procedure
    : public std::function<cursor (const cursor&)>
  {
    const std::string description;

  public:
    template <typename... Ts>
    procedure(const std::string& description, Ts&&... args)
      : std::function<cursor (const cursor&)> {std::forward<Ts>(args)...}
      , description {description}
    {}

    friend std::ostream& operator<<(std::ostream& os, const procedure& procedure)
    {
      return os << "<procedure: " << description << ">";
    }
  };
} // namespace meevax::core

#endif // INCLUDED_MEEVAX_CORE_PROCEDURE_HPP

