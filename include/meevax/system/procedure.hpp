#ifndef INCLUDED_MEEVAX_SYSTEM_PROCEDURE_HPP
#define INCLUDED_MEEVAX_SYSTEM_PROCEDURE_HPP

#include <functional> // std::funstion

#include <meevax/system/cursor.hpp>

#define PROCEDURE(NAME) \
  meevax::system::objective NAME(const meevax::system::cursor& args)

namespace meevax::system
{
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

  std::ostream& operator<<(std::ostream&, const procedure&);
} // namespace meevax::system

#endif // INCLUDED_MEEVAX_SYSTEM_PROCEDURE_HPP

