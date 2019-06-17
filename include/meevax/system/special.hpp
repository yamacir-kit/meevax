#ifndef INCLUDED_MEEVAX_SYSTEM_SPECIAL_HPP
#define INCLUDED_MEEVAX_SYSTEM_SPECIAL_HPP

#include <functional> // std::function
#include <string>

#include <meevax/system/pair.hpp> // object

#define SPECIAL(NAME) object NAME(const object&, const object&, const object&, bool)

namespace meevax::system
{
  struct special
    : public std::function<SPECIAL()>
  {
    using signature = SPECIAL((*));

    const std::string name;

    template <typename... Ts>
    special(const std::string& name, Ts&&... args)
      : std::function<SPECIAL()> {std::forward<Ts>(args)...}
      , name {name}
    {}
  };

  std::ostream& operator<<(std::ostream& os, const special& special)
  {
    return os << "\x1B[0;36m#<special " << special.name << ">\x1b[0m";
  }
} // namespace meevax::system

#endif // INCLUDED_MEEVAX_SYSTEM_SPECIAL_HPP

