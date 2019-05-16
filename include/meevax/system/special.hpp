#ifndef INCLUDED_MEEVAX_SYSTEM_SPECIAL_HPP
#define INCLUDED_MEEVAX_SYSTEM_SPECIAL_HPP

#include <functional> // std::function
#include <string>

#include <meevax/system/pair.hpp> // objective

#define SPECIAL(NAME) objective NAME(const objective&, const objective&, const objective&)

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

  std::ostream& operator<<(std::ostream&, const special&);
} // namespace meevax::system

#endif // INCLUDED_MEEVAX_SYSTEM_SPECIAL_HPP

