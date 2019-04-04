#ifndef INCLUDED_MEEVAX_SYSTEM_MODULAR_HPP
#define INCLUDED_MEEVAX_SYSTEM_MODULAR_HPP

#include <iostream>
#include <iterator> // std::end
#include <string>
#include <unordered_map>
#include <utility> // std::forward

#include <meevax/system/cursor.hpp>

namespace meevax::system
{
  struct modular
    : public std::unordered_map<std::string, cursor>
  {
    const std::string name;

    template <typename... Ts>
    modular(const std::string& name, Ts&&... args)
      : std::unordered_map<std::string, cursor> {std::forward<Ts>(args)...}
      , name {name}
    {}

    const auto& intern(const std::string& s)
    {
      if (auto iter {find(s)}; iter != std::end(*this))
      {
        return iter->second;
      }
      else
      {
        iter = emplace(s, make<std::string>(s)).first;
        return iter->second;
      }
    }
  };

  std::ostream& operator<<(std::ostream& os, const modular&)
  {
    return os << "#<module>";
  }
} // namespace meevax::system

#endif // INCLUDED_MEEVAX_SYSTEM_MODULAR_HPP

