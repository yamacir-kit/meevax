#ifndef INCLUDED_MEEVAX_SYSTEM_MODULE_HPP
#define INCLUDED_MEEVAX_SYSTEM_MODULE_HPP

#include <iterator> // std::end
#include <unordered_map>

#include <meevax/system/cursor.hpp>
#include <meevax/system/symbol.hpp>

namespace meevax::system
{
  struct module
    : public std::unordered_map<std::string, cursor>
  {
    const std::string name;

    template <typename... Ts>
    module(const std::string& name, Ts&&... args)
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
        iter = emplace(s, make<symbol>(s)).first;
        return iter->second;
      }
    }
  };

  std::ostream& operator<<(std::ostream& os, const module&)
  {
    return os << "#<module>";
  }
} // namespace meevax::system

#endif // INCLUDED_MEEVAX_SYSTEM_MODULE_HPP

