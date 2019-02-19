#ifndef INCLUDED_MEEVAX_SYSTEM_MODULAR_HPP
#define INCLUDED_MEEVAX_SYSTEM_MODULAR_HPP

#include <cassert>
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
    const cursor enclosure;

    template <typename... Ts>
    explicit modular(const cursor& enclosure)
      : enclosure {enclosure}
    {}

    const auto& intern(const std::string& s)
    {
      if (auto iter {find(s)}; iter != std::end(*this))
      {
        return iter->second;
      }
      else
      {
        iter = emplace(s, cursor::bind<std::string>(s)).first;
        return iter->second;
      }
    }

    // returns unchecked reference
    template <typename String>
    [[deprecated]] const auto& reference(String&& s)
    {
      const auto iter {find(s)};
      assert(iter != std::end(*this));
      return iter->second;
    }
  };

  std::ostream& operator<<(std::ostream& os, const modular&)
  {
    return os << "#<module>";
  }
} // namespace meevax::system

#endif // INCLUDED_MEEVAX_SYSTEM_MODULAR_HPP

