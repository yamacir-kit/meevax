#ifndef INCLUDED_MEEVAX_CORE_NAMESCOPE_HPP
#define INCLUDED_MEEVAX_CORE_NAMESCOPE_HPP

#include <cassert>
#include <string>
#include <unordered_map>
#include <utility> // std::forward

#include <meevax/core/cursor.hpp>

namespace meevax::core
{
  struct namescope
    : public std::unordered_map<std::string, cursor>
  {
    template <typename... Ts>
    explicit constexpr namescope(Ts&&... args)
      : std::unordered_map<std::string, cursor> {std::forward<Ts>(args)...}
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
} // namespace meevax::core

#endif // INCLUDED_MEEVAX_CORE_NAMESCOPE_HPP

