#ifndef INCLUDED_MEEVAX_CORE_CONTEXT_HPP
#define INCLUDED_MEEVAX_CORE_CONTEXT_HPP

#include <cassert>
#include <string>
#include <unordered_map>
#include <utility>

#include <meevax/core/cursor.hpp>

// TODO rename to "hash_table"

namespace meevax::core
{
  struct context
    : public std::unordered_map<std::string, cursor>
  {
    template <typename... Ts>
    explicit constexpr context(Ts&&... args)
      : std::unordered_map<std::string, cursor> {std::forward<Ts>(args)...}
    {}

    template <typename String>
    const auto& intern(String&& s)
    {
      if (const auto iter {find(s)}; iter != std::end(*this))
      {
        return iter->second;
      }
      else return emplace(s, cursor::bind<std::string>(s)).first->second;
    }

    // returns unchecked reference
    // TODO rename to 'reference'
    template <typename String>
    const auto& lookup(String&& s)
    {
      const auto iter {find(s)};
      assert(iter != std::end(*this));
      return iter->second;
    }
  };
} // namespace meevax::core

#endif // INCLUDED_MEEVAX_CORE_CONTEXT_HPP

