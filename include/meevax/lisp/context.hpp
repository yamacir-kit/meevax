#ifndef INCLUDED_MEEVAX_LISP_CONTEXT_HPP
#define INCLUDED_MEEVAX_LISP_CONTEXT_HPP

#include <cassert>
#include <string>
#include <unordered_map>
#include <utility>

#include <meevax/lisp/cell.hpp>
#include <meevax/utility/type_erasure.hpp>

namespace meevax::lisp
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
      if (const auto& iter {find(s)}; iter != std::end(*this))
      {
        return iter->second;
      }
      else return emplace(
        s, std::make_shared<utility::binder<std::string, cell>>(s)
      ).first->second;
    }

    // returns unchecked reference
    // TODO rename to 'reference'
    template <typename String>
    const auto& lookup(String&& s)
    {
      const auto& iter {find(s)};
      assert(iter != std::end(*this));
      return iter->second;
    }
  };

  // XXX temporary
  context default_context {};
} // namespace meevax::lisp

#endif // INCLUDED_MEEVAX_LISP_CONTEXT_HPP

