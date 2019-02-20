#ifndef INCLUDED_MEEVAX_CHARACTER_PREDICATE_HPP
#define INCLUDED_MEEVAX_CHARACTER_PREDICATE_HPP

#include <cctype>

#include <meevax/concepts/is_character.hpp>
#include <meevax/concepts/macro.hpp>

namespace meevax::character
{
  struct graph
  {
    template <typename... Ts, REQUIRES(concepts::are_primitive_characters<Ts...>)>
    constexpr decltype(auto) operator()(Ts&&... args) noexcept
    {
      return (std::isgraph(args) && ...);
    }
  } is_graph;

  template <typename... Ts, REQUIRES(concepts::are_primitive_characters<Ts...>)>
  constexpr decltype(auto) is_space(Ts&&... args)
  {
    return (std::isspace(args) && ...);
  }
} // namespace meevax::character

#endif // INCLUDED_MEEVAX_CHARACTER_PREDICATE_HPP

