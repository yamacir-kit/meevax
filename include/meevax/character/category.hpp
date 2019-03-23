#ifndef INCLUDED_MEEVAX_CHARACTER_CATEGORY_HPP
#define INCLUDED_MEEVAX_CHARACTER_CATEGORY_HPP

#include <cctype> // std::isgraph, std::isspace

#include <meevax/concepts/is_character.hpp>
#include <meevax/concepts/macro.hpp>

namespace meevax::character
{
  [[deprecated]] auto is_paren = [](auto c) constexpr
  {
    return c == '(' or c == ')';
  };

  [[deprecated]] auto is_macro = [](auto c) constexpr
  {
    return c == '\'' or c == '`' or c == '#';
  };

  [[deprecated]] auto is_delim = [](auto c) constexpr
  {
    return is_paren(c) or std::isspace(c);
  };

  template <typename T>
  struct category
  {};

  struct space
  {
  private:
  };

  struct number
  {
    struct predicator
    {
      template <typename... Ts, REQUIRES(concepts::are_primitive_characters<Ts...>)>
      constexpr decltype(auto) operator()(Ts&&... args) noexcept
      {
        return (is_number(args) && ...);
      }

    private:
      template <typename T>
      constexpr bool is_number(T x) const noexcept
      {
        return 0x2F < x && x < 0x3A;
      }
    } static constexpr predicate {};
  };

  struct graph
    : public category<graph>
  {
    struct predicator
    {
      template <typename... Ts, REQUIRES(concepts::are_primitive_characters<Ts...>)>
      constexpr decltype(auto) operator()(Ts&&... args) noexcept
      {
        return (std::isgraph(args) && ...);
      }
    } static constexpr predicate {};
  };

  template <typename... Ts, REQUIRES(concepts::are_primitive_characters<Ts...>)>
  constexpr decltype(auto) is_space(Ts&&... args)
  {
    return (std::isspace(args) && ...);
  }
} // namespace meevax::character

#endif // INCLUDED_MEEVAX_CHARACTER_CATEGORY_HPP

