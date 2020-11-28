#ifndef INCLUDED_MEEVAX_KERNEL_CHARACTER_HPP
#define INCLUDED_MEEVAX_KERNEL_CHARACTER_HPP

#include <cstdint>
#include <unordered_map>

#include <meevax/kernel/object.hpp>

namespace meevax { inline namespace kernel
{
  /* ---- Character --------------------------------------------------------- */

  auto encode(std::uint_least32_t code) -> std::string;

  auto decode(std::string const&) -> std::uint_least32_t;

  struct character
    : public std::string
  {
    explicit character(char code)
      : std::string(1, code)
    {}

    explicit character(std::uint32_t code)
      : std::string { encode(code) }
    {}

    template <typename... Ts>
    explicit constexpr character(Ts&&... xs)
      : std::string { std::forward<decltype(xs)>(xs)... }
    {}

    virtual ~character() = default;

    decltype(auto) codepoint() const
    {
      return decode(*this);
    }

    auto display() const -> decltype(auto)
    {
      return static_cast<std::string>(*this);
    }

    auto write_char() const
    {
      return display();
    }

    auto display_to(std::ostream&) const -> std::ostream&;

    auto display_to(let const&) const -> std::ostream&;
  };

  auto operator <<(std::ostream& port, const character&) -> decltype(port);

  auto char_ci_eq = [](auto c, auto... xs) constexpr
  {
    return (std::char_traits<decltype(c)>::eq(c, xs) or ...);
  };

  auto is_intraline_whitespace = [](auto c) constexpr
  {
    return char_ci_eq(c, u8' ', u8'\f', u8'\t', u8'\v');
  };

  auto is_eol = [](auto c) constexpr
  {
    return char_ci_eq(c, u8'\n', u8'\r');
  };

  auto is_eof = [](auto c) constexpr
  {
    using traits = typename std::char_traits<decltype(c)>;

    return traits::eq_int_type(traits::to_int_type(c), traits::eof());
  };

  auto is_whitespace = [](auto c) constexpr
  {
    return is_intraline_whitespace(c)
        or is_eol(c)
        or is_eof(c);
  };

  auto is_parenthesis = [](auto c) constexpr
  {
    return char_ci_eq(c, u8'(', u8')');
  };

  auto is_quotation = [](auto c) constexpr
  {
    return char_ci_eq(c, u8'\'', u8'"', u8'`');
  };

  auto is_vertical_line = [](auto c) constexpr
  {
    return char_ci_eq(c, u8'|');
  };

  auto is_discriminator = [](auto c) constexpr
  {
    return char_ci_eq(c, u8'#');
  };

  auto is_delimiter = [](auto c) constexpr
  {
    return is_whitespace(c)
        or is_parenthesis(c)
        or is_quotation(c)
        or is_discriminator(c)
        or is_vertical_line(c)
        or char_ci_eq(c, u8';', u8',');
  };

  // NOTE in R7RS
  // auto is_delimiter = [](auto c) constexpr
  // {
  //   return is_whitespace(c)
  //       or is_vertical_line(c)
  //       or is_parenthesis(c)
  //       or u8'"'
  //       or u8';';
  // };
}} // namespace meevax::kernel

#endif // INCLUDED_MEEVAX_KERNEL_CHARACTER_HPP
