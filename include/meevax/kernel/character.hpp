#ifndef INCLUDED_MEEVAX_KERNEL_CHARACTER_HPP
#define INCLUDED_MEEVAX_KERNEL_CHARACTER_HPP

#include <unordered_map>

#include <meevax/kernel/object.hpp>

namespace meevax { inline namespace kernel
{
  using variable_width_character = std::string; // TODO convert std::u8string in future.

  /* ---- Character --------------------------------------------------------- */

  struct character
    : public variable_width_character
  {
    const std::string name;

    explicit character(const std::string& code, const std::string& name = {})
      : variable_width_character { code }
      , name { name }
    {}

    auto display() const -> decltype(auto)
    {
      return static_cast<std::string>(*this);
    }

    auto display_to(std::ostream& port) const -> decltype(port);
  };

  auto operator <<(std::ostream& port, const character&) -> decltype(port);

  /* ---- Character Table ------------------------------------------------------
   *
   *   Contains character literal #\<character> or #\<character name>.
   *
   * ------------------------------------------------------------------------ */
  extern const std::unordered_map<std::string, object> characters;

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
