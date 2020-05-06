#ifndef INCLUDED_MEEVAX_KERNEL_CHARACTER_HPP
#define INCLUDED_MEEVAX_KERNEL_CHARACTER_HPP

#include <unordered_map>

#include <meevax/kernel/pair.hpp>

namespace meevax::kernel
{
  /* ==== Character ============================================================
  *
  * TODO
  *
  * ========================================================================= */
  struct character
    : public std::string // TODO convert std::u8string in future.
  {
    const std::string name;

    explicit character(const std::string& unicode,
                       const std::string& name = {})
      : std::string {unicode}
      , name {name}
    {}

    friend auto operator<<(std::ostream& os, const character& c)
      -> decltype(os)
    {
      return os << console::cyan << "#\\"
                << (std::empty(c.name) ? static_cast<std::string>(c) : c.name)
                << console::reset;
    }
  };

  /* ==== Character Table ======================================================
  *
  * Abstract
  *   For character literal #\<character> or #\<character name>.
  *
  * ========================================================================= */
  extern const std::unordered_map<std::string, object> characters;

  auto is_intraline_whitespace = [](auto c) constexpr
  {
    return c == u8' '
        or c == u8'\f'
        or c == u8'\t'
        or c == u8'\v';
  };

  auto is_eol = [](auto c) constexpr
  {
    return c == u8'\n'
        or c == u8'\r';
  };

  auto is_eof = [](auto c) constexpr
  {
    return c == std::char_traits<decltype(c)>::eof();
  };

  auto is_whitespace = [](auto c) constexpr
  {
    return is_intraline_whitespace(c)
        or is_eol(c)
        or is_eof(c);
  };

  auto is_parenthesis = [](auto c) constexpr
  {
    return c == u8'('
        or c == u8')';
  };

  auto is_quotation = [](auto c) constexpr
  {
    return c == u8'\''
        or c == u8'"'
        or c == u8'`';
  };

  auto is_vertical_line = [](auto c) constexpr
  {
    return c == u8'|';
  };

  auto is_discriminator = [](auto c) constexpr
  {
    return c == u8'#';
  };

  auto is_delimiter = [](auto c) constexpr
  {
    return is_whitespace(c)
        or is_parenthesis(c)
        or is_quotation(c)
        or is_discriminator(c)
        or is_vertical_line(c)
        or c == u8';'
        or c == u8',';
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
} // namespace meevax::kernel

#endif // INCLUDED_MEEVAX_KERNEL_CHARACTER_HPP

