#ifndef INCLUDED_MEEVAX_KERNEL_CHARACTER_HPP
#define INCLUDED_MEEVAX_KERNEL_CHARACTER_HPP

#include <unordered_map>

#include <meevax/kernel/pair.hpp>

namespace meevax::kernel
{
  struct character
    : public std::string // TODO convert std::u8string in future.
  {
    const std::string external_representation;

    explicit character(const char ascii)
      : std::string {ascii}
    {}

    explicit character(
      const std::string& unicode,
      const std::string& external_representation = {})
      : std::string {unicode}
      , external_representation {external_representation}
    {}

    friend auto operator<<(std::ostream& os, const character& c)
      -> decltype(os)
    {
      return os << posix::highlight::datum << "#\\"
                << (std::empty(c.external_representation)
                      ? static_cast<std::string>(c)
                      : c.external_representation)
                << posix::attribute::normal;
    }
  };

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

