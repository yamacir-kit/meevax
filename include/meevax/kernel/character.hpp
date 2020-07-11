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
                #if __cpp_lib_nonmember_container_access
                << (std::empty(c.name) ? static_cast<std::string>(c) : c.name)
                #else
                << (c.name.empty() ? static_cast<std::string>(c) : c.name)
                #endif
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

  auto char_ci_eq = [](auto c, auto... xs)
    #if 201603 <= __cpp_constexpr
    constexpr
    #endif
  {
    return (std::char_traits<decltype(c)>::eq(c, xs) or ...);
  };

  auto is_intraline_whitespace = [](auto c)
    #if 201603 <= __cpp_constexpr
    constexpr
    #endif
  {
    #if 201703L <= __cplusplus
    return char_ci_eq(c, u8' ', u8'\f', u8'\t', u8'\v');
    #else
    return char_ci_eq(c, ' ', '\f', '\t', '\v');
    #endif
  };

  auto is_eol = [](auto c)
    #if 201603 <= __cpp_constexpr
    constexpr
    #endif
  {
    #if 201703L <= __cplusplus
    return char_ci_eq(c, u8'\n', u8'\r');
    #else
    return char_ci_eq(c, '\n', '\r');
    #endif
  };

  auto is_eof = [](auto c)
    #if 201603 <= __cpp_constexpr
    constexpr
    #endif
  {
    using traits = typename std::char_traits<decltype(c)>;

    return traits::eq_int_type(traits::to_int_type(c), traits::eof());
  };

  auto is_whitespace = [](auto c)
    #if 201603 <= __cpp_constexpr
    constexpr
    #endif
  {
    return is_intraline_whitespace(c)
        or is_eol(c)
        or is_eof(c);
  };

  auto is_parenthesis = [](auto c)
    #if 201603 <= __cpp_constexpr
    constexpr
    #endif
  {
    #if 201703L <= __cplusplus
    return char_ci_eq(c, u8'(', u8')');
    #else
    return char_ci_eq(c, '(', ')');
    #endif
  };

  auto is_quotation = [](auto c)
    #if 201603 <= __cpp_constexpr
    constexpr
    #endif
  {
    #if 201703L <= __cplusplus
    return char_ci_eq(c, u8'\'', u8'"', u8'`');
    #else
    return char_ci_eq(c, '\'', '"', '`');
    #endif
  };

  auto is_vertical_line = [](auto c)
    #if 201603 <= __cpp_constexpr
    constexpr
    #endif
  {
    #if 201703L <= __cplusplus
    return char_ci_eq(c, u8'|');
    #else
    return char_ci_eq(c, '|');
    #endif
  };

  auto is_discriminator = [](auto c)
    #if 201603 <= __cpp_constexpr
    constexpr
    #endif
  {
    #if 201703L <= __cplusplus
    return char_ci_eq(c, u8'#');
    #else
    return char_ci_eq(c, '#');
    #endif
  };

  auto is_delimiter = [](auto c)
    #if 201603 <= __cpp_constexpr
    constexpr
    #endif
  {
    return is_whitespace(c)
        or is_parenthesis(c)
        or is_quotation(c)
        or is_discriminator(c)
        or is_vertical_line(c)
    #if 201703L <= __cplusplus
        or char_ci_eq(c, u8';', u8',');
    #else
        or char_ci_eq(c, ';', ',');
    #endif
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
