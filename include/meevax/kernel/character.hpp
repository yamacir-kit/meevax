#ifndef INCLUDED_MEEVAX_KERNEL_CHARACTER_HPP
#define INCLUDED_MEEVAX_KERNEL_CHARACTER_HPP

#include <cstdint>
#include <unordered_map>

#include <meevax/kernel/object.hpp>

namespace meevax { inline namespace kernel
{
  /* ---- Character --------------------------------------------------------- */

  auto encode(std::uint_least32_t code) -> std::string;

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

    auto decode() const
    {
      std::uint_least32_t code {};

      /* -----------------------------------------------------------------------
       *
       *  00000000 -- 0000007F: 0xxxxxxx
       *  00000080 -- 000007FF: 110xxxxx 10xxxxxx
       *  00000800 -- 0000FFFF: 1110xxxx 10xxxxxx 10xxxxxx
       *  00010000 -- 001FFFFF: 11110xxx 10xxxxxx 10xxxxxx 10xxxxxx
       *
       * -------------------------------------------------------------------- */

      switch (size())
      {
      case 1:
        code |= (*this)[0] & 0b0111'1111;
        break;

      case 2:
        code |= (*this)[0] & 0b0001'1111; code <<= 6;
        code |= (*this)[1] & 0b0011'1111;
        break;

      case 3:
        code |= (*this)[0] & 0b0000'1111; code <<= 6;
        code |= (*this)[1] & 0b0011'1111; code <<= 6;
        code |= (*this)[2] & 0b0011'1111;
        break;

      case 4:
        code |= (*this)[0] & 0b0000'0111; code <<= 6;
        code |= (*this)[1] & 0b0011'1111; code <<= 6;
        code |= (*this)[2] & 0b0011'1111; code <<= 6;
        code |= (*this)[3] & 0b0011'1111;
        break;

      default:
        throw error("Malformed character.");
      }

      return code;
    }

    decltype(auto) codepoint() const
    {
      return decode();
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
