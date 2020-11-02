#ifndef INCLUDED_MEEVAX_KERNEL_CHARACTER_HPP
#define INCLUDED_MEEVAX_KERNEL_CHARACTER_HPP

#include <cstdint>
#include <unordered_map>

#include <meevax/kernel/object.hpp>

namespace meevax { inline namespace kernel
{
  /* ---- Character --------------------------------------------------------- */

  struct character
    : public std::string
  {
    const std::string name;

    static auto encode(std::uint_least32_t code)
    {
      char sequence[5] = {};

      if (code <= 0x7F)
      {
        sequence[1] = '\0';
        sequence[0] = (code & 0x7F);
      }
      else if (code <= 0x7FF)
      {
        sequence[2] = '\0';
        sequence[1] = 0x80 | (code & 0x3F); code >>= 6;
        sequence[0] = 0xC0 | (code & 0x1F);
      }
      else if (code <= 0xFFFF)
      {
        sequence[3] = '\0';
        sequence[2] = 0x80 | (code & 0x3F); code >>= 6;
        sequence[1] = 0x80 | (code & 0x3F); code >>= 6;
        sequence[0] = 0xE0 | (code & 0x0F);
      }
      else if (code <= 0x10FFFF)
      {
        sequence[4] = '\0';
        sequence[3] = 0x80 | (code & 0x3F); code >>= 6;
        sequence[2] = 0x80 | (code & 0x3F); code >>= 6;
        sequence[1] = 0x80 | (code & 0x3F); code >>= 6;
        sequence[0] = 0xF0 | (code & 0x07);
      }
      else
      {
        sequence[3] = '\0';
        sequence[2] = 0xEF;
        sequence[1] = 0xBF;
        sequence[0] = 0xBD;
      }

      return std::string(sequence);
    }

    auto decode() const
    {
      std::uint_least32_t code {};

      /* ----
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
        code = (*this)[0] & 0b0111'1111;
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

    explicit character(std::uint32_t code)
      : std::string { encode(code) }
    {}

    explicit character(const std::string& code, const std::string& name = {})
      : std::string { code }
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
