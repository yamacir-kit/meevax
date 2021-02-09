#ifndef INCLUDED_MEEVAX_PARSER_CLASS_HPP
#define INCLUDED_MEEVAX_PARSER_CLASS_HPP

#include <type_traits>

#include <meevax/string/unicode.hpp>

namespace meevax
{
  auto is_eof = [](auto c) constexpr
  {
    using character = typename std::char_traits<decltype(c)>;

    return character::eq_int_type(character::to_int_type(c), character::eof());
  };

  auto is_upper = [](codeunit const& c)
  {
    return 'A' <= c[0] and c[0] <= 'Z';
  };

  auto is_lower = [](codeunit const& c)
  {
    return 'a' <= c[0] and c[0] <= 'z';
  };

  auto is_letter = [](codeunit const& c)
  {
    return is_upper(c) or is_lower(c);
  };
} // namespace meevax

#endif // INCLUDED_MEEVAX_PARSER_CLASS_HPP
