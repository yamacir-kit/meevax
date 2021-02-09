#ifndef INCLUDED_MEEVAX_STRING_UNICODE_HPP
#define INCLUDED_MEEVAX_STRING_UNICODE_HPP

#include <cstdint>
#include <string>
#include <vector>

namespace meevax
{
  using codepoint = std::char_traits<char>::int_type;

  static_assert(4 <= sizeof(codepoint)); // At least 32-bit required.

  using codepoints = std::vector<codepoint>;

  using codeunit = std::string;

  using codeunits = std::string;

  auto codepoint_to_codeunit(codepoint) -> codeunit;

  auto codeunit_to_codepoint(codeunit const&) -> codepoint;
} // namespace meevax

#endif // INCLUDED_MEEVAX_STRING_UNICODE_HPP
