#ifndef INCLUDED_MEEVAX_STRING_UNICODE_HPP
#define INCLUDED_MEEVAX_STRING_UNICODE_HPP

#include <cstdint>
#include <string>

namespace meevax
{
inline namespace string
{
  using codepoint = std::uint_fast32_t;

  using codeunit = std::string;

  auto codepoint_to_codeunit(codepoint) -> codeunit;

  auto codeunit_to_codepoint(codeunit const&) -> codepoint;
} // namespace string
} // namespace meevax

#endif // INCLUDED_MEEVAX_STRING_UNICODE_HPP
