#ifndef INCLUDED_MEEVAX_STRING_UNICODE_HPP
#define INCLUDED_MEEVAX_STRING_UNICODE_HPP

#include <cstdint>
#include <string>

namespace meevax
{
  using codepoint = std::uint32_t;

  using codeunit = std::string;

  auto codepoint_to_codeunit(codepoint) -> codeunit;

  auto codeunit_to_codepoint(codeunit const&) -> codepoint;
} // namespace meevax

#endif // INCLUDED_MEEVAX_STRING_UNICODE_HPP
