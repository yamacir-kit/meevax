#ifndef INCLUDED_MEEVAX_STRING_UNICODE_HPP
#define INCLUDED_MEEVAX_STRING_UNICODE_HPP

#include <array>
#include <cstdint>
#include <string>

namespace meevax
{
inline namespace string
{
  using codepoint = std::uint_fast32_t;

  using codeunit = std::string;

  auto codepoint_to_codeunit(codepoint cp) -> codeunit;
} // namespace string
} // namespace meevax

#endif // INCLUDED_MEEVAX_STRING_UNICODE_HPP
