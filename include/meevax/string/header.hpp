#ifndef INCLUDED_MEEVAX_STRING_HEADER_HPP
#define INCLUDED_MEEVAX_STRING_HEADER_HPP

#include <meevax/string/indent.hpp>

namespace meevax
{
inline namespace string
{
  auto header(std::string const&, std::size_t = 16) -> std::string;
} // namespace string
} // namespace meevax

#endif // INCLUDED_MEEVAX_STRING_HEADER_HPP
