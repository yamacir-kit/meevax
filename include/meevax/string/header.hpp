#ifndef INCLUDED_MEEVAX_STRING_HEADER_HPP
#define INCLUDED_MEEVAX_STRING_HEADER_HPP

#include <meevax/string/indent.hpp>

namespace meevax
{
inline namespace string
{
  auto header(std::string const& from, std::size_t size = 16) -> std::string
  {
    std::string s = "; ";

    if (not indent::depth)
    {
      s.append(from);
    }

    s.resize(size, ' ');

    s.replace(s.size() - 3, 3, " ; ");

    return s;
  }
} // namespace string
} // namespace meevax

#endif // INCLUDED_MEEVAX_STRING_HEADER_HPP
