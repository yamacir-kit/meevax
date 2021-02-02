#include <meevax/string/header.hpp>

namespace meevax
{
inline namespace string
{
  auto header(std::string const& from, std::size_t size) -> std::string
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
