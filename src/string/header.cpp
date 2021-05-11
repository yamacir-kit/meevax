#include <meevax/string/header.hpp>

namespace meevax
{
  auto header(std::string const& from, std::size_t size) -> std::string
  {
    std::string s = "; ";

    if (not indent::depth)
    {
      s.append(from);
    }

    s.resize(size + 2, ' ');

    s.replace(s.size() - 3, 3, " ; ");

    return s;
  }
} // namespace meevax
