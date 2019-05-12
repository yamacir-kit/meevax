#include <meevax/system/pair.hpp>

namespace meevax::system
{
  std::ostream& operator<<(std::ostream& os, const pair& p)
  {
    os << "\x1b[35m(\x1b[0m" << std::get<0>(p);

    for (auto e {std::get<1>(p)}; e; e = cdr(e))
    {
      if (e.is<pair>())
      {
        os << " " << car(e);
      }
      else // iter is the last element of dotted-list.
      {
        os << "\x1b[35m . \x1b[0m" << e;
      }
    }

    return os << "\x1b[35m)\x1b[0m";
  }
} // namespace meevax::system

