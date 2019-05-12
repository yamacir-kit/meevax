#include <meevax/system/accessor.hpp>
#include <meevax/system/pair.hpp>

namespace meevax::system
{
  template <typename T>
  std::ostream& operator<<(std::ostream& os, const accessor<T>& rhs)
  {
    // write(os) will be dispatched to each type's stream output operator.
    return !rhs ? (os << "\x1b[35m()\x1b[0m") : rhs.access().write(os);
  }

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

