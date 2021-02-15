#include <meevax/kernel/pair.hpp>

namespace meevax
{
inline namespace kernel
{
  auto operator <<(std::ostream& port, const pair& pare) -> decltype(port)
  {
    port << magenta << "(" << reset << car(pare);

    for (let rest = cdr(pare); rest; rest = cdr(rest))
    {
      if (rest.is<pair>())
      {
        port << " " << car(rest);
      }
      else // iter is the last element of dotted-list.
      {
        port << magenta << " . " << reset << rest;
        break;
      }
    }

    return port << magenta << ")" << reset;
  }
} // namespace kernel
} // namespace meevax
