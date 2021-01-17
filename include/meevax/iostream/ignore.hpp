#ifndef INCLUDED_MEEVAX_IOSTREAM_IGNORE_HPP
#define INCLUDED_MEEVAX_IOSTREAM_IGNORE_HPP

#include <iostream>

namespace meevax
{
inline namespace iostream
{
  // TODO UNICODE SUPPORT
  template <typename F>
  decltype(auto) ignore(std::istream & is, F&& f)
  {
    while (f(is.peek()))
    {
      is.ignore(1);
    }

    return is;
  }
} // namespace iostream
} // namespace meevax

#endif // INCLUDED_MEEVAX_IOSTREAM_IGNORE_HPP
