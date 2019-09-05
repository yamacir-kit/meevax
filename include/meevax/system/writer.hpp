#ifndef INCLUDED_MEEVAX_SYSTEM_WRITER_HPP
#define INCLUDED_MEEVAX_SYSTEM_WRITER_HPP

#include <iostream>

namespace meevax::system
{
  template <typename OutputStream, typename... Objects>
  decltype(auto) write(OutputStream&& os, Objects&&... objects)
  {
    (os << ... << objects);
    return os;
  }

  namespace color
  {
    inline namespace foreground
    {
      static constexpr auto black   {"\x1b[30m"},
                            red     {"\x1b[31m"},
                            green   {"\x1b[32m"},
                            yellow  {"\x1b[33m"},
                            blue    {"\x1b[34m"},
                            magenta {"\x1b[35m"},
                            cyan    {"\x1b[36m"},
                            white   {"\x1b[37m"};
    }

    namespace background
    {
      static constexpr auto black   {"\x1b[40m"},
                            red     {"\x1b[41m"},
                            green   {"\x1b[42m"},
                            yellow  {"\x1b[43m"},
                            blue    {"\x1b[44m"},
                            magenta {"\x1b[45m"},
                            cyan    {"\x1b[46m"},
                            white   {"\x1b[47m"};
    }
  }
} // namespace meevax::system

#endif // INCLUDED_MEEVAX_SYSTEM_WRITER_HPP

