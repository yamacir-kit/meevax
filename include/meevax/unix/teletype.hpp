#ifndef INCLUDED_MEEVAX_UNIX_TELETYPE_HPP
#define INCLUDED_MEEVAX_UNIX_TELETYPE_HPP

#include <iostream>

#include <unistd.h>

namespace meevax::unix
{
  auto is_tty(std::ostream& os)
  {
    if (os.rdbuf() == std::cout.rdbuf())
    {
      static const bool result {::isatty(STDOUT_FILENO)};
      return result;
    }
    else if (os.rdbuf() == std::cerr.rdbuf())
    {
      static const bool result {::isatty(STDERR_FILENO)};
      return result;
    }
    else
    {
      return false;
    }
  }
} // namespace meevax::unix

#endif // INCLUDED_MEEVAX_UNIX_TELETYPE_HPP

