#ifndef INCLUDED_MEEVAX_POSIX_IS_TTY_HPP
#define INCLUDED_MEEVAX_POSIX_IS_TTY_HPP

#include <iostream>

#include <unistd.h>

namespace meevax { inline namespace posix
{
  auto is_tty = [](std::ostream& os)
  {
    if (os.rdbuf() == std::cout.rdbuf())
    {
      static const auto result { static_cast<bool>(::isatty(STDOUT_FILENO)) };
      return result;
    }
    else if (os.rdbuf() == std::cerr.rdbuf())
    {
      static const auto result { static_cast<bool>(::isatty(STDERR_FILENO)) };
      return result;
    }
    else
    {
      return false;
    }
  };
}} // namespace meevax::posix

#endif // INCLUDED_MEEVAX_POSIX_IS_TTY_HPP
