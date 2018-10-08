#ifndef INCLUDED_MEEVAX_POSIX_NONCANONICAL_HPP
#define INCLUDED_MEEVAX_POSIX_NONCANONICAL_HPP

#include <cerrno>
#include <system_error>

#include <sys/ioctl.h>
#include <termios.h>
#include <unistd.h>

namespace meevax::posix
{
  class noncanonical
    : public ::termios
  {
    const ::termios canonical;

  public:
    noncanonical()
      : canonical {(::tcgetattr(STDIN_FILENO, this), *this)}
    {
      c_lflag &= ~(ICANON | ECHO);
      c_cc[VMIN] = 1;
      c_cc[VTIME] = 0;

      if (::tcsetattr(STDIN_FILENO, TCSANOW, this) < 0)
      {
        throw std::system_error {errno, std::system_category()};
      }
    }

    virtual ~noncanonical() noexcept
    {
      ::tcsetattr(STDIN_FILENO, TCSANOW, &canonical);
    }
  } static noncanonical {};
} // namespace meevax::posix

#endif // INCLUDED_MEEVAX_POSIX_NONCANONICAL_HPP

