/*
   Copyright 2018-2021 Tatsuya Yamasaki.

   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at

       http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License.
*/

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

