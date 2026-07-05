/*
   Copyright 2018-2025 Tatsuya Yamasaki.

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

#include <meevax/iostream/escape_sequence.hpp>
#include <meevax/kernel/configuration.hpp>
#include <meevax/kernel/ghost.hpp>
#include <unistd.h> // isatty

namespace meevax::inline iostream
{
  auto static const index = std::ios_base::xalloc();

  auto operator <<(std::ostream & os, color const& c) -> std::ostream &
  {
    os.iword(index) = c.value;
    return os;
  }

  auto colorable(std::ostream & os) -> bool
  {
    switch (os.iword(index))
    {
    default:
    case color::unspecified:
      return (os.rdbuf() == std::cout.rdbuf() and isatty(STDOUT_FILENO)) or
             (os.rdbuf() == std::cerr.rdbuf() and isatty(STDERR_FILENO));

    case color::disabled:
      return false;

    case color::enabled:
      return true;
    };
  }
} // namespace meevax::iostream
