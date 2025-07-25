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

#ifndef INCLUDED_MEEVAX_IOSTREAM_IS_CONSOLE_HPP
#define INCLUDED_MEEVAX_IOSTREAM_IS_CONSOLE_HPP

#include <iostream>

#include <unistd.h>

namespace meevax::inline iostream
{
  inline auto is_console = [](std::ostream & os)
  {
    if (os.rdbuf() == std::cout.rdbuf())
    {
      static auto const result = static_cast<bool>(::isatty(STDOUT_FILENO));
      return result;
    }
    else if (os.rdbuf() == std::cerr.rdbuf())
    {
      static auto const result = static_cast<bool>(::isatty(STDERR_FILENO));
      return result;
    }
    else
    {
      return false;
    }
  };
} // namespace meevax::iostream

#endif // INCLUDED_MEEVAX_IOSTREAM_IS_CONSOLE_HPP
