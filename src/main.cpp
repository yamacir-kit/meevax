/*
   Copyright 2018-2022 Tatsuya Yamasaki.

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

#include <meevax/kernel/library.hpp>
#include <meevax/string/repeat.hpp>

auto main(int const argc, char const* const* const argv) -> int
{
  using namespace meevax;

  return with_exception_handler([&]()
  {
    library::boot();

    auto&& main = interaction_environment().as<environment>();

    main.configure(argc, argv);

    if (main.is_interactive_mode())
    {
      main.display_version();
      main.declare_import("(scheme r5rs)");
    }

    while (main.is_interactive_mode() and main.char_ready())
    {
      main.print(u8"\u250c", repeat(u8"\u2500", 79));
      main.write(standard_output, u8"\u2502\u03bb> ");
      main.print(main.evaluate(main.read()));
    }

    return underlying_cast(exit_status::success);
  });
}
