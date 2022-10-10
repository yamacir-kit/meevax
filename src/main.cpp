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

auto main(int const argc, char const* const* const argv) -> int
{
  using namespace meevax;

  return with_exception_handler([&]()
  {
    library::boot();

    auto&& main = interaction_environment().as<environment>();

    main.configure(argc, argv);

    if (main.interactive)
    {
      main.display_version();
      main.import_("(scheme base)");
      main.import_("(scheme char)");
      main.import_("(scheme complex)");
      main.import_("(scheme cxr)");
      main.import_("(scheme eval)");
      main.import_("(scheme inexact)");
      main.import_("(scheme lazy)");
      main.import_("(scheme load)");
      main.import_("(scheme process-context)");
      main.import_("(scheme read)");
      main.import_("(scheme write)");
    }

    while (main.interactive and main.char_ready())
    {
      try
      {
        write(standard_output, u8"\u03bb> ");
        print(main.evaluate(main.read()));
      }
      catch (error const& error)
      {
        std::cerr << error << std::endl;
      }
    }

    return success;
  });
}
