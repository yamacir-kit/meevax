/*
   Copyright 2018-2023 Tatsuya Yamasaki.

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
#include <meevax/kernel/standard_input_port.hpp>

auto main(int const argc, char const* const* const argv) -> int
{
  using namespace meevax;
  using namespace meevax::literals;

  return with_exception_handler([&]()
  {
    boot();

    auto & main = interaction_environment().as<environment>();

    main.import("(scheme base)"_read);
    main.import("(scheme case-lambda)"_read);
    main.import("(scheme char)"_read);
    main.import("(scheme complex)"_read);
    main.import("(scheme cxr)"_read);
    main.import("(scheme eval)"_read);
    main.import("(scheme file)"_read);
    main.import("(scheme inexact)"_read);
    main.import("(scheme lazy)"_read);
    main.import("(scheme load)"_read);
    main.import("(scheme process-context)"_read);
    main.import("(scheme read)"_read);
    main.import("(scheme repl)"_read);
    main.import("(scheme time)"_read);
    main.import("(scheme write)"_read);

    main.configure(argc, argv);

    for (auto input = standard_input_port(); main.interactive and input.get_ready(); )
    {
      try
      {
        std::cout << u8"\u03bb> " << main.evaluate(input.read()) << std::endl;
      }
      catch (error const& error)
      {
        std::cerr << error << std::endl;
      }
    }

    return success;
  });
}
