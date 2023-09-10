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

#include <meevax/kernel/boot.hpp>
#include <meevax/kernel/library.hpp>
#include <meevax/kernel/standard_input_port.hpp>

auto main(int const argc, char const* const* const argv) -> int
{
  using namespace meevax;
  using namespace meevax::literals;

  return with_exception_handler([&]()
  {
    auto interact = [&](auto & environment)
    {
      if (environment.configure(argc, argv); environment.interactive)
      {
        environment.import("(scheme base)"_read);
        environment.import("(scheme case-lambda)"_read);
        environment.import("(scheme char)"_read);
        environment.import("(scheme complex)"_read);
        environment.import("(scheme cxr)"_read);
        environment.import("(scheme eval)"_read);
        environment.import("(scheme file)"_read);
        environment.import("(scheme inexact)"_read);
        environment.import("(scheme lazy)"_read);
        environment.import("(scheme load)"_read);
        environment.import("(scheme process-context)"_read);
        environment.import("(scheme read)"_read);
        environment.import("(scheme repl)"_read);
        environment.import("(scheme time)"_read);
        environment.import("(scheme write)"_read);

        while (standard_input_port().good())
        {
          try
          {
            std::cout << u8"\u03bb> " << environment.evaluate(standard_input_port().read()) << std::endl;
          }
          catch (error const& error)
          {
            std::cerr << error << std::endl;
          }
        }
      }
    };

    boot();

    interact(interaction_environment().as<environment>());
  });
}
