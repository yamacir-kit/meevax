/*
   Copyright 2018-2024 Tatsuya Yamasaki.

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

#include <meevax/basis.hpp>
#include <meevax/kernel/boot.hpp>
#include <meevax/kernel/library.hpp>
#include <meevax/kernel/standard_input_port.hpp>

auto main(int const argc, char const* const* const argv) -> int
{
  using namespace meevax;
  using namespace meevax::literals;

  auto interact = [&](environment & e)
  {
    if (e.configure(argc, argv); e.interactive)
    {
      e.import("(scheme base)"_r);
      e.import("(scheme bitwise)"_r);
      e.import("(scheme box)"_r);
      e.import("(scheme case-lambda)"_r);
      e.import("(scheme char)"_r);
      e.import("(scheme complex)"_r);
      e.import("(scheme cxr)"_r);
      e.import("(scheme division)"_r);
      e.import("(scheme eval)"_r);
      e.import("(scheme file)"_r);
      e.import("(scheme fixnum)"_r);
      e.import("(scheme flonum)"_r);
      e.import("(scheme inexact)"_r);
      e.import("(scheme lazy)"_r);
      e.import("(scheme list)"_r);
      e.import("(scheme load)"_r);
      e.import("(scheme process-context)"_r);
      e.import("(scheme read)"_r);
      e.import("(scheme repl)"_r);
      e.import("(scheme time)"_r);
      e.import("(scheme write)"_r);

      while (not standard_input_port().at_end_of_file())
      {
        try
        {
          std::cout << "> " << e.evaluate(standard_input_port().read()) << std::endl;
        }
        catch (error const& error)
        {
          error.report(std::cerr);
        }
      }
    }
  };

  return with_exception_handler([&]()
  {
    boot();
    boot(basis());
    interact(interaction_environment().as<environment>());
  });
}
