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

#include <meevax/kernel/boot.hpp>
#include <meevax/kernel/configuration.hpp>
#include <meevax/kernel/input_string_port.hpp>
#include <meevax/kernel/library.hpp>
#include <meevax/kernel/standard_input_port.hpp>

auto main(int const argc, char const* const* const argv) -> int
{
  using namespace meevax;
  using namespace meevax::literals;

  try
  {
    boot();

    interaction_environment().as<environment>().load_scheme_libraries();

    if (configure(argc, argv); interactive())
    {
      interaction_environment().as<environment>().evaluate(
        "(import (scheme base)"
                "(scheme bitwise)"
                "(scheme box)"
                "(scheme case-lambda)"
                "(scheme char)"
                "(scheme complex)"
                "(scheme cxr)"
                "(scheme division)"
                "(scheme eval)"
                "(scheme file)"
                "(scheme fixnum)"
                "(scheme flonum)"
                "(scheme inexact)"
                "(scheme lazy)"
                "(scheme list)"
                "(scheme load)"
                "(scheme process-context)"
                "(scheme read)"
                "(scheme repl)"
                "(scheme time)"
                "(scheme write))"_r);

      while (not standard_input_port().at_end_of_file())
      {
        try
        {
          std::cout << "> " << interaction_environment().as<environment>().evaluate(standard_input_port().read()) << std::endl;
        }
        catch (error const& error)
        {
          error.report(std::cerr);
        }
      }
    }

    return EXIT_SUCCESS;
  }
  catch (int const status) // NOTE: emergency-exit
  {
    return status;
  }
  catch (error const& error)
  {
    error.report(std::cerr);
    return EXIT_FAILURE;
  }
  catch (std::exception const& exception)
  {
    error(make<string>(exception.what()), unit).report(std::cerr);
    return EXIT_FAILURE;
  }
}
