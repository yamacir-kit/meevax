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

#include <meevax/kernel/describe.hpp>
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
      main.declare<import_set>("(scheme base)");
      main.declare<import_set>("(scheme char)");
      main.declare<import_set>("(scheme complex)");
      main.declare<import_set>("(scheme cxr)");
      main.declare<import_set>("(scheme eval)");
      main.declare<import_set>("(scheme inexact)");
      main.declare<import_set>("(scheme lazy)");
      main.declare<import_set>("(scheme load)");
      main.declare<import_set>("(scheme process-context)");
      main.declare<import_set>("(scheme read)");
      main.declare<import_set>("(scheme repl)");
      main.declare<import_set>("(scheme write)");

      main.define<procedure>("describe", [&](let const& xs)
      {
        describe(xs[0], xs[1]);
        return unspecified;
      });
    }

    while (main.interactive and main.get_ready())
    {
      try
      {
        std::cout << u8"\u03bb> " << main.evaluate(main.read()) << std::endl;
      }
      catch (error const& error)
      {
        std::cerr << error << std::endl;
      }
    }

    return success;
  });
}
