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

#include <meevax/standard.hpp>

auto main(int const argc, char const* const* const argv) -> int
{
  using namespace meevax;

  return with_exception_handler([&]()
  {
    auto main = environment(standard::base,
                            standard::character,
                            standard::evaluate,
                            standard::inexact,
                            standard::load,
                            standard::process_context,
                            standard::read,
                            standard::write,
                            standard::experimental,
                            standard::srfis);

    main.configure(argc, argv);

    while (main.is_interactive_mode() and main.char_ready())
    {
      main.write(main.standard_interaction_port(), main.current_prompt());
      main.write(main.standard_interaction_port(), main.evaluate(main.read()), "\n");
    }

    return underlying_cast(exit_status::success);
  });
}
