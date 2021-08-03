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

#include <meevax/kernel/syntactic_continuation.hpp>

auto main(const int argc, meevax::const_pointer<meevax::const_pointer<const char>> argv) -> typename std::underlying_type<meevax::exit_status>::type
{
  using namespace meevax;

  return with_exception_handler([&]()
  {
    auto root = syntactic_continuation(boot_upto<layer::extensions>());

    root.configure(argc, argv);

    if (root.is_interactive_mode())
    {
      root.write_to(root.standard_interaction_port(), header(__func__), "You have control of root syntactic-continuation.\n");

      for (auto index = 0; root.ready(); ++index)
      {
        root.write_to(root.standard_interaction_port(), root.current_prompt());
        root.write_to(root.standard_interaction_port(), root.evaluate(root.read()), "\n");
      }

      root.write_to(root.standard_interaction_port(), header(__func__), "I have control of root syntactic-continuation.\n");
    }

    return underlying_cast(exit_status::success);
  });
}
