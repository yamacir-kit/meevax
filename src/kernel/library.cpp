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

namespace meevax
{
inline namespace kernel
{
  std::map<std::string, library> libraries {};

  auto bootstrap() -> void
  {
    define_library("(scheme base)", [](library &)
    {
    });

    define_library("(scheme char)", [](library &)
    {
    });

    define_library("(meevax test)", [](library & library)
    {
      library.define<procedure>("test-print", [](let const& xs)
      {
        std::cout << "; Procedure test-print received " << xs << std::endl;
        return unit;
      });

      library.export_("test-print");
    });

    define_library("(meevax foo)", [](library & library)
    {
      library.export_("a");
      library.export_("b");
      library.export_("c");

      library.define<procedure>("a", [](let const&)
      {
        LINE();
        return unit;
      });

      library.define("b", make<exact_integer>(42));

      library.define("c", make<symbol>("dummy"));
    });
  }
} // namespace kernel
} // namespace meevax
