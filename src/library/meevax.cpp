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

#include <meevax/iostream/lexical_cast.hpp>
#include <meevax/kernel/basis.hpp>
#include <meevax/kernel/environment.hpp>

namespace meevax
{
  environment::environment(master_t)
  {
    import("(meevax character)");
    import("(meevax context)");
    import("(meevax control)");
    import("(meevax evaluate)");
    import("(meevax exception)");
    import("(meevax experimental)");
    import("(meevax inexact)");
    import("(meevax macro)");
    import("(meevax number)");
    import("(meevax port)");
    import("(meevax read)");
    import("(meevax syntax)"); // quote-syntax
    import("(meevax write)");

    define<procedure>("features", [](auto&&...)
    {
      return features();
    });

    define<procedure>("load", [this](let const& xs)
    {
      return load(car(xs).as<string>());
    });

    std::vector<string_view> const codes {
      srfi_211,
      r4rs,
      overture,
      srfi_8,
      srfi_1,
      srfi_23,
      srfi_34,
      srfi_39,
      srfi_45,
      srfi_78,
      srfi_149,
      r7rs,
    };

    for (auto const& code : codes)
    {
      // NOTE: Since read performs a putback operation on a given stream, it must be copied and used.
      auto port = std::stringstream(std::string(code));

      for (let e = read(port); e != eof_object; e = read(port))
      {
        evaluate(e);
      }
    }
  }
} // namespace meevax
