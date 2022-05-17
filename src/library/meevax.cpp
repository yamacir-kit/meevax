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
    std::vector<string_view> const codes {
      srfi_211,
      r4rs_essential,
      srfi_45,
      r4rs,
      srfi_149,
      r5rs,
      srfi_6,  // Basic String Ports
      srfi_34, // Exception Handling for Programs
      srfi_23, // Error reporting mechanism
      srfi_39, // Parameter objects
      r7rs,
      srfi_8,  // receive: Binding to multiple values
      srfi_1,  // List Library
      srfi_78, // Lightweight testing
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
