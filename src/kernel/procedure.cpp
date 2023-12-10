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

#if __unix__
#include <dlfcn.h> // dlopen, dlclose, dlerror
#else
#error
#endif

#include <memory>

#include <meevax/kernel/error.hpp>
#include <meevax/kernel/procedure.hpp>
#include <meevax/kernel/string.hpp>
#include <meevax/kernel/symbol.hpp>

namespace meevax
{
inline namespace kernel
{
  auto operator <<(std::ostream & os, callable const& datum) -> std::ostream &
  {
    return os << magenta("#,(") << green("procedure") << " " << symbol(datum.name) << magenta(")");
  }

  auto dlsym(std::string const& symbol, void * const handle) -> procedure_pointer
  {
    if (auto address = ::dlsym(handle, symbol.c_str()); address)
    {
      return reinterpret_cast<procedure_pointer>(address);
    }
    else
    {
      throw file_error(make<string>(::dlerror()));
    }
  }
} // namespace kernel
} // namespace meevax
