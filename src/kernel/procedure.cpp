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

#include <meevax/kernel/procedure.hpp>
#include <meevax/posix/vt10x.hpp>

namespace meevax
{
inline namespace kernel
{
  procedure::procedure(std::string const& name, std::function<PROCEDURE()> const& function)
    : std::function<PROCEDURE()> { function }
    , name { name }
  {}

  procedure::procedure(std::string const& name, std::string const& libfoo_so)
    : std::function<PROCEDURE()> { load(name, open(libfoo_so)) }
    , name { name }
  {}

  auto procedure::load(std::string const& symbol_name, pointer<void> const& handle) -> signature
  {
    if (pointer<void> const address = dlsym(handle, symbol_name.c_str()); address)
    {
      return reinterpret_cast<signature>(address);
    }
    else
    {
      throw file_error(make<string>(dlerror()), unit);
    }
  }

  auto procedure::open(std::string const& libfoo_so) -> pointer<void>
  {
    auto close_dynamic_library = [](pointer<void> const handle)
    {
      if (handle and ::dlclose(handle))
      {
        std::cerr << dlerror() << std::endl;
      }
    };

    using library_handle = std::unique_ptr<void, decltype(close_dynamic_library)>;

    static std::unordered_map<std::string, library_handle> dynamic_libraries {};

    dlerror(); // clear

    try
    {
      return dynamic_libraries.at(libfoo_so).get();
    }
    catch (std::out_of_range const&)
    {
      if (pointer<void> handle = dlopen(libfoo_so.c_str(), RTLD_LAZY | RTLD_GLOBAL); handle)
      {
        dynamic_libraries.emplace(libfoo_so, library_handle(handle, close_dynamic_library));

        return open(libfoo_so);
      }
      else
      {
        throw file_error(make<string>(dlerror()), unit);
      }
    }
  }

  auto operator <<(std::ostream & os, procedure const& datum) -> std::ostream &
  {
    return os << magenta << "#,(" << green << "procedure " << reset << datum.name << magenta << ")" << reset;
  }
} // namespace kernel
} // namespace meevax
