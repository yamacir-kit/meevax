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

  auto dlopen(std::string const& libfoo_so) -> void *
  {
    auto dlclose = [](void * const handle)
    {
      if (handle and ::dlclose(handle))
      {
        std::cerr << ::dlerror() << std::endl;
      }
    };

    static std::unordered_map<std::string, std::unique_ptr<void, decltype(dlclose)>> dynamic_libraries {};

    ::dlerror(); // clear

    try
    {
      return dynamic_libraries.at(libfoo_so).get();
    }
    catch (std::out_of_range const&)
    {
      if (auto handle = ::dlopen(libfoo_so.c_str(), RTLD_LAZY | RTLD_GLOBAL); handle)
      {
        dynamic_libraries.emplace(
          std::piecewise_construct,
          std::forward_as_tuple(libfoo_so),
          std::forward_as_tuple(handle, dlclose));

        return dlopen(libfoo_so);
      }
      else
      {
        throw file_error(make<string>(::dlerror()));
      }
    }
  }

  auto dlsym(std::string const& name, void * const handle) -> FUNCTION((*))
  {
    if (auto address = ::dlsym(handle, name.c_str()); address)
    {
      return reinterpret_cast<FUNCTION((*))>(address);
    }
    else
    {
      throw file_error(make<string>(::dlerror()));
    }
  }

  procedure::procedure(std::string const& filename, std::string const& symbol)
    : procedure { name, dlsym(symbol, dlopen(filename)) }
  {}
} // namespace kernel
} // namespace meevax
