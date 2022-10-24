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

#if __unix__
#include <dlfcn.h> // dlopen, dlclose, dlerror
#else
#error
#endif

#include <meevax/kernel/error.hpp>
#include <meevax/kernel/procedure.hpp>
#include <meevax/kernel/string.hpp>

namespace meevax
{
inline namespace kernel
{
  procedure::procedure(std::string const& name, function_type const& call)
    : name { name }
    , call { call }
  {}

  procedure::procedure(std::string const& name, std::string const& libfoo_so)
    : name { name }
    , call { dlsym(name, dlopen(libfoo_so)) }
  {}

  auto procedure::dlopen(std::string const& libfoo_so) -> void *
  {
    auto dlclose = [](void * const handle)
    {
      if (handle and ::dlclose(handle))
      {
        std::cerr << ::dlerror() << std::endl;
      }
    };

    static std::unordered_map<
      std::string, std::unique_ptr<void, decltype(dlclose)>
    > dynamic_libraries {};

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

  auto procedure::dlsym(std::string const& name, void * const handle) -> function_pointer_type
  {
    if (auto address = ::dlsym(handle, name.c_str()); address)
    {
      return reinterpret_cast<function_pointer_type>(address);
    }
    else
    {
      throw file_error(make<string>(::dlerror()));
    }
  }

  auto operator <<(std::ostream & os, procedure const& datum) -> std::ostream &
  {
    return os << magenta("#,(") << green("procedure") << " " << datum.name << magenta(")");
  }
} // namespace kernel
} // namespace meevax
