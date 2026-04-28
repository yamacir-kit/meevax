/*
   Copyright 2018-2025 Tatsuya Yamasaki.

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

#include <dlfcn.h> // dlopen, dlclose, dlerror
#include <meevax/kernel/procedure.hpp>
#include <meevax/kernel/system.hpp>

namespace meevax::inline kernel
{
  auto dlopen(std::string const& pathname)
  {
    auto dlclose = [](void * const handle) -> void
    {
      if (handle and ::dlclose(handle))
      {
        std::cerr << ::dlerror() << std::endl;
      }
    };

    auto static libraries = std::unordered_map<std::string, std::unique_ptr<void, decltype(dlclose)>>();

    if (auto found = libraries.find(pathname); found != libraries.end())
    {
      return found->second.get();
    }
    else
    {
      ::dlerror(); // clear

      if (auto handle = ::dlopen(pathname.c_str(), RTLD_LAZY | RTLD_GLOBAL); handle)
      {
        auto [emplaced, success] = libraries.emplace(std::piecewise_construct,
                                                     std::forward_as_tuple(pathname),
                                                     std::forward_as_tuple(handle, dlclose));
        return emplaced->second.get();
      }
      else
      {
        throw std::runtime_error(::dlerror());
      }
    }
  }

  template <typename T>
  auto dlsym(auto const& handle, auto const& symbol)
  {
    ::dlerror(); // clear

    if (auto address = ::dlsym(handle, symbol); address)
    {
      return reinterpret_cast<T>(address);
    }
    else
    {
      throw std::runtime_error(::dlerror());
    }
  }

  auto stub(auto const& shared_library_name)
  {
    auto const name = shared_library_prefix() + shared_library_name + shared_library_suffix();

    auto static stubs = std::unordered_map<std::string, decltype(&lookup)>
    {
      { shared_library_prefix() + "meevax" + shared_library_suffix(), lookup }
    };

    if (auto found = stubs.find(name); found != stubs.end())
    {
      return found->second;
    }
    else if (auto [emplaced, success] = stubs.emplace(name, dlsym<decltype(&lookup)>(dlopen(name), "lookup")); success)
    {
      return emplaced->second;
    }
    else
    {
      throw error(make<string>("failed to load shared-library"), make<string>(name));
    }
  }

  procedure::procedure(std::string const& shared_library_name, std::string const& symbol_name)
    : shared_library_name { shared_library_name }
    , name { symbol_name }
    , call { reinterpret_cast<procedure::signature>(stub(shared_library_name)(symbol_name.c_str())) }
  {}

  auto operator <<(std::ostream & os, procedure const& datum) -> std::ostream &
  {
    return os << magenta("#,(") << green("procedure ") << datum.name << magenta(")");
  }
} // namespace meevax::kernel
