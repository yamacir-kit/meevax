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

#ifndef INCLUDED_MEEVAX_KERNEL_PROCEDURE_HPP
#define INCLUDED_MEEVAX_KERNEL_PROCEDURE_HPP

#if __unix__
#include <dlfcn.h> // dlopen, dlclose, dlerror
#else
#error
#endif

#include <meevax/kernel/error.hpp>
#include <meevax/kernel/list.hpp>

namespace meevax
{
inline namespace kernel
{
  #define PROCEDURE(...) meevax::let __VA_ARGS__(meevax::let const& xs)

  auto close_dynamic_library = [](pointer<void> const handle)
  {
    if (handle and ::dlclose(handle))
    {
      std::cerr << dlerror() << std::endl;
    }
  };

  using library_handle = std::unique_ptr<void, decltype(close_dynamic_library)>;

  auto from(std::string const& library_name) -> library_handle const& // NOTE: library_name = "lib*.so"
  {
    static std::unordered_map<std::string, library_handle> dynamic_libraries {};

    dlerror(); // clear

    try
    {
      return dynamic_libraries.at(library_name);
    }
    catch (std::out_of_range const&)
    {
      if (auto handle = library_handle(dlopen(library_name.c_str(), RTLD_LAZY | RTLD_GLOBAL), close_dynamic_library); handle)
      {
        dynamic_libraries.emplace(library_name, std::move(handle));

        return from(library_name);
      }
      else
      {
        throw file_error(make<string>(dlerror()), unit);
      }
    }
  }

  struct procedure : public std::function<PROCEDURE()>
  {
    using signature = PROCEDURE((*));

    std::string const name;

    explicit procedure(std::string const& name, std::function<PROCEDURE()> const& function)
      : std::function<PROCEDURE()> { function  }
      , name { name }
    {}

    explicit procedure(std::string const& function_name, library_handle const& handle)
      : std::function<PROCEDURE()> { link_as<signature>(function_name, handle) }
      , name { function_name }
    {}

    virtual ~procedure() = default;

    template <typename T, REQUIRES(std::is_pointer<T>)>
    static auto link_as(std::string const& symbol_name, library_handle const& handle) -> T
    {
      if (pointer<void> const address = dlsym(handle.get(), symbol_name.c_str()); address)
      {
        return reinterpret_cast<T>(address);
      }
      else
      {
        throw file_error(make<string>(dlerror()), unit);
      }
    }
  };

  auto operator <<(output_port & port, procedure const& datum) -> output_port &;

  template <typename T>
  struct is
  {
    let const& operator ()(let const& xs) const
    {
      auto is_T = [](let const& x)
      {
        return x.is<T>();
      };

      return std::all_of(std::begin(xs), std::end(xs), is_T) ? t : f;
    }
  };
} // namespace kernel
} // namespace meevax

#endif // INCLUDED_MEEVAX_KERNEL_PROCEDURE_HPP
