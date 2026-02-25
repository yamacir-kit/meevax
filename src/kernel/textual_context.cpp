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

#include <meevax/kernel/configurator.hpp>
#include <meevax/kernel/error.hpp>
#include <meevax/kernel/textual_context.hpp>

namespace meevax::inline kernel
{
  textual_context::proxy::proxy()
    : path { paths.end() }
  {}

  textual_context::proxy::proxy(std::filesystem::path const& path)
    : path { paths.find(path) }
  {}

  auto textual_context::proxy::source_file() const -> std::filesystem::path const&
  {
    return path != paths.end() ? *path : unknown_source_file;
  }

  auto textual_context::proxy::source_directory() const -> std::filesystem::path
  {
    return source_file().parent_path();
  }

  auto textual_context::proxy::resolve(std::filesystem::path const& given) const -> std::filesystem::path
  {
    if (std::filesystem::exists(given))
    {
      return std::filesystem::canonical(given);
    }
    else if (auto p = source_directory() / given; std::filesystem::exists(p))
    {
      return p;
    }
    else if (auto p = std::filesystem::current_path() / given; std::filesystem::exists(p))
    {
      return p;
    }
    else
    {
      for (auto const& directory : configurator::directories)
      {
        if (auto p = directory / given; std::filesystem::exists(p))
        {
          return p;
        }
      }

      throw error(make<string>("No such file"),
                  make<string>(given));
    }
  }

  auto textual_context::cons(object const& a, object const& b, std::filesystem::path const& path) -> object
  {
    let const x = meevax::cons(a, b);

    proxies[x.get()] = proxy(path);

    return x;
  }

  auto textual_context::of(object const& x) -> proxy
  {
    if (auto iterator = proxies.find(x.get()); iterator != proxies.end())
    {
      return iterator->second;
    }
    else
    {
      return proxy();
    }
  }
} // namespace meevax::kernel
