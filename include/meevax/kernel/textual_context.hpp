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

#ifndef INCLUDED_MEEVAX_KERNEL_TEXTUAL_CONTEXT_HPP
#define INCLUDED_MEEVAX_KERNEL_TEXTUAL_CONTEXT_HPP

#include <filesystem>
#include <set>

#include <meevax/kernel/pair.hpp>

namespace meevax::inline kernel
{
  struct textual_context
  {
    auto static inline const unknown_source_file = std::filesystem::path();

    struct proxy
    {
      std::set<std::filesystem::path>::const_iterator path;

      explicit proxy();

      explicit proxy(std::filesystem::path const&);

      auto source_file() const -> std::filesystem::path const&;

      auto source_directory() const -> std::filesystem::path;

      auto resolve(std::filesystem::path const&) const -> std::filesystem::path;
    };

    static inline std::set<std::filesystem::path> paths {};

    static inline std::unordered_map<pair const*, proxy> proxies {};

    auto static cons(object const&, object const&, std::filesystem::path const&) -> object;

    auto static of(object const&) -> proxy;
  };
} // namespace meevax::kernel

#endif // INCLUDED_MEEVAX_KERNEL_TEXTUAL_CONTEXT_HPP
