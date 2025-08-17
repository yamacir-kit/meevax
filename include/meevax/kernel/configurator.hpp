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

#ifndef INCLUDED_MEEVAX_KERNEL_CONFIGURATOR_HPP
#define INCLUDED_MEEVAX_KERNEL_CONFIGURATOR_HPP

#include <filesystem>
#include <list>
#include <regex>

#include <meevax/kernel/pair.hpp>

namespace meevax::inline kernel
{
  struct configurator
  {
    struct option
    {
      std::regex const pattern;

      std::function<auto (std::function<auto () -> object> const&) -> void> evaluate;

      template <typename S, typename F>
      explicit option(S&& s, F&& f)
        : pattern  { std::forward<decltype(s)>(s) }
        , evaluate { std::forward<decltype(f)>(f) }
      {}
    };

    auto static inline interactive = false;

    auto static inline command_line = std::vector<std::string>();

    auto static inline directories = std::list<std::filesystem::path>();

    auto static configure(int const, char const* const* const) -> void;

    auto static configure(std::vector<std::string> const&) -> void;
  };
} // namespace meevax::kernel

#endif // INCLUDED_MEEVAX_KERNEL_CONFIGURATOR_HPP
