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

#ifndef INCLUDED_MEEVAX_KERNEL_LIBRARY_HPP
#define INCLUDED_MEEVAX_KERNEL_LIBRARY_HPP

#include <meevax/kernel/interaction_environment.hpp>

namespace meevax::inline kernel
{
  struct library
  {
    environment evaluator;

    let declarations = unit;

    let export_specs = unit;

    template <typename F>
    requires std::invocable<F, std::function<auto (object const&, object const&) -> object>>
    explicit library(F body)
      : export_specs { std::invoke(body, [this](auto&&... xs) { return evaluator.define(std::forward<decltype(xs)>(xs)...); }) }
    {}

    explicit library(object const&);

    friend auto boot() -> void;

    auto evaluate(object const&) -> object;

    auto import_set() -> object;
  };

  auto operator <<(std::ostream &, library const&) -> std::ostream &;

  auto libraries() -> std::map<std::string, object> &;
} // namespace meevax::kernel

#endif // INCLUDED_MEEVAX_KERNEL_LIBRARY_HPP
