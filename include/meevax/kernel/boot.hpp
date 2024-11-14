/*
   Copyright 2018-2024 Tatsuya Yamasaki.

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

#ifndef INCLUDED_MEEVAX_KERNEL_BOOT_HPP
#define INCLUDED_MEEVAX_KERNEL_BOOT_HPP

#include <meevax/kernel/environment.hpp>
#include <meevax/kernel/interaction_environment.hpp>

namespace meevax::inline kernel
{
  auto boot() -> void;

  template <typename Sources>
  auto boot(Sources const& sources) -> void
  {
    for (auto&& source : sources)
    {
      for (let const& x : input_string_port(source))
      {
        interaction_environment().as<environment>().evaluate(x);
      }
    }
  }
} // namespace meevax::kernel

#endif // INCLUDED_MEEVAX_KERNEL_BOOT_HPP
