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

#ifndef INCLUDED_MEEVAX_KERNEL_INCLUDE_HPP
#define INCLUDED_MEEVAX_KERNEL_INCLUDE_HPP

#include <meevax/kernel/eof.hpp>
#include <meevax/kernel/input_file_port.hpp>
#include <meevax/kernel/list.hpp>

namespace meevax::inline kernel
{
  struct case_insensitive
  {};

  template <typename T = void>
  auto include(object const& names, object const& xs = nullptr) -> object
  {
    if (names.is<pair>())
    {
      auto port = input_file_port(textual_context::of(names).locate(car(names).as<string>().utf8(), is_existing_non_directory));

      port.fold_case = std::is_same_v<T, case_insensitive>;

      return include<T>(port, names, xs);
    }
    else
    {
      return reverse(xs);
    }
  }

  template <typename T>
  auto include(input_file_port & port, object const& names, object const& xs) -> object
  {
    if (let const& x = port.read(); not x.is<eof>())
    {
      return include<T>(port, names, cons(x, xs));
    }
    else
    {
      return include<T>(cdr(names), xs);
    }
  }
} // namespace meevax::kernel

#endif // INCLUDED_MEEVAX_KERNEL_INCLUDE_HPP
