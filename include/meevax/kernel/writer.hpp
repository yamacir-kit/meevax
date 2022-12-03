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

#ifndef INCLUDED_MEEVAX_KERNEL_WRITER_HPP
#define INCLUDED_MEEVAX_KERNEL_WRITER_HPP

#include <meevax/kernel/port.hpp>

namespace meevax
{
inline namespace kernel
{
  auto write_simple(std::ostream &, pair const&) -> std::ostream &;

  auto write_simple(std::ostream &, object const&) -> std::ostream &;

  template <typename... Ts>
  auto write_simple(object const& x, Ts&&... xs) -> decltype(auto)
  {
    return write_simple(x.as<std::ostream>(), std::forward<decltype(xs)>(xs)...);
  }

  template <typename... Ts>
  auto write(std::ostream & os, Ts&&... xs) -> std::ostream &
  {
    return (os << ... << xs);
  }

  template <typename... Ts>
  auto write(object const& x, Ts&&... xs) -> decltype(auto)
  {
    return write(x.as<std::ostream>(), std::forward<decltype(xs)>(xs)...);
  }
} // namespace kernel
} // namespace meevax

#endif // INCLUDED_MEEVAX_KERNEL_WRITER_HPP
