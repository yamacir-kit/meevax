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

#ifndef INCLUDED_MEEVAX_KERNEL_WRITER_HPP
#define INCLUDED_MEEVAX_KERNEL_WRITER_HPP

#include <meevax/kernel/port.hpp>

namespace meevax
{
inline namespace kernel
{
  template <typename EnvironmentSpecifier>
  class writer
  {
    friend EnvironmentSpecifier;

    explicit writer()
    {}

    IMPORT(EnvironmentSpecifier, is_batch_mode,   const);
    IMPORT(EnvironmentSpecifier, is_debug_mode,   const);
    IMPORT(EnvironmentSpecifier, is_verbose_mode, const);

  public:
    template <typename... Ts>
    auto write(std::ostream & os, Ts&&... xs) const -> std::ostream &
    {
      return (os << ... << xs) << reset;
    }

    template <typename... Ts>
    auto write(pair::const_reference x, Ts&&... xs) const -> decltype(auto)
    {
      return write(x.as<std::ostream>(), std::forward<decltype(xs)>(xs)...);
    }

    template <typename... Ts>
    auto print(Ts&&... xs) const -> decltype(auto)
    {
      return write(default_output_port, std::forward<decltype(xs)>(xs)..., '\n');
    }

  public:
    auto null_port() const -> pair::const_reference
    {
      let static port = make<output_file_port>("/dev/null");
      return port;
    }

    auto verbose_port() const -> pair::const_reference
    {
      return is_verbose_mode() ? default_output_port : null_port();
    }

    auto debug_port() const -> pair::const_reference
    {
      return is_debug_mode() ? default_error_port : null_port();
    }
  };
} // namespace kernel
} // namespace meevax

#endif // INCLUDED_MEEVAX_KERNEL_WRITER_HPP
