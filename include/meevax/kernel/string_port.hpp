/*
   Copyright 2018-2023 Tatsuya Yamasaki.

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

#ifndef INCLUDED_MEEVAX_KERNEL_STRING_PORT_HPP
#define INCLUDED_MEEVAX_KERNEL_STRING_PORT_HPP

#include <meevax/kernel/port.hpp>

namespace meevax
{
inline namespace kernel
{
  struct string_port : public textual_input_port, public textual_output_port
  {
    std::stringstream stringstream;

    template <typename... Ts>
    explicit string_port(Ts&&... xs)
      : stringstream { std::forward<decltype(xs)>(xs)... }
    {}

    operator std::istream &() override;

    operator std::ostream &() override;

    explicit operator std::string() override;
  };

  auto operator <<(std::ostream &, string_port const&) -> std::ostream &;
} // namespace kernel
} // namespace meevax

#endif // INCLUDED_MEEVAX_KERNEL_STRING_PORT_HPP

