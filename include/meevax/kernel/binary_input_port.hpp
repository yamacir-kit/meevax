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

#ifndef INCLUDED_MEEVAX_KERNEL_BINARY_INPUT_PORT_HPP
#define INCLUDED_MEEVAX_KERNEL_BINARY_INPUT_PORT_HPP

#include <meevax/kernel/binary_port.hpp>
#include <meevax/kernel/input_port.hpp>
#include <meevax/kernel/pair.hpp>

namespace meevax::inline kernel
{
  struct binary_input_port : public virtual binary_port, public virtual input_port
  {
    virtual auto get() -> object = 0;

    virtual auto get(std::size_t) -> object = 0;

    virtual auto get_ready() const -> bool = 0;

    virtual auto peek() -> object = 0;
  };
} // namespace meevax::kernel

#endif // INCLUDED_MEEVAX_KERNEL_BINARY_INPUT_PORT_HPP
