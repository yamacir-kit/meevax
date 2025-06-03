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

#ifndef INCLUDED_MEEVAX_KERNEL_BINARY_OUTPUT_PORT_HPP
#define INCLUDED_MEEVAX_KERNEL_BINARY_OUTPUT_PORT_HPP

#include <meevax/kernel/binary_port.hpp>
#include <meevax/kernel/homogeneous_vector.hpp>
#include <meevax/kernel/output_port.hpp>

namespace meevax::inline kernel
{
  struct binary_output_port : public virtual binary_port, public virtual output_port
  {
    virtual auto put(std::uint8_t) -> void = 0;

    virtual auto put(u8vector const&) -> void = 0;
  };
} // namespace meevax::kernel

#endif // INCLUDED_MEEVAX_KERNEL_BINARY_OUTPUT_PORT_HPP
