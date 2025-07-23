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

#ifndef INCLUDED_MEEVAX_KERNEL_STANDARD_OUTPUT_PORT_HPP
#define INCLUDED_MEEVAX_KERNEL_STANDARD_OUTPUT_PORT_HPP

#include <meevax/kernel/textual_output_port.hpp>

namespace meevax::inline kernel
{
  struct standard_output_port : public textual_output_port
  {
    auto close() -> void override;

    auto is_open() const -> bool override;

    operator std::ostream &() override;
  };

  auto operator <<(std::ostream &, standard_output_port const&) -> std::ostream &;
} // namespace meevax::kernel

#endif // INCLUDED_MEEVAX_KERNEL_STANDARD_OUTPUT_PORT_HPP
