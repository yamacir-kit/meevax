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

#ifndef INCLUDED_MEEVAX_KERNEL_STANDARD_ERROR_PORT_HPP
#define INCLUDED_MEEVAX_KERNEL_STANDARD_ERROR_PORT_HPP

#include <meevax/kernel/textual_output_port.hpp>

namespace meevax
{
inline namespace kernel
{
  struct standard_error_port : public textual_output_port
  {
    operator std::ostream &() override;
  };

  auto operator <<(std::ostream &, standard_error_port const&) -> std::ostream &;
} // namespace kernel
} // namespace meevax

#endif // INCLUDED_MEEVAX_KERNEL_STANDARD_ERROR_PORT_HPP