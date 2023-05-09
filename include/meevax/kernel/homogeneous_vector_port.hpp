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

#ifndef INCLUDED_MEEVAX_KERNEL_HOMOGENEOUS_VECTOR_PORT_HPP
#define INCLUDED_MEEVAX_KERNEL_HOMOGENEOUS_VECTOR_PORT_HPP

#include <meevax/kernel/homogeneous_vector.hpp>
#include <meevax/kernel/port.hpp>

namespace meevax
{
inline namespace kernel
{
  template <typename T>
  struct homogeneous_vector_port : public binary_input_port, public binary_output_port
  {
    std::vector<T> vector;

    homogeneous_vector_port() = default;

    explicit homogeneous_vector_port(homogeneous_vector<T> const& v)
      : vector(std::begin(v.values), std::end(v.values))
    {}

    auto flush() -> void override
    {}
  };

  template <typename T>
  auto operator <<(std::ostream & output, homogeneous_vector_port<T> const&) -> std::ostream &
  {
    return output << magenta("#,(") << blue("open-", homogeneous_vector<T>::tag(), "vector") << magenta(")");
  }

  using u8vector_port = homogeneous_vector_port<std::uint8_t>;
} // namespace kernel
} // namespace meevax

#endif // INCLUDED_MEEVAX_KERNEL_HOMOGENEOUS_VECTOR_PORT_HPP
